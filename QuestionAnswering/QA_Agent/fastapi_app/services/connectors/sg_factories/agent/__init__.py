from typing import Annotated, Callable, Dict, List, Optional, Tuple

from fastapi import Depends

from model.constraint import ExtremeValueConstraint
from model.aggregate import AggregateOperator
from model.qa import QAData
from services.core.kg import KgClient
from services.core.labels_store import LabelsStore
from ..model import FactoryAttrKey, FactoryNumAttrKey, Industry
from ..kg import get_sgFactories_ontopClient
from .labels_store import get_sgFactories_labelsStore
from .make_sparql import SGFactoriesSPARQLMaker, get_sgFactories_sparqlmaker


class SGFactoriesAgent:

    def __init__(
        self,
        ontop_client: KgClient,
        labels_store: LabelsStore,
        sparql_maker: SGFactoriesSPARQLMaker,
    ):
        self.ontop_client = ontop_client
        self.labels_store = labels_store
        self.sparql_maker = sparql_maker

    def _flatten_sparql_response(self, res: dict):
        vars: List[str] = res["head"]["vars"]
        bindings = [
            {k: v["value"] for k, v in binding.items()}
            for binding in res["results"]["bindings"]
        ]
        return vars, bindings

    def _add_label(self, vars: List[str], bindings: List[dict]):
        vars.insert(vars.index("IRI") + 1, "label")
        for binding in bindings:
            labels = self.labels_store.lookup_labels(binding["IRI"])
            if labels:
                binding["label"] = labels[0]

    def lookup_factory_attribute(self, plant_name: str, attr_key: FactoryAttrKey):
        iris = self.labels_store.link_entity(plant_name)

        query = self.sparql_maker.lookup_factory_attribute(iris=iris, attr_key=attr_key)
        res = self.ontop_client.query(query)

        vars, bindings = self._flatten_sparql_response(res)
        self._add_label(vars, bindings)

        return QAData(vars=vars, bindings=bindings)

    def _aggregate_across_industries(
        self, industry: Optional[Industry], query_maker: Callable[[Industry], str]
    ):
        industries = [industry] if industry else [i for i in Industry]

        vars = []
        vars_set = set()
        bindings = []

        for _industry in industries:
            query = query_maker(_industry)

            res = self.ontop_client.query(query)
            _vars, _bindings = self._flatten_sparql_response(res)

            _vars.insert(0, "Industry")
            for binding in _bindings:
                binding["Industry"] = _industry.value

            for var in _vars:
                if var not in vars_set:
                    vars.append(var)
                    vars_set.add(var)

            bindings.extend(_bindings)

        return vars, bindings

    def find_factories(
        self,
        industry: Optional[Industry] = None,
        numattr_constraints: Dict[FactoryNumAttrKey, ExtremeValueConstraint] = dict(),
        limit: Optional[int] = None,
    ):
        vars, bindings = self._aggregate_across_industries(
            industry=industry,
            query_maker=lambda _industry: self.sparql_maker.find_factories(
                industry=_industry, numattr_constraints=numattr_constraints, limit=limit
            ),
        )
        self._add_label(vars, bindings)

        return QAData(vars=vars, bindings=bindings)

    def count_factories(
        self,
        industry: Optional[Industry] = None,
    ):
        vars, bindings = self._aggregate_across_industries(
            industry=industry, query_maker=self.sparql_maker.count_factories
        )

        if industry is None:
            bindings.append(
                {
                    "Industry": "SUM",
                    "Count": str(sum(int(binding["Count"]) for binding in bindings)),
                }
            )

        return QAData(
            vars=vars,
            bindings=bindings,
        )

    def compute_aggregate_factory_attribute(
        self,
        attr_agg: Tuple[FactoryNumAttrKey, AggregateOperator],
        industry: Optional[Industry] = None,
    ):
        vars, bindings = self._aggregate_across_industries(
            industry=industry,
            query_maker=lambda _industry: self.sparql_maker.compute_aggregate_factory_attribute(
                industry=_industry, attr_agg=attr_agg
            ),
        )

        return QAData(
            vars=vars,
            bindings=bindings,
        )


def get_sgFactories_agent(
    ontop_client: Annotated[KgClient, Depends(get_sgFactories_ontopClient)],
    labels_store: Annotated[LabelsStore, Depends(get_sgFactories_labelsStore)],
    sparql_maker: Annotated[
        SGFactoriesSPARQLMaker, Depends(get_sgFactories_sparqlmaker)
    ],
):
    return SGFactoriesAgent(
        ontop_client=ontop_client, labels_store=labels_store, sparql_maker=sparql_maker
    )
