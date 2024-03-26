from functools import lru_cache
from typing import Annotated, Callable, Dict, List, Optional, Tuple

from fastapi import Depends

from model.constraint import ExtremeValueConstraint
from model.aggregate import AggregateOperator
from model.qa import QAData
from services.utils.rdf import add_label_to_sparql_resposne, flatten_sparql_response
from services.core.kg import KgClient
from services.core.label_store import LabelStore
from services.connectors.sg import get_sg_ontopClient
from ..model import FactoryAttrKey, FactoryNumAttrKey, Industry
from .label_store import get_sgFactories_labelStore
from .make_sparql import SGFactoriesSPARQLMaker, get_sgFactories_sparqlmaker


class SGFactoriesAgent:

    def __init__(
        self,
        ontop_client: KgClient,
        label_store: LabelStore,
        sparql_maker: SGFactoriesSPARQLMaker,
    ):
        self.ontop_client = ontop_client
        self.label_store = label_store
        self.sparql_maker = sparql_maker

    @lru_cache(maxsize=128)
    def _lookup_company_name(self, factory_iri: str):
        query = """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany#>

SELECT ?CompanyLabel WHERE {{
VALUES ?Factory {{ <{iri}> }}
?Company ontocompany:isOwnerOf ?Factory .
?Company rdfs:label ?CompanyLabel .
}}""".format(
            iri=factory_iri
        )
        res = self.ontop_client.query(query)
        bindings = res["results"]["bindings"]
        if bindings:
            return bindings[0]["CompanyLabel"]["value"]
        return None

    def _add_company_label(self, vars: List[str], bindings: List[dict]):
        try:
            iri_idx = vars.index("IRI")
        except ValueError:
            raise ValueError("IRI must be present, found: " + str(vars))

        vars.insert(iri_idx + 1, "Company")
        for binding in bindings:
            binding["Company"] = self._lookup_company_name(binding["IRI"])

    def lookup_factory_attribute(self, name: str, attr_key: FactoryAttrKey):
        iris = self.label_store.link_entity(name)

        query = self.sparql_maker.lookup_factory_attribute(iris=iris, attr_key=attr_key)
        res = self.ontop_client.query(query)

        vars, bindings = flatten_sparql_response(res)
        add_label_to_sparql_resposne(
            self.label_store, vars, bindings, label_header="FactoryName"
        )
        self._add_company_label(vars, bindings)

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
            _vars, _bindings = flatten_sparql_response(res)

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
        add_label_to_sparql_resposne(
            self.label_store, vars, bindings, label_header="FactoryName"
        )
        self._add_company_label(vars, bindings)

        return QAData(vars=vars, bindings=bindings)

    def count_factories(
        self,
        industry: Optional[Industry] = None,
    ):
        vars, bindings = self._aggregate_across_industries(
            industry=industry, query_maker=self.sparql_maker.count_factories
        )

        if industry is None and all(header in vars for header in ["FactoryCount", "CompanyCount"]):
            bindings.append(
                {
                    "Industry": "SUM",
                    "FactoryCount": str(sum(int(binding["FactoryCount"]) for binding in bindings)),
                    "CompanyCount": str(sum(int(binding["CompanyCount"]) for binding in bindings))
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
    ontop_client: Annotated[KgClient, Depends(get_sg_ontopClient)],
    labels_store: Annotated[LabelStore, Depends(get_sgFactories_labelStore)],
    sparql_maker: Annotated[
        SGFactoriesSPARQLMaker, Depends(get_sgFactories_sparqlmaker)
    ],
):
    return SGFactoriesAgent(
        ontop_client=ontop_client, label_store=labels_store, sparql_maker=sparql_maker
    )
