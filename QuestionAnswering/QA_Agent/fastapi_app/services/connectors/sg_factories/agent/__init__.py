from functools import cache
from typing import Annotated, List, Literal, Optional, Tuple, Union

from fastapi import Depends

from model.constraint import ExtremeValueConstraint
from model.aggregate import AggregateOperator
from model.qa import QAData
from services.core.kg import KgClient
from services.core.labels_store import LabelsStore
from ..model import FACTORYATTR2UNIT, FactoryAttrKey, FactoryConstraints, Industry
from ..kg import get_sg_factories_ontop_client
from .labels_store import get_sg_factories_labels_store
from .make_sparql import SGFactoriesSPARQLMaker


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

    def lookup_factory_attribute(self, plant_name: str, attr_key: FactoryAttrKey):
        iris = self.labels_store.link_entity(plant_name)

        query = self.sparql_maker.lookup_factory_attribute(iris=iris, attr_key=attr_key)
        res = self.ontop_client.query(query)

        return QAData(
            vars=res["head"]["vars"],
            bindings=[
                {k: v["value"] for k, v in binding.items()}
                for binding in res["results"]["bindings"]
            ],
        )

    def _add_units(self, vars: List[str], bindings: List[dict]):
        i = 0
        while i < len(vars):
            found = False
            for key in [
                FactoryAttrKey.DESIGN_CAPACITY,
                FactoryAttrKey.GENERATED_HEAT,
                FactoryAttrKey.SPECIFIC_ENERGY_CONSUMPTION,
            ]:
                if vars[i].startswith(key.value):
                    found = True
                    unit_var = key.value + "Unit"
                    vars.insert(i + 1, unit_var)
                    for binding in bindings:
                        binding[unit_var] = FACTORYATTR2UNIT[key]
                    break
            i += 1
            if found:
                i += 1

    def find_factories(
        self,
        constraints: Optional[FactoryConstraints] = None,
        limit: Optional[int] = None,
    ):
        query = self.sparql_maker.find_factories(constraints=constraints, limit=limit)
        res = self.ontop_client.query(query)

        vars = res["head"]["vars"]
        bindings = [
            {k: v["value"] for k, v in binding.items()}
            for binding in res["results"]["bindings"]
        ]
        self._add_units(vars=vars, bindings=bindings)

        return QAData(vars=vars, bindings=bindings)

    def count_factories(
        self,
        industry: Optional[Industry] = None,
        groupby_industry: bool = False,
    ):
        query = self.sparql_maker.count_factories(
            industry=industry, groupby_industry=groupby_industry
        )
        print(query)
        res = self.ontop_client.query(query)

        vars: List[str] = res["head"]["vars"]
        bindings = [
            {k: v["value"] for k, v in binding.items()}
            for binding in res["results"]["bindings"]
        ]

        return QAData(
            vars=vars,
            bindings=bindings,
        )

    def compute_aggregate_factory_attribute(
        self,
        attr_agg: Tuple[FactoryAttrKey, AggregateOperator],
        industry: Optional[Industry] = None,
        groupby_industry: bool = False,
    ):
        query = self.sparql_maker.compute_aggregate_factory_attribute(
            attr_agg=attr_agg, industry=industry, groupby_industry=groupby_industry
        )
        print(query)
        res = self.ontop_client.query(query)

        vars = res["head"]["vars"]
        bindings = [
            {k: v["value"] for k, v in binding.items()}
            for binding in res["results"]["bindings"]
        ]
        self._add_units(vars=vars, bindings=bindings)

        return QAData(
            vars=vars,
            bindings=bindings,
        )


def get_sg_factories_agent(
    ontop_client: Annotated[KgClient, Depends(get_sg_factories_ontop_client)],
    labels_store: Annotated[LabelsStore, Depends(get_sg_factories_labels_store)],
):
    return SGFactoriesAgent(
        ontop_client=ontop_client,
        labels_store=labels_store,
    )
