from functools import cache
from typing import Annotated, List, Optional

from fastapi import Depends

from model.qa import QAData
from services.core.kg import KgClient
from services.kg import get_sg_ontopClient
from services.utils.rdf import extract_name, flatten_sparql_response
from .make_sparql import SGBuildingsSPARQLMaker
from ..model import PropertyUsage


class SGBuildingsAgent:
    def __init__(self, sparql_maker: SGBuildingsSPARQLMaker, ontop_client: KgClient):
        self.sparql_maker = sparql_maker
        self.ontop_client = ontop_client

    def _sanitise_property_usage(self, bindings: List[dict]):
        for binding in bindings:
            if "PropertyUsage" in binding:
                binding["PropertyUsage"] = extract_name(binding["PropertyUsage"])

    def count_buildings(
        self, usage: Optional[PropertyUsage] = None, groupby_usage: bool = False
    ):
        query = self.sparql_maker.count_buildings(usage, groupby_usage)

        res = self.ontop_client.query(query)
        vars, bindings = flatten_sparql_response(res)
        self._sanitise_property_usage(bindings)

        return QAData(vars=vars, bindings=bindings)


@cache
def get_sgBuildings_agent(
    ontop_client: Annotated[KgClient, Depends(get_sg_ontopClient)]
):
    return SGBuildingsAgent(
        sparql_maker=SGBuildingsSPARQLMaker(), ontop_client=ontop_client
    )
