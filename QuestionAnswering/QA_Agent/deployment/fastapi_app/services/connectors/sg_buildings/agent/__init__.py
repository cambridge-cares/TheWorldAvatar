from functools import cache
from typing import Annotated, List, Optional

from fastapi import Depends

from model.qa import QAData
from core.kg import KgClient
from services.link_entity import EntityLinker, get_entity_linker
from services.kg import get_sg_ontopClient
from services.utils.rdf import extract_name, flatten_sparql_response
from .make_sparql import SGBuildingsSPARQLMaker
from ..model import BuildingAttrKey, PropertyUsage


class SGBuildingsAgent:
    def __init__(
        self,
        ontop_client: KgClient,
        entity_linker: EntityLinker,
    ):
        self.sparql_maker = SGBuildingsSPARQLMaker()
        self.ontop_client = ontop_client
        self.entity_linker = entity_linker

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

    def lookup_building_attribute(self, facility_name: str, attr_key: BuildingAttrKey):
        facility_iris = self.entity_linker.link(facility_name, "Facility", k=1)
        query = self.sparql_maker.lookup_building_attribute(facility_iris, attr_key)

        res = self.ontop_client.query(query)
        vars, bindings = flatten_sparql_response(res)

        return QAData(vars=vars, bindings=bindings)


@cache
def get_sgBuildings_agent(
    ontop_client: Annotated[KgClient, Depends(get_sg_ontopClient)],
    entity_linker: Annotated[EntityLinker, Depends(get_entity_linker)],
):
    return SGBuildingsAgent(ontop_client=ontop_client, entity_linker=entity_linker)
