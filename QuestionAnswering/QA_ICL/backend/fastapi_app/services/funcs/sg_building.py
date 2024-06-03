from collections import defaultdict
from functools import cache
import logging
from typing import Annotated

from fastapi import Depends
import shapely
import shapely.wkt

from services.entity_store import EntityStore, get_entity_store
from services.funcs.base import Name2Func
from services.kg import KgClient, get_sg_ontopClient
from model.qa import WKTGeometryData
from services.wkt import WKTTextSRS
from utils.rdf import flatten_sparql_select_response


logger = logging.getLogger(__name__)


class SGBuildingFuncExecutor(Name2Func):
    def __init__(self, entity_store: EntityStore, ontop_client: KgClient):
        self.entity_store = entity_store
        self.ontop_client = ontop_client

    def get_name2func(
        self,
    ):
        return {"visualise_building_footprint": self.visualise_building_footprint}

    def visualise_building_footprint(self, cls: str, text: str):
        logger.info("Perform entity linking for `text`: {text}".format(text=text))
        iris = self.entity_store.link(cls=cls, text=text)
        logger.info("Linked IRIs: " + str(iris))

        if cls == "Facility":
            logger.info("Retrieve WKT for " + str(iris))

            query = """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX bldg: <http://www.opengis.net/citygml/building/2.0/>
PREFIX grp: <http://www.opengis.net/citygml/cityobjectgroup/2.0/>
PREFIX geo: <http://www.opengis.net/ont/geosparql#>
PREFIX ontobim: <https://www.theworldavatar.com/kg/ontobim/>

SELECT * WHERE {{
    FILTER ( ?Facility IN ( {filter_values} ) )
    ?Building rdf:type bldg:Building .
    ?Building ontobim:hasFacility ?Facility .
    ?Building bldg:lod0FootPrint/^grp:parent/geo:asWKT ?FootPrintWKT .
}}""".format(
                filter_values=", ".join("<{iri}>".format(iri=iri) for iri in iris)
            )

            res = self.ontop_client.querySelect(query)
            _, bindings = flatten_sparql_select_response(res)

            iri2wkts = defaultdict(list)
            for binding in bindings:
                iri2wkts[binding["Facility"]].append(binding["FootPrintWKT"])

            iri2wkts = {
                iri: [WKTTextSRS.from_literal(wkt) for wkt in wkts]
                for iri, wkts in iri2wkts.items()
            }
            iri2merged = {
                iri: shapely.union_all(
                    [shapely.wkt.loads(wkt.wkt_text) for wkt in wkts]
                ).wkt
                for iri, wkts in iri2wkts.items()
            }

            data = [
                WKTGeometryData(
                    title=self.entity_store.lookup_label(iri),
                    literal=iri2merged[iri],
                )
                for iri in iris
                if iri in iri2merged
            ]

        else:
            data = []

        return data


@cache
def get_sgBuilding_funcExec(
    entity_store: Annotated[EntityStore, Depends(get_entity_store)],
    ontop_client: Annotated[KgClient, Depends(get_sg_ontopClient)],
):
    return SGBuildingFuncExecutor(entity_store=entity_store, ontop_client=ontop_client)
