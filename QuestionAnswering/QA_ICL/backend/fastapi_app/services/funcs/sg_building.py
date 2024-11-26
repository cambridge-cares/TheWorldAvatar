from collections import defaultdict
from functools import cache
import logging
from typing import Annotated

from fastapi import Depends
from rdflib import RDF
import shapely
import shapely.wkt

from constants.namespace import CITYGML_BUILDING, CITYGML_CITYOBJECTGROUP, ONT_GEOSPARQL, ONTOBIM
from model.structured_answer import WKTGeometryData
from services.funcs.base import Name2Func
from services.sparql import SparqlClient, get_sgOntop_endpoint
from services.wkt import WKTTextSRS


logger = logging.getLogger(__name__)


class SGBuildingFuncExecutor(Name2Func):
    def __init__(self, ontop_endpoint: SparqlClient):
        self.ontop_client = SparqlClient(ontop_endpoint)

    def get_name2func(
        self,
    ):
        return {"visualise_facility_building_footprint": self.visualise_facility_building_footprint}

    def visualise_facility_building_footprint(self, facility: list[str], **kwargs):
        query = f"""PREFIX rdf: <{RDF}>
PREFIX bldg: <{CITYGML_BUILDING}>
PREFIX grp: <{CITYGML_CITYOBJECTGROUP}>
PREFIX geo: <{ONT_GEOSPARQL}>
PREFIX ontobim: <{ONTOBIM}>

SELECT * WHERE {{
    FILTER ( ?Facility IN ( {", ".join("<{iri}>".format(iri=iri) for iri in facility)} ) )
    ?Building rdf:type bldg:Building .
    ?Building ontobim:hasFacility ?Facility .
    ?Building bldg:lod0FootPrint/^grp:parent/geo:asWKT ?FootPrintWKT .
}}"""

        _, bindings = self.ontop_client.querySelectThenFlatten(query)

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
                title=None,  # TODO: retrieve facility label
                literal=iri2merged[iri],
            )
            for iri in facility
            if iri in iri2merged
        ]

        return data


@cache
def get_sgBuilding_funcExec(
    ontop_endpoint: Annotated[str, Depends(get_sgOntop_endpoint)],
):
    return SGBuildingFuncExecutor(ontop_endpoint=ontop_endpoint)
