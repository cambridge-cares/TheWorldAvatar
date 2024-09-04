from functools import cache
from typing import Annotated

from fastapi import Depends

from constants.namespace import OM2, ONTODISPERSION
from services.sparql import get_sgDispersion_endpoint
from services.sparql import SparqlClient
from .base import LinkerManager


class ShipLinkerManager(LinkerManager):
    def __init__(self, dispersion_endpoint: str):
        self.sparql_client = SparqlClient(dispersion_endpoint)

    @property
    def cls2linker(self):
        return {"disp:Ship": self.linkShip}

    def linkShip(self, text: str | None, **kwargs):
        if "mmsi" not in kwargs:
            return []

        query = f"""PREFIX disp: <{ONTODISPERSION}>
PREFIX om: <{OM2}>

SELECT ?IRI WHERE {{
  ?IRI a disp:Ship .
  ?IRI disp:hasProperty [ a disp:MMSI ; om:hasValue/om:hasNumericalValue {kwargs["mmsi"]} ]
}}"""
        _, bindings = self.sparql_client.querySelectThenFlatten(query)

        return [binding["IRI"] for binding in bindings]


@cache
def get_ship_linkerManager(
    endpoint: Annotated[str, Depends(get_sgDispersion_endpoint)]
):
    return ShipLinkerManager(endpoint)
