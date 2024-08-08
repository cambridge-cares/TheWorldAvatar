from functools import cache
from typing import Annotated

from fastapi import Depends

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

        query = """PREFIX disp: <https://www.theworldavatar.com/kg/ontodispersion/>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>

SELECT ?IRI WHERE {{
  ?IRI a disp:Ship .
  ?IRI disp:hasProperty [ a disp:MMSI ; om:hasValue/om:hasNumericalValue {MMSI} ]
}}""".format(
            MMSI=kwargs["mmsi"]
        )
        _, bindings = self.sparql_client.querySelectThenFlatten(query)

        return [binding["IRI"] for binding in bindings]


@cache
def get_ship_linkerManager(
    endpoint: Annotated[str, Depends(get_sgDispersion_endpoint)]
):
    return ShipLinkerManager(endpoint)
