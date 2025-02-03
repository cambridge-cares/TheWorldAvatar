from functools import cache
from typing import Annotated

from fastapi import Depends

from model.kg.ontozoning import OntozoningLandUseType
from services.rdf_ogm import RDFStore
from services.rdf_stores.base import Cls2NodeGetter
from services.sparql import SparqlClient, get_sgPlot_endpoint


class SGPlotRDFStore(Cls2NodeGetter, RDFStore):
    @property
    def cls2getter(self):
        return {
            "ontozoning:LandUseType": self.get_landUseType_many
        }

    def get_landUseType_many(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntozoningLandUseType, iris)

@cache
def get_sgPlot_rdfStore(
    endpoint: Annotated[str, Depends(get_sgPlot_endpoint)]
):
    return SGPlotRDFStore(endpoint)
