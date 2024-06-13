from functools import cache
from typing import Annotated, Sequence

from fastapi import Depends

from model.ontocompchem import (
    OntocompchemHasValueHasUnit,
    OntocompchemOptimizedGeometry,
    OntocompchemRotationalConstants,
)
from services.rdf_orm import RDFStore
from services.rdf_stores.base import Cls2GetterRDFStore
from services.sparql import get_ontocompchem_endpoint


class OntocompchemRDFStore(Cls2GetterRDFStore):
    def __init__(self, ontocompchem_endpoint: str):
        self.rdf_store = RDFStore(ontocompchem_endpoint)

    @property
    def cls2getter(self):
        return {
            "occ:OptimizedGeometry": self.get_optimized_geometries,
            "occ:RotationalConstants": self.get_rotational_constants_lst,
            **{
                "occ:" + k: self.get_has_value_has_unit_models
                for k in [
                    "Frequencies",
                    "HOMOEnergy",
                    "HOMOMinus1Energy",
                    "HOMOMinus2Energy",
                    "LUMOEnergy",
                    "LUMOPlus1Energy",
                    "LUMOPlus2Energy",
                    "RotationalSymmetryNumber",
                    "SCFEnergy",
                    "TotalEnergy",
                    "TotalEnthalpy",
                    "TotalGibbsFreeEnergy",
                    "ZeroPointEnergy",
                ]
            },
        }

    def get_optimized_geometries(self, iris: Sequence[str]):
        return self.rdf_store(OntocompchemOptimizedGeometry, iris)

    def get_rotational_constants_lst(self, iris: Sequence[str]):
        return self.rdf_store(OntocompchemRotationalConstants, iris)

    def get_has_value_has_unit_models(self, iris: Sequence[str]):
        return self.rdf_store(OntocompchemHasValueHasUnit, iris)


@cache
def get_ontocompchem_rdfStore(
    endpoint: Annotated[str, Depends(get_ontocompchem_endpoint)]
):
    return OntocompchemRDFStore(endpoint)
