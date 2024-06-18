from functools import cache
from typing import Annotated

from fastapi import Depends
from model.kg.ontozeolite import (
    OntocrystalAtomicStructure,
    OntocrystalCoordinateTransformation,
    OntocrystalMeasureMatrix,
    OntocrystalMeasureVector,
    OntocrystalQuantity,
    OntocrystalTiledStructure,
    OntocrystalUnitCell,
    OntozeoliteZeoliteFramework,
    OntozeoliteZeoliticMaterial,
)
from services.rdf_orm import RDFStore
from services.rdf_stores.base import Cls2GetterRDFStore
from services.sparql import get_ontozeolite_endpoint


class OntozeoliteRDFStore(Cls2GetterRDFStore):
    def __init__(self, ontozeolite_endpoint: str):
        self.rdf_store = RDFStore(ontozeolite_endpoint)

    @property
    def cls2getter(self):
        return {
            "zeo:ZeoliteFramework": self.get_zeolite_frameworks,
            "zeo:ZeoliticMaterial": self.get_zeolitic_materials,
            "ocr:Quantity": self.get_quantities,
            "ocr:MeasureVector": self.get_vectors,
            "ocr:MeasureMatrix": self.get_matrices,
            "ocr:AtomicStructure": self.get_atomic_structures,
            "ocr:CoordinateTransformation": self.get_coordinate_transforms,
            "ocr:UnitCell": self.get_unit_cells,
            "ocr:TiledStructure": self.get_tiled_structures,
        }

    def get_zeolite_frameworks(self, iris: list[str] | tuple[str]):
        return self.rdf_store.getMany(OntozeoliteZeoliteFramework, iris)

    def get_zeolitic_materials(self, iris: list[str] | tuple[str]):
        return self.rdf_store.getMany(OntozeoliteZeoliticMaterial, iris)

    def get_quantities(self, iris: list[str] | tuple[str]):
        return self.rdf_store.getMany(OntocrystalQuantity, iris)

    def get_vectors(self, iris: list[str] | tuple[str]):
        return self.rdf_store.getMany(OntocrystalMeasureVector, iris)

    def get_matrices(self, iris: list[str] | tuple[str]):
        return self.rdf_store.getMany(OntocrystalMeasureMatrix, iris)

    def get_atomic_structures(self, iris: list[str] | tuple[str]):
        return self.rdf_store.getMany(OntocrystalAtomicStructure, iris)

    def get_coordinate_transforms(self, iris: list[str] | tuple[str]):
        return self.rdf_store.getMany(OntocrystalCoordinateTransformation, iris)

    def get_unit_cells(self, iris: list[str] | tuple[str]):
        return self.rdf_store.getMany(OntocrystalUnitCell, iris)

    def get_tiled_structures(self, iris: list[str] | tuple[str]):
        return self.rdf_store.getMany(OntocrystalTiledStructure, iris)


@cache
def get_ontozeolite_rdfStore(
    endpoint: Annotated[str, Depends(get_ontozeolite_endpoint)]
):
    return OntozeoliteRDFStore(endpoint)
