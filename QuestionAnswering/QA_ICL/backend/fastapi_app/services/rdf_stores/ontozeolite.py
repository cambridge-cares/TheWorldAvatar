from functools import cache
from typing import Annotated, Sequence

from fastapi import Depends
from model.ontozeolite import OntozeoliteZeoliteFramework, OntozeoliteZeoliticMaterial
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
        }

    def get_zeolite_frameworks(self, iris: list[str] | tuple[str]):
        return self.rdf_store.getMany(OntozeoliteZeoliteFramework, iris)

    def get_zeolitic_materials(self, iris: list[str] | tuple[str]):
        return self.rdf_store.getMany(OntozeoliteZeoliticMaterial, iris)


@cache
def get_ontozeolite_rdfStore(
    endpoint: Annotated[str, Depends(get_ontozeolite_endpoint)]
):
    return OntozeoliteRDFStore(endpoint)
