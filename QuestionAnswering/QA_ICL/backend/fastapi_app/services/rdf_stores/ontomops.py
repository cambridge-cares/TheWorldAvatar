from functools import cache
from typing import Annotated

from fastapi import Depends
from model.kg.ontomops import OntomopsAM, OntomopsCBU, OntomopsGBU, OntomopsMOP
from services.rdf_orm import RDFStore
from services.rdf_stores.base import Cls2NodeGetter
from services.sparql import get_ontomops_endpoint


class OntomopsRDFStore(Cls2NodeGetter, RDFStore):
    @property
    def cls2getter(self):
        return {
            "mops:MetalOrganicPolyhedron": self.get_MOPs,
            "mops:ChemicalBuildingUnit": self.get_CBUs,
            "mops:AssemblyModel": self.get_AMs,
            "mops:GenericBuildingUnit": self.get_GBUs,
        }

    def get_MOPs(self, iris: list[str] | tuple[str]):
        return self.getMany(OntomopsMOP, iris)

    def get_CBUs(self, iris: list[str] | tuple[str]):
        return self.getMany(OntomopsCBU, iris)

    def get_AMs(self, iris: list[str] | tuple[str]):
        return self.getMany(OntomopsAM, iris)

    def get_GBUs(self, iris: list[str] | tuple[str]):
        return self.getMany(OntomopsGBU, iris)


@cache
def get_ontomops_rdfStore(endpoint: Annotated[str, Depends(get_ontomops_endpoint)]):
    return OntomopsRDFStore(endpoint)
