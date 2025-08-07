from functools import cache
from typing import Annotated

from fastapi import Depends
from model.kg.ontomops import OmMeasure, OntomopsAM, OntomopsCBU, OntomopsGBU, OntomopsMOP, OntomopsGBUType, OntomopCalculatedProperty, OntomopCalculationParameter, OntomopSoftware
from services.rdf_ogm import RDFStore
from services.rdf_stores.base import Cls2NodeGetter
from services.sparql import SparqlClient, get_ontomops_endpoint

import logging

logger = logging.getLogger(__name__)

class OntomopsRDFStore(Cls2NodeGetter, RDFStore):
    @property
    def cls2getter(self):
        return {
            "mops:MetalOrganicPolyhedron": self.get_MOPs,
            "mops:ChemicalBuildingUnit": self.get_CBUs,
            "mops:AssemblyModel": self.get_AMs,
            "mops:GenericBuildingUnit": self.get_GBUs,
            "mops:GenericBuildingUnitType": self.get_GBUTypes,
            "om:Measure": self.get_Measures,
            "mops:CalculatedProperty": self.get_CalculatedProperty,
            "mops:CalculationParameter": self.get_CalculationParameter,
            "mops:Software": self.get_Software,
        }

    def get_MOPs(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntomopsMOP, iris)

    def get_CBUs(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntomopsCBU, iris)

    def get_AMs(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntomopsAM, iris)

    def get_GBUs(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntomopsGBU, iris)
    
    def get_GBUTypes(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntomopsGBUType, iris)
    
    def get_Measures(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OmMeasure, iris)
    
    def get_CalculatedProperty(
        self, 
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntomopCalculatedProperty, iris)
    
    def get_CalculationParameter(
        self, 
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntomopCalculationParameter, iris)
        
    def get_Software(
        self, 
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntomopSoftware, iris)


@cache
def get_ontomops_rdfStore(endpoint: Annotated[str, Depends(get_ontomops_endpoint)]):
    return OntomopsRDFStore(endpoint)
