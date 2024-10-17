from functools import cache
from typing import Annotated

from fastapi import Depends
from model.kg.ontocompany import OntocompanyIndustrialFacility, OntocompanyCompany
from services.rdf_ogm import RDFStore
from services.rdf_stores.base import Cls2NodeGetter
from services.sparql import SparqlClient, get_sgOntop_endpoint


class SGOntopRDFStore(Cls2NodeGetter, RDFStore):
    @property
    def cls2getter(self):
        return {
            "ontocompany:IndustrialFacility": self.get_industrialFacility_many,
            "ontocompany:Company": self.get_company_many
        }
    
    def get_industrialFacility_many(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntocompanyIndustrialFacility, iris)
    
    def get_company_many(
        self,
        iris: list[str] | tuple[str],
        sparql_client: str | SparqlClient | None = None,
    ):
        return self.get_many(OntocompanyCompany, iris)
    
@cache
def get_sgOntop_rdfStore(
    endpoint: Annotated[str, Depends(get_sgOntop_endpoint)]
):
    return SGOntopRDFStore(endpoint)