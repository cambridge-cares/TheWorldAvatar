from functools import cache
from typing import Annotated, Literal


from SPARQLWrapper import SPARQLWrapper, POST, JSON
from fastapi import Depends
from pydantic import BaseModel, ConfigDict, TypeAdapter

from config import AppSettings, get_app_settings


class SparqlSelectResponseHead(BaseModel):
    model_config = ConfigDict(frozen=True)

    vars: list[str]


class SparqlSelectResponseBindingValue(BaseModel):
    model_config = ConfigDict(frozen=True)

    datatype: str | None = None
    type: Literal["uri", "literal"]
    value: str


class SparqlSelectResponseResults(BaseModel):
    model_config = ConfigDict(frozen=True)

    bindings: list[dict[str, SparqlSelectResponseBindingValue]]


class SparqlSelectResponse(BaseModel):
    model_config = ConfigDict(frozen=True)

    head: SparqlSelectResponseHead
    results: SparqlSelectResponseResults


class SparqlClient:
    def __init__(
        self, endpoint: str, user: str | None = None, password: str | None = None
    ):
        sparql = SPARQLWrapper(endpoint)
        sparql.setReturnFormat(JSON)
        if user is not None and password is not None:
            sparql.setCredentials(user=user, passwd=password)
        sparql.setMethod(POST)
        self.sparql = sparql
        self.res_adapter = TypeAdapter(SparqlSelectResponse)

    def querySelect(self, query: str):
        self.sparql.setQuery(query)
        res = self.sparql.queryAndConvert()
        return self.res_adapter.validate_python(res)

    def flatten_select_response(self, res: SparqlSelectResponse):
        # TODO: cast value into appropriate type e.g. str, float, URIRef
        vars = list(res.head.vars)
        bindings = [
            {k: v.value for k, v in binding.items()} for binding in res.results.bindings
        ]
        return vars, bindings

    def querySelectThenFlatten(self, query: str):
        res = self.querySelect(query)
        return self.flatten_select_response(res)

def get_pubchem_agent_url(
    settings: Annotated[AppSettings, Depends(get_app_settings)]
):
    return settings.agents.pubchem_agent

def get_ontospecies_endpoint(
    settings: Annotated[AppSettings, Depends(get_app_settings)]
):
    return settings.chemistry_endpoints.ontospecies

# TODO: ONTOSPECIES_V3 should be remove after merging
def get_ontospecies_endpoint_v3(
    settings: Annotated[AppSettings, Depends(get_app_settings)]
):
    return settings.chemistry_endpoints.ontospecies_v3


def get_ontokin_endpoint(settings: Annotated[AppSettings, Depends(get_app_settings)]):
    return settings.chemistry_endpoints.ontokin


def get_ontocompchem_endpoint(
    settings: Annotated[AppSettings, Depends(get_app_settings)]
):
    return settings.chemistry_endpoints.ontocompchem


def get_ontozeolite_endpoint(
    settings: Annotated[AppSettings, Depends(get_app_settings)]
):
    return settings.chemistry_endpoints.ontozeolite


def get_ontomops_endpoint(settings: Annotated[AppSettings, Depends(get_app_settings)]):
    return settings.chemistry_endpoints.ontomops


def get_sgOntop_endpoint(settings: Annotated[AppSettings, Depends(get_app_settings)]):
    return settings.singapore_endpoints.ontop


def get_sgPlot_endpoint(settings: Annotated[AppSettings, Depends(get_app_settings)]):
    return settings.singapore_endpoints.plot


def get_sgCompany_endpoint(settings: Annotated[AppSettings, Depends(get_app_settings)]):
    return settings.singapore_endpoints.company


def get_sgDispersion_endpoint(
    settings: Annotated[AppSettings, Depends(get_app_settings)]
):
    return settings.singapore_endpoints.dispersion


def get_sgCarpark_endpoint(settings: Annotated[AppSettings, Depends(get_app_settings)]):
    return settings.singapore_endpoints.carpark
