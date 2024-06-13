from functools import cache
import logging
from typing import Annotated, Dict, List, Literal, Optional


from SPARQLWrapper import SPARQLWrapper, POST, JSON
from fastapi import Depends
from pydantic import BaseModel, ConfigDict, TypeAdapter

from config import AppSettings, get_app_settings

logger = logging.getLogger(__name__)


class SparqlSelectResponseHead(BaseModel):
    model_config = ConfigDict(froze=True)

    vars: List[str]


class SparqlSelectResponseBindingValue(BaseModel):
    model_config = ConfigDict(froze=True)

    datatype: Optional[str] = None
    type: Literal["uri", "literal"]
    value: str


class SparqlSelectResponseResults(BaseModel):
    model_config = ConfigDict(froze=True)

    bindings: List[Dict[str, SparqlSelectResponseBindingValue]]


class SparqlSelectResponse(BaseModel):
    model_config = ConfigDict(froze=True)

    head: SparqlSelectResponseHead
    results: SparqlSelectResponseResults


class SparqlClient:
    def __init__(
        self, endpoint: str, user: Optional[str] = None, password: Optional[str] = None
    ):
        sparql = SPARQLWrapper(endpoint)
        sparql.setReturnFormat(JSON)
        if user is not None and password is not None:
            sparql.setCredentials(user=user, passwd=password)
        sparql.setMethod(POST)
        self.sparql = sparql
        self.res_adapter = TypeAdapter(SparqlSelectResponse)

    def querySelect(self, query: str):
        logger.info(
            "Executing the following SPARQL query at {endpoint}:\n{query}".format(
                endpoint=self.sparql.endpoint, query=query
            )
        )
        self.sparql.setQuery(query)
        res = self.sparql.queryAndConvert()
        logger.info("Execution done")
        return self.res_adapter.validate_python(res)

    def flatten_select_response(self, res: SparqlSelectResponse):
        vars = list(res.head.vars)
        bindings = [
            {k: v.value for k, v in binding.items()} for binding in res.results.bindings
        ]
        return vars, bindings

    def querySelectThenFlatten(self, query: str):
        res = self.querySelect(query)
        return self.flatten_select_response(res)


@cache
def get_ontospecies_endpoint(
    settings: Annotated[AppSettings, Depends(get_app_settings)]
):
    return settings.chemistry_endpoints.ontospecies


@cache
def get_ontokin_endpoint(settings: Annotated[AppSettings, Depends(get_app_settings)]):
    return settings.chemistry_endpoints.ontokin


@cache
def get_ontocompchem_endpoint(
    settings: Annotated[AppSettings, Depends(get_app_settings)]
):
    return settings.chemistry_endpoints.ontocompchem


@cache
def get_ontozeolite_endpoint(
    settings: Annotated[AppSettings, Depends(get_app_settings)]
):
    return settings.chemistry_endpoints.ontozeolite


@cache
def get_sgOntop_endpoint(settings: Annotated[AppSettings, Depends(get_app_settings)]):
    return settings.singapore_endpoints.ontop


@cache
def get_sgPlot_endpoint(settings: Annotated[AppSettings, Depends(get_app_settings)]):
    return settings.singapore_endpoints.plot


@cache
def get_sgCompany_endpoint(settings: Annotated[AppSettings, Depends(get_app_settings)]):
    return settings.singapore_endpoints.company


@cache
def get_sgDispersion_endpoint(
    settings: Annotated[AppSettings, Depends(get_app_settings)]
):
    return settings.singapore_endpoints.dispersion


@cache
def get_sgCarpark_endpoint(settings: Annotated[AppSettings, Depends(get_app_settings)]):
    return settings.singapore_endpoints.carpark
