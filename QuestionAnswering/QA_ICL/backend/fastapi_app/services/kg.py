from functools import cache
import logging
import os
from typing import Dict, List, Literal, Optional


from SPARQLWrapper import SPARQLWrapper, POST, JSON
from pydantic import BaseModel, ConfigDict, TypeAdapter

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


class KgClient:
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
        logger.info("Executing SPARQL query:\n" + query)
        self.sparql.setQuery(query)
        res = self.sparql.queryAndConvert()
        logger.info("Execution done")
        return self.res_adapter.validate_python(res)


@cache
def get_ontospecies_bgClient():
    return KgClient(os.environ["KG_ENDPOINT_ONTOSPECIES"])


@cache
def get_ontokin_bgClient():
    return KgClient(os.environ["KG_ENDPOINT_ONTOKIN"])


@cache
def get_ontocompchem_bgClient():
    return KgClient(os.environ["KG_ENDPOINT_ONTOCOMPCHEM"])


@cache
def get_ontozeolite_bgClient():
    return KgClient(os.environ["KG_ENDPOINT_ONTOZEOLITE"])


@cache
def get_sg_ontopClient():
    return KgClient(os.environ["KG_ENDPOINT_SG_ONTOP"])


@cache
def get_sgPlot_bgClient():
    return KgClient(os.environ["KG_ENDPOINT_SG_PLOT"])


@cache
def get_sgCompany_bgClient():
    return KgClient(os.environ["KG_ENDPOINT_SG_COMPANY"])


@cache
def get_sgDispersion_bgClient():
    return KgClient(os.environ["KG_ENDPOINT_SG_DISPERSION"])


@cache
def get_sgCarpark_bgClient():
    return KgClient(os.environ["KG_ENDPOINT_SG_CARPARK"])
