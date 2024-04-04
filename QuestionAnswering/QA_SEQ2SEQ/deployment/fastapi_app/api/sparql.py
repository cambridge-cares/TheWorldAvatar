from collections import defaultdict
from functools import cache
import logging
import os
import time
from typing import Annotated, DefaultDict, Dict

from fastapi import APIRouter, Depends, HTTPException
from pydantic import BaseModel
from SPARQLWrapper.SPARQLExceptions import QueryBadFormed

from services.utils.frozendict import FrozenDict
from services.kg_execute.kg_client import KgClient
from services.kg_execute import KgExecutor, KgExecutor, UnexpectedDomainError


class SparqlRequest(BaseModel):
    query: str
    domain: str


class SparqlResponse(BaseModel):
    data: dict
    latency: float


logger = logging.getLogger(__name__)

router = APIRouter()


@cache
def get_kg_configs():
    domain2kgconfig: DefaultDict[str, Dict[str, str]] = defaultdict(dict)
    for key, value in os.environ.items():
        flag = False
        for field in ["endpoint", "user", "password"]:
            prefix = "KG_{field}_".format(field=field.upper())
            if key.startswith(prefix):
                flag = True
                domain = key[len(prefix) :].lower()
                break
        if flag:
            domain2kgconfig[domain][field] = value
    return FrozenDict({k: FrozenDict(v) for k, v in domain2kgconfig.items()})


@cache
def get_kg_executor(
    domain2kgconfig: Annotated[
        FrozenDict[str, FrozenDict[str, str]], Depends(get_kg_configs)
    ]
):
    domain2sparql = {
        domain: KgClient(**kg_config) for domain, kg_config in domain2kgconfig.items()
    }

    return KgExecutor(domain2sparql)


@router.post("")
def query(
    req: SparqlRequest,
    kg_executor: Annotated[KgExecutor, Depends(get_kg_executor)],
):
    logger.info(
        "Received request to KG execution endpoint with the following request body"
    )
    logger.info(req)

    logger.info("Sending query to KG")
    start = time.time()
    try:
        data = kg_executor.query(domain=req.domain, query=req.query)
        end = time.time()
        logger.info("Results from KG received")

        return SparqlResponse(data=data, latency=end - start)
    except (UnexpectedDomainError, QueryBadFormed) as e:
        logger.error(e)
        raise HTTPException(status_code=400, detail=str(e))
