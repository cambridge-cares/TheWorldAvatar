from collections import defaultdict
from dataclasses import fields
from functools import cache
import logging
import os
import time
from typing import Annotated, Callable, Dict

from fastapi import APIRouter, Depends, HTTPException
from pydantic import BaseModel
from SPARQLWrapper.SPARQLExceptions import QueryBadFormed

from services.kg_execute.kg_client import IKgClient, KgClient, KgClientConfig
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
def get_domain2kgconfig():
    domain2kgconfig = defaultdict(dict)
    for key, value in os.environ.items():
        flag = False
        for field in fields(KgClientConfig):
            prefix = "KG_{field}_".format(field=field.name.upper())
            if key.startswith(prefix):
                flag = True
                domain = key[len(prefix) :].lower()
                break
        if flag:
            domain2kgconfig[domain][field.name] = value
    return {domain: KgClientConfig(**kv) for domain, kv in domain2kgconfig.items()}


def get_kg_client_factory():
    return KgClient


def get_kg_executor(
    domain2endpoint: Annotated[Dict[str, str], Depends(get_domain2kgconfig)],
    kg_client_factory: Annotated[
        Callable[[str], IKgClient], Depends(get_kg_client_factory)
    ],
):
    domain2sparql = {
        domain: kg_client_factory(endpoint)
        for domain, endpoint in domain2endpoint.items()
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
