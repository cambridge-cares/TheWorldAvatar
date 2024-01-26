import logging
import time

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
from SPARQLWrapper.SPARQLExceptions import QueryBadFormed

from services.kg_execute import KgExecutor, UnexpectedDomainError


class SparqlRequest(BaseModel):
    query: str
    domain: str


class SparqlResponse(BaseModel):
    data: dict
    latency: float


logger = logging.getLogger(__name__)

router = APIRouter()


kg_executor = KgExecutor()


@router.post("")
async def query(req: SparqlRequest):
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
