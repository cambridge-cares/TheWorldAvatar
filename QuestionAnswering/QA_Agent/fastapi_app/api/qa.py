import logging
import time
from typing import Annotated, List, Optional

from fastapi import APIRouter, Depends
from pydantic import BaseModel

from model.qa import QAData, QAMode, QAStep
from services.connector import (
    AgentConnectorMediator,
    get_chemistry_mediator,
    get_cities_mediator,
)


class QARequest(BaseModel):
    question: str
    domain: Optional[str] = None


class QAResponseMetadata(BaseModel):
    mode: QAMode
    latency: float
    steps: List[QAStep]


class QAResponse(BaseModel):
    metadata: QAResponseMetadata
    data: QAData


logger = logging.getLogger(__name__)

chemistry_router = APIRouter()
cities_router = APIRouter()


@chemistry_router.post("/")
def qa_chemistry(
    req: QARequest,
    mediator: Annotated[AgentConnectorMediator, Depends(get_chemistry_mediator)],
):
    logger.info("Received request to QA endpoint with the following request body")
    logger.info(req)

    timestamp = time.time()
    qa_mode, steps, data = mediator.query(req.question)
    latency = time.time() - timestamp

    metadata = QAResponseMetadata(mode=qa_mode, latency=latency, steps=steps)

    return QAResponse(metadata=metadata, data=data)


@cities_router.post("/")
def qa_cities(
    req: QARequest,
    mediator: Annotated[AgentConnectorMediator, Depends(get_cities_mediator)],
):
    logger.info("Received request to QA endpoint with the following request body")
    logger.info(req)

    timestamp = time.time()
    qa_mode, steps, data = mediator.query(query=req.question, domain=req.domain)
    latency = time.time() - timestamp

    metadata = QAResponseMetadata(mode=qa_mode, latency=latency, steps=steps)

    return QAResponse(metadata=metadata, data=data)
