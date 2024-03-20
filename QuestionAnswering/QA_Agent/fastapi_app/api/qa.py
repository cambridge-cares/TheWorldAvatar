import logging
import time
from typing import Annotated, List

from fastapi import APIRouter, Depends
from pydantic import BaseModel

from model.qa import QAData, QAMode, QAStep
from services.qa import get_marie_mediator, get_zaha_mediator
from services.connectors import AgentConnectorMediator


class QARequest(BaseModel):
    question: str
    domain: str  # TODO: validate domain based on qa_engine (marie or zaha)


class QAResponseMetadata(BaseModel):
    mode: QAMode
    latency: float
    steps: List[QAStep]


class QAResponse(BaseModel):
    metadata: QAResponseMetadata
    data: QAData


logger = logging.getLogger(__name__)

router = APIRouter()


def _qa(req: QARequest, mediator: AgentConnectorMediator):
    logger.info("Received request to QA endpoint with the following request body")
    logger.info(req)

    timestamp = time.time()
    qa_mode, steps, data = mediator.query(req.question, domain=req.domain)
    latency = time.time() - timestamp

    metadata = QAResponseMetadata(mode=qa_mode, latency=latency, steps=steps)

    return QAResponse(metadata=metadata, data=data)


@router.post("/marie")
def qa_marie(
    req: QARequest,
    mediator: Annotated[AgentConnectorMediator, Depends(get_marie_mediator)],
):
    return _qa(req=req, mediator=mediator)


@router.post("/zaha")
def qa_zaha(
    req: QARequest,
    mediator: Annotated[AgentConnectorMediator, Depends(get_zaha_mediator)],
):
    return _qa(req=req, mediator=mediator)
