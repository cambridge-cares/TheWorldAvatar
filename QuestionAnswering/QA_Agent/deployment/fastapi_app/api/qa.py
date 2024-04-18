import logging
import time
from typing import Annotated, List

from fastapi import APIRouter, Depends
from pydantic import BaseModel

from model.qa import QAData, QAStep
from services.qa import get_mediator
from services.connectors import AgentConnectorMediator


class QARequest(BaseModel):
    question: str
    qa_domain: str  # TODO: validate domain based on qa_engine (marie or zaha)


class QAResponseMetadata(BaseModel):
    latency: float
    steps: List[QAStep]


class QAResponse(BaseModel):
    metadata: QAResponseMetadata
    data: QAData


logger = logging.getLogger(__name__)

router = APIRouter()


@router.post("/")
def qa(
    req: QARequest,
    mediator: Annotated[AgentConnectorMediator, Depends(get_mediator)],
):
    logger.info("Received request to QA endpoint with the following request body")
    logger.info(req)

    timestamp = time.time()
    steps, data = mediator.query(req.question, domain=req.qa_domain)
    latency = time.time() - timestamp

    metadata = QAResponseMetadata(latency=latency, steps=steps)

    return QAResponse(metadata=metadata, data=data)