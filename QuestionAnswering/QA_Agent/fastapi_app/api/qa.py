import logging
import time
from typing import Annotated, List

from fastapi import APIRouter, Depends
from pydantic import BaseModel

from model.qa import QAData, QAStep
from services.connector import AgentConnectorMediator, get_agent_connector_mediator


class QARequest(BaseModel):
    question: str


class QAResponseMetadata(BaseModel):
    latency: float
    steps: List[QAStep]


class QAResponse(BaseModel):
    metadata: QAResponseMetadata
    data: QAData


logger = logging.getLogger(__name__)

router = APIRouter()


@router.post("")
def qa(
    req: QARequest,
    agent_connector: Annotated[AgentConnectorMediator, Depends(get_agent_connector_mediator)],
):
    logger.info("Received request to QA endpoint with the following request body")
    logger.info(req)

    timestamp = time.time()
    steps, data = agent_connector.query(req.question)
    latency = time.time() - timestamp

    return QAResponse(
        metadata=QAResponseMetadata(latency=latency, steps=steps), data=data
    )
