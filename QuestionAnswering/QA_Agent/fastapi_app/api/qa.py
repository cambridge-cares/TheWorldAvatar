import logging
import time
from typing import Annotated, List

from fastapi import APIRouter, Depends
from pydantic import BaseModel

from model.qa import QAStep
from services.connector import AgentConnector, get_agent_connector


class QARequest(BaseModel):
    question: str


class QAResponseMetadata(BaseModel):
    latency: float
    steps: List[QAStep]


class QAResponse(BaseModel):
    metadata: QAResponseMetadata
    data: List[dict]


logger = logging.getLogger(__name__)

router = APIRouter()


@router.post("")
def qa(
    req: QARequest,
    agent_connector: Annotated[AgentConnector, Depends(get_agent_connector)],
):
    logger.info("Received request to QA endpoint with the following request body")
    logger.info(req)

    timestamp = time.time()
    steps, data = agent_connector.query(req.question)
    latency = time.time() - timestamp

    return QAResponse(
        metadata=QAResponseMetadata(latency=latency, steps=steps), data=data
    )
