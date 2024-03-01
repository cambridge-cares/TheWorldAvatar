import logging
import time
from typing import Annotated, List

from fastapi import APIRouter, Depends
from pydantic import BaseModel

from services.connector import AgentConnector, get_agent_connector


class QARequest(BaseModel):
    question: str


class QAResponse(BaseModel):
    latency: float
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

    start = time.time()
    data = agent_connector.query(req.question)
    end = time.time()

    return QAResponse(latency=end - start, data=data)
