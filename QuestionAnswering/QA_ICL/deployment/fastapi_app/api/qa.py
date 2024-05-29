import logging
import time
from typing import Annotated, List

from fastapi import APIRouter, Depends
from pydantic import BaseModel

from controllers.qa import DataSupporter, get_data_supporter
from controllers.qa.model import QAStep
from services.model import DataItem


class QARequest(BaseModel):
    question: str


class QAResponseMetadata(BaseModel):
    latency: float
    steps: List[QAStep]


class QAResponse(BaseModel):
    metadata: QAResponseMetadata
    data: List[DataItem]


logger = logging.getLogger(__name__)

router = APIRouter()


@router.post("/", response_model=QAResponse)
def qa(
    req: QARequest,
    data_supporter: Annotated[DataSupporter, Depends(get_data_supporter)],
):
    logger.info("Received request to QA endpoint with the following request body")
    logger.info(req)

    timestamp = time.time()
    steps, data = data_supporter.query(query=req.question)
    latency = time.time() - timestamp

    metadata = QAResponseMetadata(latency=latency, steps=steps)

    return QAResponse(metadata=metadata, data=data)
