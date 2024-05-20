import logging
import time
from typing import Annotated, List

from fastapi import APIRouter, Depends
from pydantic import BaseModel, model_serializer

from controllers.qa import DataSupporter, get_data_supporter
from controllers.qa.retrieve import QADomain
from controllers.qa.model import QAStep, serialize_data_item
from controllers.qa.model import DataItem


class QARequest(BaseModel):
    question: str
    qa_domain: QADomain


class QAResponseMetadata(BaseModel):
    latency: float
    steps: List[QAStep]


class QAResponse(BaseModel):
    metadata: QAResponseMetadata
    data: List[DataItem]

    @model_serializer()
    def serialize_model(self):
        return {
            "metadata": self.metadata.model_dump(),
            "data": [serialize_data_item(item) for item in self.data],
        }


logger = logging.getLogger(__name__)

router = APIRouter()


@router.post("/")
def qa(
    req: QARequest,
    data_supporter: Annotated[DataSupporter, Depends(get_data_supporter)],
):
    logger.info("Received request to QA endpoint with the following request body")
    logger.info(req)

    timestamp = time.time()
    steps, data = data_supporter.query(qa_domain=req.qa_domain, query=req.question)
    latency = time.time() - timestamp

    metadata = QAResponseMetadata(latency=latency, steps=steps)

    return QAResponse(metadata=metadata, data=data)
