import logging
import time
from typing import Annotated, List

from fastapi import APIRouter, Depends
from pydantic import BaseModel

from controllers.qa import DataSupporter, get_data_supporter
from services.model import DataItem


class QARequest(BaseModel):
    question: str


class QAResponse(BaseModel):
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

    data = data_supporter.query(query=req.question)

    return QAResponse(data=data)
