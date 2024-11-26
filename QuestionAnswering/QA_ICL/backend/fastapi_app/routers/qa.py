import logging
from typing import Annotated

from fastapi import APIRouter, Depends

from controllers.qa import DataSupporter, get_data_supporter
from model.web.qa import QARequest, QAResponse


logger = logging.getLogger(__name__)

router = APIRouter()


@router.post("", response_model=QAResponse)
async def qa(
    req: QARequest,
    data_supporter: Annotated[DataSupporter, Depends(get_data_supporter)],
):
    logger.info("Received request to QA endpoint with the following request body")
    logger.info(req)

    return data_supporter.query(query=req.question)
