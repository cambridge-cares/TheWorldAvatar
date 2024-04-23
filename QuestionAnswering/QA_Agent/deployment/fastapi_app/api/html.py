import os
from typing import Annotated, List

from fastapi import APIRouter, Depends, Request
from fastapi.responses import HTMLResponse
from fastapi.templating import Jinja2Templates

from config import QAEngineName, get_qa_engine
from services.html import (
    PageMetadata,
    QADomainSampleQuestions,
    get_metadata,
    get_sample_questions,
)


templates = Jinja2Templates(directory="templates")


router = APIRouter()


@router.get("/", response_class=HTMLResponse)
async def home(
    request: Request,
    qa_engine: Annotated[QAEngineName, Depends(get_qa_engine)],
    metadata: Annotated[PageMetadata, Depends(get_metadata)],
    sample_questions: Annotated[
        List[QADomainSampleQuestions], Depends(get_sample_questions)
    ],
):

    return templates.TemplateResponse(
        "qa.html",
        dict(
            request=request,
            qa_engine=qa_engine.value,
            ga_measurement_id=os.getenv("GA_MEASUREMENT_ID"),
            qa_domains=[
                dict(value=datum.qa_domain, label=datum.label)
                for datum in sample_questions
            ],
            title=metadata.title,
            subtitle_paras=metadata.subtitle.split("\n"),
            sample_questions=[datum.model_dump() for datum in sample_questions],
        ),
    )
