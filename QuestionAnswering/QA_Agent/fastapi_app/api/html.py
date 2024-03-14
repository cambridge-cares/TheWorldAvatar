from functools import cache
from importlib.resources import files
import json
import os
from typing import Annotated, Dict, List

from fastapi import APIRouter, Depends, Request
from fastapi.responses import HTMLResponse
from fastapi.templating import Jinja2Templates

from config import QA_SUPERDOMAIN


templates = Jinja2Templates(directory="templates")


@cache
def get_metadata():
    return json.loads(
        files("resources." + QA_SUPERDOMAIN).joinpath("metadata.json").read_text()
    )


@cache
def get_sample_questions():
    return json.loads(
        files("resources." + QA_SUPERDOMAIN)
        .joinpath("sample_questions.json")
        .read_text()
    )


def get_domains_info(
    sample_questions: Annotated[List[dict], Depends(get_sample_questions)]
):
    return [dict(value=x["domain"], label=x["label"]) for x in sample_questions]


router = APIRouter()


@router.get("/", response_class=HTMLResponse)
async def home(
    request: Request,
    metadata: Annotated[Dict[str, str], Depends(get_metadata)],
    sample_questions: Annotated[List[dict], Depends(get_sample_questions)],
    domains_info: Annotated[List[dict], Depends(get_domains_info)],
):

    return templates.TemplateResponse(
        "qa.html",
        dict(
            request=request,
            superdomain=QA_SUPERDOMAIN,
            domains=domains_info,
            title=metadata["title"],
            subtitle_paras=metadata["subtitle"].split("\n"),
            sample_questions=sample_questions,
        ),
    )
