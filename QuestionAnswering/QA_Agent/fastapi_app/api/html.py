from functools import cache
from importlib.resources import files
import json
import os
from typing import Annotated, Dict

from fastapi import APIRouter, Depends, Request
from fastapi.responses import HTMLResponse
from fastapi.templating import Jinja2Templates


templates = Jinja2Templates(directory="templates")


@cache
def get_superdomain():
    return os.getenv("QA_SUPERDOMAIN", "chemistry")


@cache
def get_metadata(superdomain: Annotated[str, Depends(get_superdomain)]):
    return json.loads(
        files("resources." + superdomain).joinpath("metadata.json").read_text()
    )


@cache
def get_sample_questions(superdomain: Annotated[str, Depends(get_superdomain)]):
    return json.loads(
        files("resources." + superdomain).joinpath("sample_questions.json").read_text()
    )


router = APIRouter()


@router.get("/", response_class=HTMLResponse)
async def home(
    request: Request,
    superdomain: Annotated[str, Depends(get_superdomain)],
    metadata: Annotated[Dict[str, str], Depends(get_metadata)],
    sample_questions: Annotated[list, Depends(get_sample_questions)],
):

    return templates.TemplateResponse(
        "qa.html",
        dict(
            request=request,
            superdomain=superdomain,
            title=metadata["title"],
            subtitle_paras=metadata["subtitle"].split("\n"),
            sample_questions=sample_questions,
        ),
    )
