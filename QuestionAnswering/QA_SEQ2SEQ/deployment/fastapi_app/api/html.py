from importlib.resources import files
import json
import os
from fastapi import APIRouter, Request
from fastapi.responses import HTMLResponse
from fastapi.templating import Jinja2Templates


templates = Jinja2Templates(directory="templates")

domains = ("chemistry", "cities")

SAMPLE_QUESTIONS = {
    domain: json.loads(
        files("resources." + domain).joinpath("sample_questions.json").read_text()
    )
    for domain in domains
}
METADATA = {
    domain: json.loads(
        files("resources." + domain).joinpath("metadata.json").read_text()
    )
    for domain in domains
}

router = APIRouter()


@router.get("/", response_class=HTMLResponse)
async def home(request: Request):
    superdomain = os.getenv("QA_SUPERDOMAIN", "chemistry")
    model_path = os.getenv("SEQ2SEQ_MODEL_PATH")
    model_version = model_path.split("/")[-1] if model_path else model_path

    return templates.TemplateResponse(
        "qa.html",
        dict(
            request=request,
            superdomain=superdomain,
            model_version=model_version,
            title=METADATA[superdomain]["title"],
            subtitle_paras=METADATA[superdomain]["subtitle"].split("\n"),
            sample_questions=SAMPLE_QUESTIONS,
        ),
    )
