from functools import cache
from importlib.resources import files
from typing import Annotated

from fastapi import Depends
from pydantic import BaseModel, TypeAdapter

from config import get_frontend_name


class PageMetadata(BaseModel):
    title: str
    subtitle: str


class DataDomainSampleQuestions(BaseModel):
    data_domain: str
    label: str
    questions: list[str]


class QADomainSampleQuestions(BaseModel):
    qa_domain: str
    label: str
    subdomains: list[DataDomainSampleQuestions]


@cache
def get_metadata(frontend_name: Annotated[str, Depends(get_frontend_name)]):
    return PageMetadata.model_validate_json(
        files("resources." + frontend_name).joinpath("metadata.json").read_text()
    )


@cache
def get_sample_questions(frontend_name: Annotated[str, Depends(get_frontend_name)]):
    return TypeAdapter(list[QADomainSampleQuestions]).validate_json(
        files("resources." + frontend_name)
        .joinpath("sample_questions.json")
        .read_text()
    )
