from functools import cache
from importlib.resources import files
from typing import Annotated, List

from fastapi import Depends
from pydantic import BaseModel, TypeAdapter

from config import QAEngineName, get_qa_engine_name


class PageMetadata(BaseModel):
    title: str
    subtitle: str


class DataDomainSampleQuestions(BaseModel):
    data_domain: str
    label: str
    questions: List[str]


class QADomainSampleQuestions(BaseModel):
    qa_domain: str
    label: str
    subdomains: List[DataDomainSampleQuestions]


@cache
def get_metadata(qa_engine: Annotated[QAEngineName, Depends(get_qa_engine_name)]):
    return PageMetadata.model_validate_json(
        files("resources." + qa_engine.value).joinpath("metadata.json").read_text()
    )


@cache
def get_sample_questions(qa_engine: Annotated[QAEngineName, Depends(get_qa_engine_name)]):
    return TypeAdapter(List[QADomainSampleQuestions]).validate_json(
        files("resources." + qa_engine.value)
        .joinpath("sample_questions.json")
        .read_text()
    )
