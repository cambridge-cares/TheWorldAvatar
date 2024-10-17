from pydantic import BaseModel

from model.nlq2datareq import DataRequest
from model.structured_answer import ChemicalStructureData, DataItem, TranslationContext


class QARequest(BaseModel):
    question: str


class QAResponseMetadata(BaseModel):
    rewritten_question: str | None
    translation_context: TranslationContext
    data_request: DataRequest
    linked_variables: dict[str, list[str]]


class QAResponse(BaseModel):
    request_id: str
    metadata: QAResponseMetadata
    data: list[DataItem]
    visualisation: dict[str, list[ChemicalStructureData]]
