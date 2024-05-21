from typing import List, Literal
from pydantic import BaseModel


QADomain = Literal["singapore", "chemistry"]


class Nlq2ActionExample(BaseModel):
    qa_domain: QADomain
    nlq: str
    action: dict


class Nlq2ActionExampleProcessed(Nlq2ActionExample):
    nlq_embedding: List[float]
