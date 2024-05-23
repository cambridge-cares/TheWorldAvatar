from typing import List
from pydantic import BaseModel

EXAMPLES_KEY_PREFIX = "nlq2actionExamples:"
EXAMPLES_INDEX_NAME = "idx:nlq2actionExamples_vss"


class Nlq2ActionExample(BaseModel):
    nlq: str
    action: dict


class Nlq2ActionExampleProcessed(Nlq2ActionExample):
    nlq_embedding: List[float]
