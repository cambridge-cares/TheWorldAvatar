from typing import Any, List
from pydantic import BaseModel


class QAStep(BaseModel):
    action: str
    arguments: List[dict] = []
    latency: float