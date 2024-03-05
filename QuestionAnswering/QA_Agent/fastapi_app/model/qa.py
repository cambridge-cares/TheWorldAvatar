from typing import Any, Dict, List

from pydantic import BaseModel


class QAStep(BaseModel):
    action: str
    arguments: List[dict] = []
    latency: float

class QAData(BaseModel):
    vars: List[str]
    bindings: List[Dict[str, Any]]