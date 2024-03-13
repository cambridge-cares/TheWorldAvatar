from typing import Any, Dict, List, Literal

from pydantic import BaseModel


class QAStep(BaseModel):
    action: str
    arguments: Any = None
    results: Any = None
    latency: float

class QAData(BaseModel):
    vars: List[str] = []
    bindings: List[Dict[str, Any]] = []

QAMode = Literal["IR", "QA"]