from typing import Any

from pydantic import BaseModel


class QAStep(BaseModel):
    action: str
    arguments: Any = None
    results: Any = None
    latency: float
