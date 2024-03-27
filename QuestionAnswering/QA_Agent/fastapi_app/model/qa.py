from typing import Any, Dict, List, Optional

from pydantic import BaseModel


class QAStep(BaseModel):
    action: str
    arguments: Any = None
    results: Any = None
    latency: float

class QAData(BaseModel):
    title: Optional[str] = None
    vars: List[str] = []
    bindings: List[Dict[str, Any]] = []
