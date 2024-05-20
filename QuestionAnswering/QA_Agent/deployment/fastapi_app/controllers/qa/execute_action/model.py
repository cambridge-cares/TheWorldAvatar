from typing import Any, Dict, List, Optional

from pydantic import BaseModel, Field


class ActionBase(BaseModel):
    pass


class SparqlBindingValuesItem(BaseModel):
    clsname: str = Field(..., alias="class")
    text: Optional[str] = None


class SparqlBinding(BaseModel):
    var: str
    values: List[SparqlBindingValuesItem]


class SparqlAction(ActionBase):
    namespace: str
    bindings: List[SparqlBinding]
    query: str


class FuncAction(ActionBase):
    name: str
    args: Dict[str, Any]
