from typing import Annotated, Any, Dict, List, Literal, Optional, Union
from pydantic import BaseModel, Field


EXAMPLES_KEY_PREFIX = "nlq2actionExamples:"
EXAMPLES_INDEX_NAME = "idx:nlq2actionExamples_vss"


class SparqlBindingValuesItem(BaseModel):
    text: Optional[str] = None
    identifier: dict = dict()


class SparqlBinding(BaseModel):
    var: str
    clsname: str = Field(..., alias="class")
    values: List[SparqlBindingValuesItem]


class TypedVarNode(BaseModel):
    var: str
    clsname: str = Field(..., alias="class")


class SparqlAction(BaseModel):
    action_type: Literal["sparql"]
    namespace: str
    bindings: List[SparqlBinding] = []
    query: str
    nodes_for_expansion: List[TypedVarNode] = []


class FuncAction(BaseModel):
    action_type: Literal["func"]
    name: str
    args: Dict[str, Any]


DataRetrievalAction = Annotated[
    Union[SparqlAction, FuncAction], Field(discriminator="action_type")
]


class Nlq2ActionExample(BaseModel):
    nlq: str
    action: DataRetrievalAction


class Nlq2ActionExampleProcessed(Nlq2ActionExample):
    nlq_embedding: List[float]
