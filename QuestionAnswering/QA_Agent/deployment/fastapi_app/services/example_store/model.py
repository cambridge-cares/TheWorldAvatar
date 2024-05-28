from typing import Annotated, Any, Dict, List, Literal, Optional, Union
from pydantic import BaseModel, Field


EXAMPLES_KEY_PREFIX = "nlq2actionExamples:"
EXAMPLES_INDEX_NAME = "idx:nlq2actionExamples_vss"


class SparqlBindingValuesItem(BaseModel):
    text: Optional[str] = None
    identifier: dict = dict()


class TypedVarNode(BaseModel):
    var: str
    cls: str


class TypedVarNodeWithValues(TypedVarNode):
    values: List[SparqlBindingValuesItem]


class SparqlAction(BaseModel):
    action_type: Literal["sparql"]
    namespace: str
    bindings: List[TypedVarNodeWithValues] = []
    query: str
    nodes_to_augment: List[TypedVarNode] = []


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
