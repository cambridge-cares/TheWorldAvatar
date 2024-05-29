from typing import Annotated, Any, Dict, List, Literal, Optional, Union
from pydantic import BaseModel, Field


EXAMPLES_KEY_PREFIX = "nlq2datareqExamples:"
EXAMPLES_INDEX_NAME = "idx:nlq2datareqExamples_vss"


class SparqlBindingValuesItem(BaseModel):
    text: Optional[str] = None
    identifier: dict = dict()


class TypedVarNode(BaseModel):
    var: str
    cls: str


class TypedVarNodeWithValues(TypedVarNode):
    values: List[SparqlBindingValuesItem]


class SparqlDataRequest(BaseModel):
    type: Literal["sparql"] = "sparql"
    namespace: str
    bindings: List[TypedVarNodeWithValues] = []
    query: str
    nodes_to_augment: List[TypedVarNode] = []


class FuncDataRequest(BaseModel):
    type: Literal["func"] = "func"
    name: str
    args: Dict[str, Any]


DataRequest = Annotated[
    Union[SparqlDataRequest, FuncDataRequest], Field(discriminator="type")
]


class Nlq2DataReqExample(BaseModel):
    nlq: str
    data_req: DataRequest


class Nlq2DataReqExampleProcessed(Nlq2DataReqExample):
    nlq_embedding: List[float]
