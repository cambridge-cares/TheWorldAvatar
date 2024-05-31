from typing import Annotated, Any, Dict, List, Literal, Optional, Union
from pydantic import BaseModel, Field


EXAMPLES_KEY_PREFIX = "nlq2datareqExamples:"
EXAMPLES_INDEX_NAME = "idx:nlq2datareqExamples_vss"


class LexicalBindingValue(BaseModel):
    text: Optional[str] = None
    identifier: Dict[str, str] = dict()


class LexicalEntityBinding(BaseModel):
    cls: str
    values: List[LexicalBindingValue]


class SparqlNodeMappingConfig(BaseModel):
    pkey: bool = False
    cls: Optional[str] = None


class SparqlDataReqForm(BaseModel):
    type: Literal["sparql"] = "sparql"
    namespace: str
    query: str
    res_map: Dict[str, SparqlNodeMappingConfig]


class FuncDataReqForm(BaseModel):
    type: Literal["func"] = "func"
    name: str


DataRequestForm = Annotated[
    Union[SparqlDataReqForm, FuncDataReqForm], Field(discriminator="type")
]


class DataRequest(BaseModel):
    entity_bindings: Dict[str, LexicalEntityBinding] = dict()
    const_bindings: Dict[str, Any] = dict()
    req_form: DataRequestForm


class Nlq2DataReqExample(BaseModel):
    nlq: str
    data_req: DataRequest


class Nlq2DataReqExampleProcessed(Nlq2DataReqExample):
    nlq_embedding: List[float]
