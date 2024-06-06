from typing import Annotated, Literal, Union
from pydantic import BaseModel, Field


EXAMPLES_KEY_PREFIX = "nlq2datareqExamples:"
EXAMPLES_INDEX_NAME = "idx:nlq2datareqExamples_vss"


class LexicalBindingValue(BaseModel):
    text: str | None = None
    identifier: dict[str, str] = dict()


class LexicalEntityBinding(BaseModel):
    cls: str
    values: list[LexicalBindingValue]


class SparqlNodeMappingConfig(BaseModel):
    pkey: bool = False
    cls: str | None = None


class SparqlDataReqForm(BaseModel):
    type: Literal["sparql"] = "sparql"
    namespace: str
    query: str
    res_map: dict[str, SparqlNodeMappingConfig]


class FuncDataReqForm(BaseModel):
    type: Literal["func"] = "func"
    name: str


DataRequestForm = Annotated[
    SparqlDataReqForm | FuncDataReqForm, Field(discriminator="type")
]


class DataRequest(BaseModel):
    entity_bindings: dict[str, LexicalEntityBinding] = dict()
    const_bindings: dict[str, object] = dict()
    req_form: DataRequestForm


class Nlq2DataReqExample(BaseModel):
    nlq: str
    data_req: DataRequest


class Nlq2DataReqExampleProcessed(Nlq2DataReqExample):
    nlq_embedding: list[float]
