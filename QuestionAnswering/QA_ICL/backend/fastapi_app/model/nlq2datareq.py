from typing import Annotated, Literal
from pydantic import BaseModel, Field


NLQ2DATAREQ_EXAMPLES_KEY_PREFIX = "nlq2datareqExamples:"
NLQ2DATAREQ_EXAMPLES_INDEX_NAME = "idx:nlq2datareqExamples_vss"


class SparqlDataReqForm(BaseModel):
    type: Literal["sparql"] = "sparql"
    namespace: str
    query: str
    pkeys: list[str]


class FuncDataReqForm(BaseModel):
    type: Literal["func"] = "func"
    name: str


DataRequestForm = Annotated[
    SparqlDataReqForm | FuncDataReqForm, Field(discriminator="type")
]


class DataRequest(BaseModel):
    var2cls: dict[str, str] = dict()
    entity_bindings: dict[str, list[str | dict[str, str]]] = dict()
    const_bindings: dict[str, object] = dict()
    req_form: DataRequestForm | None = None
    visualise: list[str] = list()


class Nlq2DataReqExample(BaseModel):
    nlq: str
    data_req: DataRequest


class Nlq2DataReqExampleProcessed(Nlq2DataReqExample):
    nlq_embedding: list[float]
