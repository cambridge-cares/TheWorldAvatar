from __future__ import annotations
from typing import Annotated, Literal, Sequence

from pydantic import BaseModel, Field

from model.nlq2datareq import (
    DataRequest,
    Nlq2DataReqExample,
)
from model.rdf_schema import RDFRelation


class DocumentCollection(BaseModel):
    type: Literal["document_collection"] = "document_collection"
    data: list[dict[str, object]]


class TableDataColumn(BaseModel):
    value: str
    label: str

class TableDataBase(BaseModel):
    columns: list[TableDataColumn]
    data: Sequence[
        dict[str, str | float | Sequence[str] | Sequence[float] | TableDataBase | None]
    ]

    @classmethod
    def from_data(cls, data: Sequence[dict[str, object]]):
        cols: list[TableDataColumn] = []
        cols_set = set()
        for datum in data:
            for k in datum.keys():
                if k not in cols_set:
                    cols.append(TableDataColumn(value=k, label=k))
                    cols_set.add(k)

        for datum in data:
            new_kv: dict[str, cls] = dict()
            for k, v in datum.items():
                if isinstance(v, Sequence) and all(
                    isinstance(elem, dict) for elem in v
                ):
                    new_kv[k] = cls.from_data(v)
            datum.update(new_kv)

        return cls(columns=cols, data=data)


class TableData(TableDataBase):
    type: Literal["table"] = "table"

    @classmethod
    def from_data(cls, data: Sequence[dict[str, object]]):
        return cls(**super().from_data(data).model_dump())


class TypedSeries(BaseModel):
    data: list
    type: str


class ScatterPlotTrace(BaseModel):
    name: str | None = None
    x: TypedSeries
    y: TypedSeries


class ScatterPlotData(BaseModel):
    type: Literal["scatter_plot"] = "scatter_plot"
    title: str | None = None
    traces: list[ScatterPlotTrace] = []


CRS84_URI = "http://www.opengis.net/def/crs/OGC/1.3/CRS84"


class WKTGeometryData(BaseModel):
    type: Literal["wkt_geometry"] = "wkt_geometry"
    srs: Literal["crs84"] = "crs84"
    title: str | None = None
    literal: str


DataItem = Annotated[
    DocumentCollection | TableData | ScatterPlotData | WKTGeometryData,
    Field(discriminator="type"),
]


class QARequestArtifact(BaseModel):
    nlq: str
    data_req: DataRequest
    data: object


class QARequest(BaseModel):
    question: str


class TranslationContext(BaseModel):
    examples: list[Nlq2DataReqExample]
    schema_relations: list[RDFRelation]


class QAResponseMetadata(BaseModel):
    rewritten_question: str | None
    translation_context: TranslationContext
    data_request: DataRequest
    linked_variables: dict[str, list[str]]


class QAResponse(BaseModel):
    request_id: str
    metadata: QAResponseMetadata
    data: list[DataItem]
