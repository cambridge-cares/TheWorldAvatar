from __future__ import annotations
from typing import Annotated, Literal, Sequence

from pydantic import BaseModel, Field

from model.nlq2datareq import (
    DataRequest,
    Nlq2DataReqExample,
)
from model.rdf_schema import RDFProperty


class DocumentCollection(BaseModel):
    type: Literal["document_collection"] = "document_collection"
    data: list[dict[str, object]]


class TableDataBase(BaseModel):
    columns: list[str]
    data: list[dict[str, str | float | list[str] | list[float] | TableDataBase | None]]

    @classmethod
    def from_data(
        cls,
        data: Sequence[dict[str, object]],
    ):
        cols: list[str] = []
        cols_set = set()
        for datum in data:
            for k in datum.keys():
                if k not in cols_set:
                    cols.append(k)
                    cols_set.add(k)

        for datum in data:
            new_kv: dict[str, TableDataBase] = dict()
            for k, v in datum.items():
                if (isinstance(v, list) or isinstance(v, tuple)) and all(
                    isinstance(elem, dict) for elem in v
                ):
                    new_kv[k] = cls.from_data(v)
            datum.update(new_kv)
        return cls(columns=cols, data=data)


class TableData(TableDataBase):
    type: Literal["table"] = "table"

    @classmethod
    def from_data(cls, data: Sequence[dict[str, object]]):
        base = super().from_data(data)
        return cls(columns=base.columns, data=base.data)


class ChemicalStructureData(BaseModel):
    type: Literal["xyz", "cif"]
    iri: str
    label: str
    data: str


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
    nlq_rewritten: str | None
    data_req: DataRequest
    data: object


class TranslationContext(BaseModel):
    examples: list[tuple[Nlq2DataReqExample, float]]
    properties: list[tuple[RDFProperty, float]]
