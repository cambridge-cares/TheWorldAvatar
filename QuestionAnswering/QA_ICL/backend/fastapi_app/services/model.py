from __future__ import annotations
from typing import Annotated, Any, Dict, Literal, Sequence, Union

from pydantic import BaseModel, Field


class DocumentCollectionDataItem(BaseModel):
    type: Literal["document_collection"] = "document_collection"
    data: list[dict[str, Any]]


class TableDataItemBase(BaseModel):
    columns: list[str]
    data: Sequence[dict[str, str | float | Sequence[str | float] | TableDataItemBase | None]]

    @classmethod
    def from_data(cls, data: Sequence[dict[str, Any]]):
        cols = []
        cols_set = set()
        for datum in data:
            for k in datum.keys():
                if k not in cols_set:
                    cols.append(k)
                    cols_set.add(k)

        for datum in data:
            new_kv: Dict[str, cls] = dict()
            for k, v in datum.items():
                if isinstance(v, Sequence) and all(
                    isinstance(elem, dict) for elem in v
                ):
                    new_kv[k] = cls.from_data(v)
            datum.update(new_kv)

        return cls(columns=cols, data=data)


class TableDataItem(TableDataItemBase):
    type: Literal["table"] = "table"

    @classmethod
    def from_data(cls, data: Sequence[dict[str, Any]]):
        return cls(**super().from_data(data).model_dump())


class TypedSeries(BaseModel):
    data: list
    type: str


class ScatterPlotTrace(BaseModel):
    name: str | None = None
    x: TypedSeries
    y: TypedSeries


class ScatterPlotDataItem(BaseModel):
    type: Literal["scatter_plot"] = "scatter_plot"
    title: str | None = None
    traces: list[ScatterPlotTrace] = []


CRS84_URI = "http://www.opengis.net/def/crs/OGC/1.3/CRS84"


class MapDataItem(BaseModel):
    type: Literal["map"] = "map"
    title: str | None = None
    wkt_crs84: str


DataItem = Annotated[
    Union[DocumentCollectionDataItem, TableDataItem, ScatterPlotDataItem, MapDataItem],
    Field(discriminator="type"),
]
