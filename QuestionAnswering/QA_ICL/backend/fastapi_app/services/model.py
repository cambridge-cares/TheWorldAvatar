from typing import Annotated, Any, Dict, List, Literal, Optional, Union

from pydantic import BaseModel, Field


class DocumentCollectionDataItem(BaseModel):
    type: Literal["document_collection"] = "document_collection"
    data: List[Dict[str, Any]]


class TableDataItem(BaseModel):
    type: Literal["table"] = "table"
    columns: List[str]
    data: List[Dict[str, Any]]

    @classmethod
    def from_data(cls, data: List[Dict[str, Any]]):
        cols = []
        cols_set = set()
        for datum in data:
            for k in datum.keys():
                if k not in cols_set:
                    cols.append(k)
                    cols_set.add(k)

        return cls(columns=cols, data=data)


class TypedSeries(BaseModel):
    data: list
    type: str


class ScatterPlotTrace(BaseModel):
    name: Optional[str] = None
    x: TypedSeries
    y: TypedSeries


class ScatterPlotDataItem(BaseModel):
    type: Literal["scatter_plot"] = "scatter_plot"
    title: Optional[str] = None
    traces: List[ScatterPlotTrace] = []


CRS84_URI = "http://www.opengis.net/def/crs/OGC/1.3/CRS84"


class MapDataItem(BaseModel):
    type: Literal["map"] = "map"
    title: Optional[str] = None
    wkt_crs84: str


DataItem = Annotated[
    Union[DocumentCollectionDataItem, TableDataItem, ScatterPlotDataItem, MapDataItem],
    Field(discriminator="type"),
]
