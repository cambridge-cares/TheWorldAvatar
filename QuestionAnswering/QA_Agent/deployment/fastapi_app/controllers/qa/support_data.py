from abc import ABC, abstractmethod
from typing import Any, Dict, List, Optional, Tuple, Union

from pydantic import BaseModel


class QAStep(BaseModel):
    action: str
    arguments: Any = None
    results: Any = None
    latency: float


class TableDataItem(BaseModel):
    vars: List[str]
    bindings: List[Dict[str, Union[str, float]]]


class TypedSeries(BaseModel):
    data: list
    type: str


class ScatterPlotTrace(BaseModel):
    name: Optional[str] = None
    x: TypedSeries
    y: TypedSeries


class ScatterPlotDataItem(BaseModel):
    title: Optional[str] = None
    traces: List[ScatterPlotTrace] = []


CRS84_URI = "http://www.opengis.net/def/crs/OGC/1.3/CRS84"


class WktCrs84DataItem(BaseModel):
    wkt_text: str


DataItem = Union[TableDataItem, ScatterPlotDataItem, WktCrs84DataItem]


def serialize_data_item(item: DataItem):
    if isinstance(item, TableDataItem):
        t = "table"
    elif isinstance(item, ScatterPlotDataItem):
        t = "scatter_plot"
    else:
        t = "wkt_crs84"

    return {"type": t, "data": item.model_dump()}


class DataSupporter(ABC):
    @abstractmethod
    def query(self, query: str) -> Tuple[List[QAStep], List[DataItem]]:
        pass
