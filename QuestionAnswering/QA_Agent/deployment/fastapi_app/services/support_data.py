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


class ScatterPlotTrace(BaseModel):
    name: Optional[str] = None
    x: list
    y: list


class ScatterPlotDataItem(BaseModel):
    title: Optional[str] = None
    traces: List[ScatterPlotTrace]


class WktDataItem(BaseModel):
    value: str


DataItem = Union[TableDataItem, ScatterPlotDataItem, WktDataItem]


class DataSupporter(ABC):
    @abstractmethod
    def query(self, query: str) -> Tuple[List[QAStep], List[DataItem]]:
        pass
