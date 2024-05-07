from abc import ABC, abstractmethod
from typing import Dict, List, Optional, Tuple, Union

from pydantic import BaseModel

from model.qa import QAStep


class TableDataItem(BaseModel):
    vars: List[str]
    bindings: List[Dict[str, str]]


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
