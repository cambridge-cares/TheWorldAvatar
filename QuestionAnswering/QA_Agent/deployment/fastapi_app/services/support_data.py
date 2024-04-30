from abc import ABC, abstractmethod
import json
from typing import Any, Dict, List, Optional, Tuple, Union

from pydantic import BaseModel, model_serializer

from model.qa import QAData, QAStep


class TableDataItem(BaseModel):
    vars: List[str]
    bindings: List[Dict[str, Any]]


class ScatterPlotTrace(BaseModel):
    name: Optional[str] = None
    x: list
    y: list


class ScatterPlotDataItem(BaseModel):
    title: Optional[str] = None
    traces: List[ScatterPlotTrace] = []


class WktDataItem(BaseModel):
    pass


DataItem = Union[TableDataItem, ScatterPlotDataItem, WktDataItem]


class DataSupporter(ABC):
    @abstractmethod
    def query(self, query: str) -> Tuple[List[QAStep], List[DataItem]]:
        pass
