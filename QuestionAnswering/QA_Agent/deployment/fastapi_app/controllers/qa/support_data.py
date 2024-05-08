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
    traces: List[ScatterPlotTrace] = []


class WktDataItem(BaseModel):
    wkt_text: str
    srs_uri: str = "http://www.opengis.net/def/crs/OGC/1.3/CRS84"

    @classmethod
    def from_literal(cls, wkt_literal: str):
        start_srs = wkt_literal.find("<")
        end_srs = wkt_literal.find(">", start_srs + 1)

        if start_srs >= 0 and end_srs >= 0:
            return cls(
                srs_uri=wkt_literal[start_srs + 1 : end_srs],
                wkt_text=wkt_literal[end_srs + 1 :].strip(),
            )
        else:
            return cls(wkt_text=wkt_literal.strip())


DataItem = Union[TableDataItem, ScatterPlotDataItem, WktDataItem]


class DataSupporter(ABC):
    @abstractmethod
    def query(self, query: str) -> Tuple[List[QAStep], List[DataItem]]:
        pass
