from typing import Annotated, Dict, List, Literal, Optional, Union

from pydantic import BaseModel, Field


class TableHeader(BaseModel):
    value: str
    label: str
    dtype: Literal["string", "number", "object"]


class TableDataItem(BaseModel):
    type: Literal["table"] = "table"
    headers: List[TableHeader]
    data: List[Dict[str, Union[str, float, dict]]]

    @classmethod
    def from_vars_and_bindings(
        cls, vars: List[str], bindings: List[Dict[str, Union[str, float, dict]]]
    ):
        def resolve_dtype(var: str):
            for ptype, jstype in [(float, "number"), (dict, "object")]:
                if any(isinstance(binding[var], ptype) for binding in bindings):
                    return jstype
            return "string"

        return cls(
            headers=[
                TableHeader(value=var, label=var, dtype=resolve_dtype(var))
                for var in vars
            ],
            data=bindings,
        )


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
    Union[TableDataItem, ScatterPlotDataItem, MapDataItem],
    Field(discriminator="type"),
]
