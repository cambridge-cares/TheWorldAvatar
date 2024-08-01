from typing import Annotated, get_args
from fastapi import Depends, Query, Request, Response

from model.kg.ontozeolite import (
    ZEOLITIC_MATERIAL_KEY,
    CrystalInfoKey,
    TopologicalPropertyKey,
)
from model.web.ontozeolite import (
    CrystalInfoRequest,
    ScalarTopologicalPropertyKey,
    TopoPropsRequest,
    UnitCellKey,
    UnitCellRequest,
    XRDPeakRequest,
    ZeoliteFrameworkRequest,
    ZeoliteFrameworkReturnFields,
)
from routers.base import RETURN_FIELD_QUERY_KEY

from routers.utils import parse_rhs_colon

UNIT_CELL_QUERY_KEYS = {
    f"UnitCell-{key.value}": key for cls in get_args(UnitCellKey) for key in cls
}
XRD_PEAK_QUERY_KEY = "XRDPeak"


async def parse_unit_cell_request(req: Request):
    return {
        py_key: [parse_rhs_colon(val) for val in req.query_params.getlist(query_key)]
        for query_key, py_key in UNIT_CELL_QUERY_KEYS.items()
        if query_key in req.query_params
    }


async def parse_zeolite_framework_request(
    req: Request,
    unit_cell: Annotated[UnitCellRequest, Depends(parse_unit_cell_request)],
    xrd_peak: Annotated[
        list[str],
        Query(
            ...,
            alias=XRD_PEAK_QUERY_KEY,
            description="URL-encoded JSON object with keys `position` (required), `width` (optional, defaults to `0.5`), `threshold` (optional, defaults to `50) that describe an XRD peak. All keys are optional.",
        ),
    ] = [],
    composite_bu: Annotated[
        list[str], Query(..., alias=TopologicalPropertyKey.COMPOSITE_BU)
    ] = [],
    secondary_bu: Annotated[
        list[str], Query(..., alias=TopologicalPropertyKey.SECONDARY_BU)
    ] = [],
):
    return ZeoliteFrameworkRequest(
        crystal_info=CrystalInfoRequest(
            xrd_peak=[XRDPeakRequest.model_validate_json(x) for x in xrd_peak],
            unit_cell=unit_cell,
        ),
        topo_props=TopoPropsRequest(
            scalars={
                key: [
                    parse_rhs_colon(val) for val in req.query_params.getlist(key.value)
                ]
                for key in get_args(ScalarTopologicalPropertyKey)
                if key.value in req.query_params
            },
            composite_bu=composite_bu,
            secondary_bu=secondary_bu,
        ),
    )


async def parse_zeolite_framework_return_fields(
    return_fields: Annotated[list[str], Query(..., alias=RETURN_FIELD_QUERY_KEY)] = []
):
    return_fields_set = set(return_fields)
    return ZeoliteFrameworkReturnFields(
        crystal_info=[key for key in CrystalInfoKey if key.value in return_fields_set],
        topo_props=[
            key for key in TopologicalPropertyKey if key.value in return_fields_set
        ],
        material=ZEOLITIC_MATERIAL_KEY in return_fields_set,
    )


UNIT_CELL_QUERY_PARAMS = [
    {
        "in": "query",
        "name": name,
        "schema": {
            "type": "string",
        },
    }
    for name in UNIT_CELL_QUERY_KEYS.keys()
]

SCALAR_TOPO_PROP_QUERY_PARAMS = [
    {
        "in": "query",
        "name": key.value,
        "schema": {"type": "array", "items": {"type": "string"}},
        "description": "RHS colon filters e.g. `eq:100`, `lte:200`",
    }
    for key in get_args(ScalarTopologicalPropertyKey)
]


class CIFResponse(Response):
    media_type = "chemical/x-cif"
