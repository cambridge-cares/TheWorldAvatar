from typing import Annotated

from fastapi import APIRouter, Depends, Query

from model.kg.ontozeolite import OntozeoliteZeoliticMaterialBase
from model.web.ontozeolite import (
    CitationRequest,
    UnitCellRequest,
    ZeoliticMaterialRequest,
)
from routers.ontozeolite.common import UNIT_CELL_QUERY_PARAMS, parse_unit_cell_request
from services.rdf_stores.ontozeolite import (
    OntozeoliteRDFStore,
    get_ontozeolite_rdfStore,
)


router = APIRouter()


def parse_citation_request(
    author_family_name: Annotated[
        str | None, Query(..., alias="Author-family_name")
    ] = None,
    year: int | None = None,
    journal: Annotated[str | None, Query(..., alias="Journal")] = None,
    doi: str | None = None,
):
    return CitationRequest(
        author_family_name=author_family_name, year=year, journal=journal, doi=doi
    )


def parse_zeolitic_material_request(
    unit_cell: Annotated[UnitCellRequest, Depends(parse_unit_cell_request)],
    citation: Annotated[CitationRequest, Depends(parse_citation_request)],
    framework: Annotated[str | None, Query(..., alias="Framework")] = None,
    name: str | None = None,
    formula: Annotated[str | None, Query(..., alias="ChemicalFormula")] = None,
    framework_components: Annotated[
        list[str], Query(..., alias="FrameworkComponent")
    ] = [],
    guest_components: Annotated[list[str], Query(..., alias="GuestComponent")] = [],
):
    return ZeoliticMaterialRequest(
        framework=framework,
        name=name,
        formula=formula,
        framework_components=framework_components,
        guest_components=guest_components,
        unit_cell=unit_cell,
        citation=citation,
    )


@router.get(
    "/",
    summary="Get zeolite frameworks",
    openapi_extra={
        "parameters": [
            *UNIT_CELL_QUERY_PARAMS,
        ]
    },
    response_model=list[OntozeoliteZeoliticMaterialBase],
)
async def getZeoliticMaterials(
    req: Annotated[ZeoliticMaterialRequest, Depends(parse_zeolitic_material_request)],
    ontozeolite_store: Annotated[
        OntozeoliteRDFStore, Depends(get_ontozeolite_rdfStore)
    ],
):
    iris = ontozeolite_store.get_zeolitic_material_IRIs(req)
    return [x for x in ontozeolite_store.get_zeolitic_materials_many(iris=iris) if x]
