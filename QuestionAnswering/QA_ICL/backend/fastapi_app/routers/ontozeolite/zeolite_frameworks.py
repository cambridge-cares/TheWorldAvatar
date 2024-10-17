import logging
from typing import Annotated

from fastapi import APIRouter, Depends, HTTPException, Response

from model.kg.ontozeolite import (
    OntozeoliteZeoliteFramework,
    OntozeoliteZeoliteFrameworkBase,
)
from model.web.ontozeolite import (
    ZeoliteFrameworkRequest,
)
from routers.ontozeolite.common import (
    SCALAR_TOPO_PROP_QUERY_PARAMS,
    UNIT_CELL_QUERY_PARAMS,
    CIFResponse,
    parse_zeolite_framework_request,
)
from services.mol_vis.cif import CIFManager, get_cif_manager
from services.rdf_stores.ontozeolite import (
    OntozeoliteRDFStore,
    get_ontozeolite_rdfStore,
)


logger = logging.getLogger(__name__)

router = APIRouter()


@router.get(
    "",
    summary="Get zeolite frameworks",
    openapi_extra={
        "parameters": [
            *UNIT_CELL_QUERY_PARAMS,
            *SCALAR_TOPO_PROP_QUERY_PARAMS,
        ]
    },
    response_model=list[OntozeoliteZeoliteFrameworkBase],
)
async def getZeoliteFrameworks(
    framework_req: Annotated[
        ZeoliteFrameworkRequest, Depends(parse_zeolite_framework_request)
    ],
    ontozeolite_store: Annotated[
        OntozeoliteRDFStore, Depends(get_ontozeolite_rdfStore)
    ],
):
    iris = ontozeolite_store.get_zeolite_framework_IRIs(framework_req)
    return [x for x in ontozeolite_store.get_zeolite_framework_base_many(iris=iris) if x]


@router.get(
    "/cif",
    summary="Get zeolite framework's CIF geometry file",
    response_class=CIFResponse,
)
async def getZeoliteFrameworkCIF(
    iri: str, cif_manager: Annotated[CIFManager, Depends(get_cif_manager)]
):
    cif = cif_manager.get([iri])[0]
    if not cif:
        raise HTTPException(
            status_code=404, detail=f"CIF not found for zeolite `{iri}`"
        )
    return CIFResponse(
        content=cif,
        headers={"Content-Disposition": 'attachment; filename="zeolite.cif"'},
    )


@router.get(
    "/one",
    summary="Get zeolite framework",
    response_model=OntozeoliteZeoliteFramework,
    response_model_exclude_none=True,
)
async def getZeoliteFrameworkOne(
    iri: str,
    ontozeolite_store: Annotated[
        OntozeoliteRDFStore, Depends(get_ontozeolite_rdfStore)
    ],
):
    return ontozeolite_store.get_zeolite_framework_one(iri)
