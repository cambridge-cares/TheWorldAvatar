import logging
from typing import Annotated

from fastapi import APIRouter, Depends, HTTPException, Response

from services.mol_vis.cif import CIFManager, get_cif_manager
from services.rdf_stores.ontozeolite import (
    OntozeoliteRDFStore,
    get_ontozeolite_rdfStore,
)
from .zeolite_frameworks import router as zeolite_framework_router

logger = logging.getLogger(__name__)

router = APIRouter()


@router.get(
    "/composite-building-units",
    summary="Get all composite building units",
    response_model=list[str],
)
async def get_cbu_all(
    ontozeolite_store: Annotated[OntozeoliteRDFStore, Depends(get_ontozeolite_rdfStore)]
):
    return ontozeolite_store.get_cbu_all()


@router.get(
    "/secondary-building-units",
    summary="Get all secondary building units",
    response_model=list[str],
)
async def get_sbu_all(
    ontozeolite_store: Annotated[OntozeoliteRDFStore, Depends(get_ontozeolite_rdfStore)]
):
    return ontozeolite_store.get_sbu_all()


class CIFResponse(Response):
    media_type = "chemical/x-cif"


@router.get(
    "/{iri:path}/cif",
    summary="Get zeolite's CIF geometry file",
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


router.include_router(zeolite_framework_router, prefix="/zeolite-frameworks")
