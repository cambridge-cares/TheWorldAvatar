import logging
from typing import Annotated

from fastapi import APIRouter, Depends, HTTPException, Response

from services.mol_vis.cif import CIFManager, get_cif_manager
from services.mol_vis.xyz import XYZManager, get_xyz_manager


logger = logging.getLogger(__name__)

router = APIRouter()


@router.get("/xyz/{iri:path}")
async def get_xyz(
    iri: str, xyz_manager: Annotated[XYZManager, Depends(get_xyz_manager)]
):
    logger.info("Received XYZ file request for IRI " + iri)
    xyz_str = xyz_manager.get(iri)

    if xyz_str is None:
        raise HTTPException(status_code=404, detail="Species not found")

    return Response(content=xyz_str, media_type="chemical/x-xyz")


@router.get("/cif/{iri:path}")
async def get_cif(
    iri: str, cif_manager: Annotated[CIFManager, Depends(get_cif_manager)]
):
    logger.info("Received CIF file request for IRI " + iri)
    cif_str = cif_manager.get(iri)

    if cif_str is None:
        raise HTTPException(status_code=404, detail="Crystal not found")

    return Response(content=cif_str, media_type="chemical/x-cif")
