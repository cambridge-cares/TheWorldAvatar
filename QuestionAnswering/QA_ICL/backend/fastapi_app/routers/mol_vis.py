import logging
from typing import Annotated

from fastapi import APIRouter, Depends, Response

from controllers.mol_vis import XYZManager, get_xyz_manager


logger = logging.getLogger(__name__)

router = APIRouter()


@router.get("/xyz/{iri:path}")
async def xyz(iri: str, xyz_manager: Annotated[XYZManager, Depends(get_xyz_manager)]):
    logger.info("Received XYZ file request for IRI " + iri)
    xyz_str = xyz_manager.get(iri)
    return Response(content=xyz_str, media_type="chemical/x-xyz")
