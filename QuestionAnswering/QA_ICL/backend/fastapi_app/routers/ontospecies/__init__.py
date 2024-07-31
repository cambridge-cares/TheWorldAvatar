import logging
from typing import Annotated

from fastapi import APIRouter, Depends

from model.kg.ontospecies import OntospeciesChemicalClass, OntospeciesUse
from services.rdf_stores.ontospecies import (
    OntospeciesRDFStore,
    get_ontospecies_rdfStore,
)
from .species import router as species_router
from .species_partial import router as species_partial_router


logger = logging.getLogger(__name__)

router = APIRouter()


@router.get(
    "/chemical-classes",
    summary="Get all chemical classes",
    response_model=list[OntospeciesChemicalClass],
)
async def getChemicalClasses(
    ontospecies_store: Annotated[OntospeciesRDFStore, Depends(get_ontospecies_rdfStore)]
):
    return ontospecies_store.get_chemical_classes_all()


@router.get(
    "/uses",
    summary="Get all uses",
    response_model=list[OntospeciesUse],
)
async def getUses(
    ontospecies_store: Annotated[OntospeciesRDFStore, Depends(get_ontospecies_rdfStore)]
):
    return ontospecies_store.get_uses_all()


router.include_router(species_router, prefix="/species")
router.include_router(species_partial_router, prefix="/species-partial")
