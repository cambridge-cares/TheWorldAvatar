import logging
from typing import Annotated

from fastapi import APIRouter, Depends

from constants.namespace import BIBO, ONTOZEOLITE
from model.kg.ontospecies import OntospeciesSpeciesBase, PeriodictableElement
from model.kg.ontozeolite import BiboJournal
from services.rdf_stores.ontospecies import (
    OntospeciesRDFStore,
    get_ontospecies_rdfStore,
)
from services.rdf_stores.ontozeolite import (
    OntozeoliteRDFStore,
    get_ontozeolite_rdfStore,
)
from .zeolite_frameworks import router as zeolite_frameworks_router
from .zeolite_frameworks_partial import router as zeolite_frameworks_partial_router
from .zeolitic_materials import router as zeolitic_materials_router

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


@router.get(
    "/framework-components",
    summary="Get all elements that serve as framework components",
    response_model=list[PeriodictableElement],
)
async def get_framework_components_all(
    ontozeolite_store: Annotated[
        OntozeoliteRDFStore, Depends(get_ontozeolite_rdfStore)
    ],
    ontospecies_store: Annotated[
        OntospeciesRDFStore, Depends(get_ontospecies_rdfStore)
    ],
):
    query = f"""PREFIX zeo: <{ONTOZEOLITE}>
SELECT DISTINCT ?o
WHERE {{
    ?s zeo:hasFrameworkComponent ?o .
}}"""
    _, bindings = ontozeolite_store.sparql_client.querySelectThenFlatten(query)
    iris = [binding["o"] for binding in bindings]
    return [x for x in ontospecies_store.get_elements_many(iris) if x]


@router.get(
    "/guest-components",
    summary="Get all species that serve as guest components",
    response_model=list[OntospeciesSpeciesBase],
)
async def get_guest_components_all(
    ontozeolite_store: Annotated[
        OntozeoliteRDFStore, Depends(get_ontozeolite_rdfStore)
    ],
    ontospecies_store: Annotated[
        OntospeciesRDFStore, Depends(get_ontospecies_rdfStore)
    ],
):
    query = f"""PREFIX zeo: <{ONTOZEOLITE}>
SELECT DISTINCT ?o
WHERE {{
    ?s zeo:hasGuestComponent ?o .
}}"""
    _, bindings = ontozeolite_store.sparql_client.querySelectThenFlatten(query)
    iris = [binding["o"] for binding in bindings]
    return [x for x in ontospecies_store.get_species_base_many(iris) if x]


@router.get("/journals", summary="Get all journals", response_model=list[BiboJournal])
async def get_journals_all(
    ontozeolite_store: Annotated[OntozeoliteRDFStore, Depends(get_ontozeolite_rdfStore)]
):
    return ontozeolite_store.get_all(T=BiboJournal, type_iri=BIBO.Journal)


router.include_router(zeolite_frameworks_router, prefix="/zeolite-frameworks")
router.include_router(
    zeolite_frameworks_partial_router, prefix="/zeolite-frameworks-partial"
)
router.include_router(zeolitic_materials_router, prefix="/zeolitic-materials")
