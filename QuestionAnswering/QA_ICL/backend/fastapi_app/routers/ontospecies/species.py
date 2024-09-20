import logging
from typing import Annotated

from fastapi import APIRouter, Depends, HTTPException, Response

from model.kg.ontospecies import (
    OntospeciesSpecies,
    OntospeciesSpeciesBase,
)
from model.web.ontospecies import SpeciesRequest
from routers.ontospecies.common import (
    OPENAPI_SPECIES_IDENTIFIER_QUERY_PARAMS,
    OPENAPI_SPECIES_PROPERTY_QUERY_PARAMS,
    parse_species_request,
)
from services.mol_vis.xyz import XYZManager, get_xyz_manager
from services.rdf_stores.ontospecies import (
    OntospeciesRDFStore,
    get_ontospecies_rdfStore,
)


logger = logging.getLogger(__name__)

router = APIRouter()


@router.get(
    "",
    summary="Get species",
    openapi_extra={
        "parameters": [
            *OPENAPI_SPECIES_PROPERTY_QUERY_PARAMS,
            *OPENAPI_SPECIES_IDENTIFIER_QUERY_PARAMS,
        ]
    },
    response_model=list[OntospeciesSpeciesBase],
)
async def getSpecies(
    species_req: Annotated[SpeciesRequest, Depends(parse_species_request)],
    ontospecies_store: Annotated[
        OntospeciesRDFStore, Depends(get_ontospecies_rdfStore)
    ],
):
    iris = ontospecies_store.get_species_IRIs(species_req)
    return [x for x in ontospecies_store.get_species_base_many(iris) if x]


class XYZResponse(Response):
    media_type = "chemical/x-xyz"


@router.get(
    "/xyz",
    summary="Get species' XYZ geometry file",
    response_class=XYZResponse,
)
async def getSpeciesXYZ(
    iri: str, xyz_manager: Annotated[XYZManager, Depends(get_xyz_manager)]
):
    xyz = xyz_manager.get_from_pubchem([iri])[0]
    if not xyz:
        raise HTTPException(
            status_code=404, detail=f"XYZ file not found for species `{iri}`"
        )
    return XYZResponse(
        content=xyz,
        headers={"Content-Disposition": 'attachment; filename="species.xyz"'},
    )


@router.get(
    "/one",
    summary="Get species",
    response_model_exclude_none=True,
    response_model=OntospeciesSpecies,
)
async def getSpeciesOne(
    iri: str,
    ontospecies_store: Annotated[
        OntospeciesRDFStore, Depends(get_ontospecies_rdfStore)
    ],
):
    species = ontospecies_store.get_species_many(iris=[iri])[0]
    if species is None:
        raise HTTPException(
            status_code=404, detail=f'No species is found with IRI "{iri}"'
        )
    return species
