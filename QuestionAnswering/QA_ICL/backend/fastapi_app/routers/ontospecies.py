import logging
from typing import Annotated

from fastapi import APIRouter, Depends, HTTPException, Query, Request, Response

from model.kg.ontospecies import SpeciesPropertyKey
from model.ontospecies import SpeciesRequest
from routers.utils import parse_rhs_colon
from services.mol_vis.xyz import XYZManager, get_xyz_manager
from services.rdf_stores.ontospecies import (
    OntospeciesRDFStore,
    get_ontospecies_rdfStore,
)
from utils.str import CAMEL_CASE_PATTERN


logger = logging.getLogger(__name__)

router = APIRouter()


@router.get("/chemical-classes", summary="Get all chemical classes")
async def getChemicalClasses(
    ontospecies_store: Annotated[OntospeciesRDFStore, Depends(get_ontospecies_rdfStore)]
):
    return ontospecies_store.get_chemical_classes_all()


SPECIES_PROPERTY_QUERY_KEYS = {
    CAMEL_CASE_PATTERN.sub("-", key.value).lower(): key for key in SpeciesPropertyKey
}


async def parse_species_request(
    req: Request,
    chemical_class: Annotated[list[str], Query(..., alias="chemical-class")] = [],
    use: Annotated[list[str], Query()] = [],
):
    return SpeciesRequest(
        chemical_class=chemical_class,
        use=use,
        property={
            py_key: [
                parse_rhs_colon(val) for val in req.query_params.getlist(query_key)
            ]
            for query_key, py_key in SPECIES_PROPERTY_QUERY_KEYS.items()
        },
    )


@router.get(
    "/species",
    summary="Get species",
    openapi_extra={
        "parameters": [
            {
                "in": "query",
                "name": name,
                "schema": {"type": "array", "items": {"type": "string"}},
                "description": "RHS colon filters e.g. `eq:100`, `lte:200`",
            }
            for name in SPECIES_PROPERTY_QUERY_KEYS.keys()
        ]
    },
)
async def getSpecies(
    species_req: Annotated[SpeciesRequest, Depends(parse_species_request)],
    ontospecies_store: Annotated[
        OntospeciesRDFStore, Depends(get_ontospecies_rdfStore)
    ],
):
    return ontospecies_store.get_species(species_req)


@router.get("/species/{iri:path}/xyz", summary="Get species' XYZ geometry file")
async def getSpeciesXyz(
    iri: str, xyz_manager: Annotated[XYZManager, Depends(get_xyz_manager)]
):
    xyz = xyz_manager.get_from_pubchem([iri])[0]
    if not xyz:
        raise HTTPException(
            status_code=404, detail=f"XYZ file not found for species `{iri}`"
        )
    return Response(content=xyz, media_type="chemical/x-xyz")
