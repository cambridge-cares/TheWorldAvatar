import logging
from typing import Annotated

from fastapi import APIRouter, Depends, HTTPException, Query, Request, Response

from model.kg.ontospecies import (
    OntospeciesChemicalClass,
    OntospeciesSpeciesBase,
    OntospeciesUse,
    SpeciesIdentifierKey,
    SpeciesPropertyKey,
)
from model.web.ontospecies import SpeciesRequest
from routers.utils import parse_rhs_colon
from services.mol_vis.xyz import XYZManager, get_xyz_manager
from services.rdf_stores.ontospecies import (
    OntospeciesRDFStore,
    get_ontospecies_rdfStore,
)
from utils.str import CAMEL_CASE_PATTERN


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


SPECIES_PROPERTY_QUERY_KEYS = {
    key: CAMEL_CASE_PATTERN.sub("-", key.value).lower() for key in SpeciesPropertyKey
}
SPECIES_IDENTIFIER_QUERY_KEYS = {
    SpeciesIdentifierKey.CID: "cid",
    SpeciesIdentifierKey.CHEBI_ID: "chebi-id",
    SpeciesIdentifierKey.IUPAC_NAME: "iupac-name",
    SpeciesIdentifierKey.INCHI: "inchi",
    SpeciesIdentifierKey.INCHI_KEY: "inchi-key",
    SpeciesIdentifierKey.MOLECULAR_FORMULA: "molecular-formula",
    SpeciesIdentifierKey.SMILES: "smiles-string",
}
SPECIES_IDENTIFIER_KEY_TO_LABEL = {
    SpeciesIdentifierKey.CID: "CID",
    SpeciesIdentifierKey.CHEBI_ID: "ChEBI ID",
    SpeciesIdentifierKey.IUPAC_NAME: "IUPAC name",
    SpeciesIdentifierKey.INCHI: "InChI",
    SpeciesIdentifierKey.INCHI_KEY: "InChIKey",
    SpeciesIdentifierKey.MOLECULAR_FORMULA: "molecular formula",
    SpeciesIdentifierKey.SMILES: "SMILES string",
}


async def parse_species_request(
    req: Request,
    chemical_class: Annotated[list[str], Query(..., alias="chemical-class")] = [],
    use: Annotated[list[str], Query()] = [],
):
    return SpeciesRequest(
        chemical_class=chemical_class,
        use=use,
        identifier={
            py_key: req.query_params[query_key]
            for py_key, query_key in SPECIES_IDENTIFIER_QUERY_KEYS.items()
            if query_key in req.query_params
        },
        property={
            py_key: [
                parse_rhs_colon(val) for val in req.query_params.getlist(query_key)
            ]
            for py_key, query_key in SPECIES_PROPERTY_QUERY_KEYS.items()
        },
    )


@router.get(
    "/species",
    summary="Get species",
    openapi_extra={
        "parameters": [
            *(
                {
                    "in": "query",
                    "name": name,
                    "schema": {"type": "array", "items": {"type": "string"}},
                    "description": "RHS colon filters e.g. `eq:100`, `lte:200`",
                }
                for name in SPECIES_PROPERTY_QUERY_KEYS.values()
            ),
            *(
                {
                    "in": "query",
                    "name": SPECIES_IDENTIFIER_QUERY_KEYS[key],
                    "schema": {"type": "string"},
                    "description": label,
                }
                for key, label in SPECIES_IDENTIFIER_KEY_TO_LABEL.items()
            ),
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
    return ontospecies_store.get_species_base(species_req)


# TODO
# @router.get(
#     "/species/{iri:path}",
#     summary="Get species",
#     response_class=OntospeciesSpecies
# )
# async def getSpeciesOne(iri: str, ontospecies_store: Annotated[OntospeciesRDFStore, Depends(get_ontospecies_rdfStore)]):
#     pass


class XYZResponse(Response):
    media_type = "chemical/x-xyz"


@router.get(
    "/species/{iri:path}/xyz",
    summary="Get species' XYZ geometry file",
    response_class=XYZResponse,
)
async def getSpeciesXyz(
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
