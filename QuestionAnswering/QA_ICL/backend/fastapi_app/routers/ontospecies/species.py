import logging
from typing import Annotated

from fastapi import APIRouter, Depends, HTTPException, Query, Request, Response

from model.kg.ontospecies import (
    OntospeciesSpecies,
    SpeciesAttrKey,
    SpeciesIdentifierKey,
    SpeciesPropertyKey,
)
from model.web.comp_op import ComparisonOperator
from model.web.ontospecies import SpeciesReqReturnFields, SpeciesRequest
from services.mol_vis.xyz import XYZManager, get_xyz_manager
from services.rdf_stores.ontospecies import (
    OntospeciesRDFStore,
    get_ontospecies_rdfStore,
)
from routers.utils import parse_rhs_colon


logger = logging.getLogger(__name__)

router = APIRouter()

RETURN_FIELD_QUERY_KEY = "ReturnField"

SPECIES_IDENTIFIER_KEY_TO_LABEL = {
    SpeciesIdentifierKey.CID: "CID",
    SpeciesIdentifierKey.CHEBI_ID: "ChEBI ID",
    SpeciesIdentifierKey.IUPAC_NAME: "IUPAC name",
    SpeciesIdentifierKey.INCHI: "InChI",
    SpeciesIdentifierKey.INCHI_KEY: "InChIKey",
    SpeciesIdentifierKey.MOLECULAR_FORMULA: "molecular formula",
    SpeciesIdentifierKey.SMILES: "SMILES string",
}


async def parse_identifier_query_params(req: Request):
    return {
        key: req.query_params[key.value]
        for key in SpeciesIdentifierKey
        if key.value in req.query_params
    }


async def parse_property_query_params(req: Request):
    return {
        key: [parse_rhs_colon(val) for val in req.query_params.getlist(key.value)]
        for key in SpeciesPropertyKey
        if key.value in req.query_params
    }


async def parse_return_fields(
    return_field: Annotated[list[str], Query(..., alias=RETURN_FIELD_QUERY_KEY)] = []
):
    if not return_field:
        return None

    return_fields_set = set(return_field)
    return SpeciesReqReturnFields(
        alt_label=SpeciesAttrKey.ALT_LABEL in return_fields_set,
        chemical_class=SpeciesAttrKey.CHEMICAL_CLASS in return_fields_set,
        use=SpeciesAttrKey.USE in return_fields_set,
        identifier=[
            key for key in SpeciesIdentifierKey if key.value in return_fields_set
        ],
        property=[key for key in SpeciesPropertyKey if key.value in return_fields_set],
    )


async def parse_species_request(
    identifier: Annotated[
        dict[SpeciesIdentifierKey, str], Depends(parse_identifier_query_params)
    ],
    property: Annotated[
        dict[SpeciesPropertyKey, list[tuple[ComparisonOperator, float]]],
        Depends(parse_property_query_params),
    ],
    return_fields: Annotated[
        SpeciesReqReturnFields | None, Depends(parse_return_fields)
    ],
    chemical_class: Annotated[
        list[str], Query(..., alias=SpeciesAttrKey.CHEMICAL_CLASS)
    ] = [],
    use: Annotated[list[str], Query(..., alias=SpeciesAttrKey.USE)] = [],
):
    return SpeciesRequest(
        chemical_class=chemical_class,
        use=use,
        identifier=identifier,
        property=property,
        return_fields=return_fields,
    )


@router.get(
    "/",
    summary="Get species",
    openapi_extra={
        "parameters": [
            *(
                {
                    "in": "query",
                    "name": key.value,
                    "schema": {"type": "array", "items": {"type": "string"}},
                    "description": "RHS colon filters e.g. `eq:100`, `lte:200`",
                }
                for key in SpeciesPropertyKey
            ),
            *(
                {
                    "in": "query",
                    "name": key.value,
                    "schema": {"type": "string"},
                    "description": label,
                }
                for key, label in SPECIES_IDENTIFIER_KEY_TO_LABEL.items()
            ),
        ]
    },
    response_model=list[OntospeciesSpecies],
)
async def getSpecies(
    species_req: Annotated[SpeciesRequest, Depends(parse_species_request)],
    return_fields: Annotated[
        SpeciesReqReturnFields | None, Depends(parse_return_fields)
    ],
    ontospecies_store: Annotated[
        OntospeciesRDFStore, Depends(get_ontospecies_rdfStore)
    ],
):
    iris = ontospecies_store.get_species_IRIs(species_req)

    if return_fields is None:
        return_fields = SpeciesReqReturnFields()

    return [
        x
        for x in ontospecies_store.get_species_fields(iris, return_fields=return_fields)
        if x
    ]


class XYZResponse(Response):
    media_type = "chemical/x-xyz"


@router.get(
    "/{iri:path}/xyz",
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
    "/{iri:path}",
    summary="Get species",
    response_model_exclude_none=True,
    response_model=OntospeciesSpecies,
)
async def getSpeciesOne(
    iri: str,
    return_fields: Annotated[
        SpeciesReqReturnFields | None, Depends(parse_return_fields)
    ],
    ontospecies_store: Annotated[
        OntospeciesRDFStore, Depends(get_ontospecies_rdfStore)
    ],
):
    species = ontospecies_store.get_species_fields(
        iris=[iri], return_fields=return_fields
    )[0]
    if species is None:
        raise HTTPException(
            status_code=404, detail=f'No species is found with IRI "{iri}"'
        )
    return species
