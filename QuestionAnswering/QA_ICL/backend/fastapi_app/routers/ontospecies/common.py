from typing import Annotated
from fastapi import Depends, Query, Request
from model.kg.ontospecies import (
    SpeciesAttrKey,
    SpeciesIdentifierKey,
    SpeciesPropertyKey,
)
from model.web.comp_op import ComparisonOperator
from model.web.ontospecies import SpeciesReturnFields, SpeciesRequest
from routers.base import RETURN_FIELD_QUERY_KEY
from routers.utils import parse_rhs_colon


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
    return_fields: Annotated[list[str], Query(..., alias=RETURN_FIELD_QUERY_KEY)] = []
):
    return_fields_set = set(return_fields)
    return SpeciesReturnFields(
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
    )


OPENAPI_SPECIES_PROPERTY_QUERY_PARAMS = [
    {
        "in": "query",
        "name": key.value,
        "schema": {"type": "array", "items": {"type": "string"}},
        "description": "RHS colon filters e.g. `eq:100`, `lte:200`",
    }
    for key in SpeciesPropertyKey
]


SPECIES_IDENTIFIER_KEY_TO_LABEL = {
    SpeciesIdentifierKey.CID: "CID",
    SpeciesIdentifierKey.CHEBI_ID: "ChEBI ID",
    SpeciesIdentifierKey.IUPAC_NAME: "IUPAC name",
    SpeciesIdentifierKey.INCHI: "InChI",
    SpeciesIdentifierKey.INCHI_KEY: "InChIKey",
    SpeciesIdentifierKey.MOLECULAR_FORMULA: "molecular formula",
    SpeciesIdentifierKey.SMILES: "SMILES string",
}
OPENAPI_SPECIES_IDENTIFIER_QUERY_PARAMS = [
    {
        "in": "query",
        "name": key.value,
        "schema": {"type": "string"},
        "description": label,
    }
    for key, label in SPECIES_IDENTIFIER_KEY_TO_LABEL.items()
]
