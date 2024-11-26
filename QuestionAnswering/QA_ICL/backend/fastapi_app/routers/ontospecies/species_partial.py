import logging
from typing import Annotated

from fastapi import APIRouter, Depends, Query

from model.kg.ontospecies import (
    OntospeciesSpeciesPartial,
    SpeciesAttrKey,
    SpeciesIdentifierKey,
    SpeciesPropertyKey,
)
from model.web.ontospecies import SpeciesReturnFields, SpeciesRequest
from routers.base import RETURN_FIELD_QUERY_KEY
from routers.ontospecies.common import (
    OPENAPI_SPECIES_IDENTIFIER_QUERY_PARAMS,
    OPENAPI_SPECIES_PROPERTY_QUERY_PARAMS,
    parse_species_request,
)
from services.rdf_stores.ontospecies import (
    OntospeciesRDFStore,
    get_ontospecies_rdfStore,
)


logger = logging.getLogger(__name__)

router = APIRouter()


async def parse_species_return_fields(
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


@router.get(
    "",
    summary="Get species",
    openapi_extra={
        "parameters": [
            *OPENAPI_SPECIES_PROPERTY_QUERY_PARAMS,
            *OPENAPI_SPECIES_IDENTIFIER_QUERY_PARAMS,
        ]
    },
    response_model=list[OntospeciesSpeciesPartial],
    response_model_exclude_none=True,
)
async def getSpeciesPartial(
    species_req: Annotated[SpeciesRequest, Depends(parse_species_request)],
    return_fields: Annotated[SpeciesReturnFields, Depends(parse_species_return_fields)],
    ontospecies_store: Annotated[
        OntospeciesRDFStore, Depends(get_ontospecies_rdfStore)
    ],
):
    iris = ontospecies_store.get_species_IRIs(species_req)

    return [
        x
        for x in ontospecies_store.get_species_partial_many(
            iris, return_fields=return_fields
        )
        if x
    ]
