import logging
from typing import Annotated

from fastapi import APIRouter, Depends

from model.kg.ontospecies import (
    OntospeciesSpeciesPartial,
)
from model.web.ontospecies import SpeciesReturnFields, SpeciesRequest
from routers.ontospecies.common import (
    OPENAPI_SPECIES_IDENTIFIER_QUERY_PARAMS,
    OPENAPI_SPECIES_PROPERTY_QUERY_PARAMS,
    parse_return_fields,
    parse_species_request,
)
from services.rdf_stores.ontospecies import (
    OntospeciesRDFStore,
    get_ontospecies_rdfStore,
)


logger = logging.getLogger(__name__)

router = APIRouter()


@router.get(
    "/",
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
    return_fields: Annotated[SpeciesReturnFields, Depends(parse_return_fields)],
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
