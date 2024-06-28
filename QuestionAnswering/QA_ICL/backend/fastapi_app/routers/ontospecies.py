import logging
import re
from typing import Annotated

from fastapi import APIRouter, Depends, Query, Request

from model.comp_op import ComparisonOperator
from model.kg.ontospecies import SpeciesPropertyKey
from model.ontospecies import SpeciesRequest
from services.rdf_stores.ontospecies import (
    OntospeciesRDFStore,
    get_ontospecies_rdfStore,
)


logger = logging.getLogger(__name__)

router = APIRouter()


@router.get("/chemical-classes", summary="Get all chemical classes")
async def getChemicalClasses(
    ontospecies_store: Annotated[OntospeciesRDFStore, Depends(get_ontospecies_rdfStore)]
):
    return ontospecies_store.get_chemical_classes_all()


def parse_rhs_colon(val: str):
    operator, operand = val.split(":", maxsplit=1)
    return ComparisonOperator(operator), float(operand)


CHEMICAL_CLASS_QUERY_KEY = "chemical-class"
USE_QUERY_KEY = "use"

_CAMEL_CASE_PATTERN = re.compile(r"(?<!^)(?=[A-Z])")
SPECIES_PROPERTY_QUERY_KEYS = {
    _CAMEL_CASE_PATTERN.sub("-", key.value).lower(): key for key in SpeciesPropertyKey
}


async def parse_species_request(req: Request):
    return SpeciesRequest(
        chemical_class=req.query_params.getlist(CHEMICAL_CLASS_QUERY_KEY),
        use=req.query_params.getlist(USE_QUERY_KEY),
        property={
            py_key: [
                parse_rhs_colon(val) for val in req.query_params.getlist(query_key)
            ]
            for query_key, py_key in SPECIES_PROPERTY_QUERY_KEYS.items()
        },
    )


@router.get(
    "/species",
    summary="Find species",
    openapi_extra={
        "parameters": [
            {
                "in": "query",
                "name": CHEMICAL_CLASS_QUERY_KEY,
                "schema": {"type": "string"},
            },
            {"in": "query", "name": USE_QUERY_KEY, "schema": {"type": "string"}},
            *(
                {
                    "in": "query",
                    "name": name,
                    "schema": {
                        "type": "string",
                    },
                    "description": "RHS colon filter e.g. `eq:100`, `lte:200`",
                }
                for name in SPECIES_PROPERTY_QUERY_KEYS.keys()
            ),
        ]
    },
)
async def getSpecies(
    species_req: Annotated[SpeciesRequest, Depends(parse_species_request)],
    ontospecies_store: Annotated[
        OntospeciesRDFStore, Depends(get_ontospecies_rdfStore)
    ],
):
    return ontospecies_store.find_species(species_req)
