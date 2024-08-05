import logging
from typing import Annotated

from fastapi import APIRouter, Depends

from model.kg.ontozeolite import (
    OntozeoliteZeoliteFrameworkBase,
    OntozeoliteZeoliteFrameworkPartial,
)
from model.web.ontozeolite import (
    ZeoliteFrameworkRequest,
    ZeoliteFrameworkReturnFields,
)
from routers.ontozeolite.common import (
    SCALAR_TOPO_PROP_QUERY_PARAMS,
    UNIT_CELL_QUERY_PARAMS,
    parse_zeolite_framework_request,
    parse_zeolite_framework_return_fields,
)
from services.rdf_stores.ontozeolite import (
    OntozeoliteRDFStore,
    get_ontozeolite_rdfStore,
)


logger = logging.getLogger(__name__)

router = APIRouter()


@router.get(
    "",
    summary="Get zeolite frameworks",
    openapi_extra={
        "parameters": [
            *UNIT_CELL_QUERY_PARAMS,
            *SCALAR_TOPO_PROP_QUERY_PARAMS,
        ]
    },
    response_model=list[OntozeoliteZeoliteFrameworkPartial],
    response_model_exclude_none=True,
)
async def getZeoliteFrameworksPartial(
    framework_req: Annotated[
        ZeoliteFrameworkRequest, Depends(parse_zeolite_framework_request)
    ],
    return_fields: Annotated[
        ZeoliteFrameworkReturnFields, Depends(parse_zeolite_framework_return_fields)
    ],
    ontozeolite_store: Annotated[
        OntozeoliteRDFStore, Depends(get_ontozeolite_rdfStore)
    ],
):
    iris = ontozeolite_store.get_zeolite_framework_IRIs(req=framework_req)
    return ontozeolite_store.get_zeolite_framework_partial_many(
        iris=iris, return_fields=return_fields
    )
