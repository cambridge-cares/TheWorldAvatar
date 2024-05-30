from functools import cache
from typing import Annotated

from fastapi import Depends

from services.kg import KgClient, get_ontokin_bgClient
from services.processs_response.augment_node import NodeDataRetriever
from services.processs_response.ontokin.rxn import get_kinetic_model_data
from services.processs_response.ontokin.species import (
    get_thermo_model_data,
    get_transport_model_data,
)


@cache
def get_ontokin_nodeDataRetriever(
    bg_client: Annotated[KgClient, Depends(get_ontokin_bgClient)]
):
    return NodeDataRetriever(
        kg_client=bg_client,
        type2getter={
            "ThermoModel": get_thermo_model_data,
            "TransportModel": get_transport_model_data,
            "KineticModel": get_kinetic_model_data,
        },
    )
