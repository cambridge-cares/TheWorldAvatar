from functools import cache
from typing import Annotated

from fastapi import Depends

from services.core.func_call import IFuncCaller, get_func_caller
from services.connectors import AgentConnectorMediator
from services.connectors.sg_factories import (
    SGFactoriesAgentConnector,
    get_sg_factories_agent_connector,
)
from services.connectors.sg_land_lots import (
    SGLandLotsAgentConnector,
    get_sg_land_lots_agent_connector,
)
from services.connectors.ontospecies import (
    OntoSpeciesAgentConnector,
    get_ontospecies_agent_connector,
)


@cache
def get_marie_mediator(
    func_call_predictor: Annotated[IFuncCaller, Depends(get_func_caller)],
    ontospecies_agent_connector: Annotated[
        OntoSpeciesAgentConnector,
        Depends(get_ontospecies_agent_connector),
    ],
):
    chemistry_connectors = [ontospecies_agent_connector]

    return AgentConnectorMediator(
        func_call_predictor=func_call_predictor,
        domain2connectors=dict(chemistry=chemistry_connectors),
    )


@cache
def get_zaha_mediator(
    func_call_predictor: Annotated[IFuncCaller, Depends(get_func_caller)],
    sg_land_lots_agent_connector: Annotated[
        SGLandLotsAgentConnector,
        Depends(get_sg_land_lots_agent_connector),
    ],
    sg_factories_agent_connector: Annotated[
        SGFactoriesAgentConnector, Depends(get_sg_factories_agent_connector)
    ],
):
    singapore_connectors = [sg_land_lots_agent_connector, sg_factories_agent_connector]

    return AgentConnectorMediator(
        func_call_predictor=func_call_predictor,
        domain2connectors=dict(singapore=singapore_connectors),
    )
