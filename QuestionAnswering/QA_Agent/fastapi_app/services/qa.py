from functools import cache
from typing import Annotated

from fastapi import Depends

from services.connectors.sg_data_centres import (
    SGDataCentresAgentConnector,
    get_sgDataCentres_agentConnector,
)
from services.core.func_call import IFuncCaller, get_func_caller
from services.connectors import AgentConnectorMediator
from services.connectors.retrieval import (
    RetrievalAgentConnector,
    get_retrieval_agentConnector,
)
from services.connectors.sg_factories import (
    SGFactoriesAgentConnector,
    get_sgFactories_agentConnector,
)
from services.connectors.sg_land_lots import (
    SGLandLotsAgentConnector,
    get_sgLandLots_agentConnector,
)
from services.connectors.ontospecies import (
    OntoSpeciesAgentConnector,
    get_ontospecies_agent_connector,
)


@cache
def get_mediator(
    func_call_predictor: Annotated[IFuncCaller, Depends(get_func_caller)],
    retrieval_agent_connector: Annotated[
        RetrievalAgentConnector, Depends(get_retrieval_agentConnector)
    ],
    ontospecies_agent_connector: Annotated[
        OntoSpeciesAgentConnector,
        Depends(get_ontospecies_agent_connector),
    ],
    sg_land_lots_agent_connector: Annotated[
        SGLandLotsAgentConnector,
        Depends(get_sgLandLots_agentConnector),
    ],
    sg_factories_agent_connector: Annotated[
        SGFactoriesAgentConnector, Depends(get_sgFactories_agentConnector)
    ],
    sg_data_centres_agent_connector: Annotated[
        SGDataCentresAgentConnector, Depends(get_sgDataCentres_agentConnector)
    ],
):
    common_connectors = [retrieval_agent_connector]
    chemistry_connectors = [ontospecies_agent_connector]
    singapore_connectors = [
        sg_land_lots_agent_connector,
        sg_factories_agent_connector,
        sg_data_centres_agent_connector,
    ]

    return AgentConnectorMediator(
        func_call_predictor=func_call_predictor,
        common_connectors=common_connectors,
        domain2connectors=dict(
            chemistry=chemistry_connectors, singapore=singapore_connectors
        ),
    )
