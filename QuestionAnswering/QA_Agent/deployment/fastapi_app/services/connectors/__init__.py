from functools import cache
import logging
import time
from typing import List, Annotated
from functools import cache

from fastapi import Depends

from model.qa import QAStep
from services.core.func_call import IFuncCaller, get_func_caller
from services.support_data import DataSupporter
from .agent_connector import AgentConnectorBase
from .retrieval import (
    RetrievalAgentConnector,
    get_retrieval_agentConnector,
)
from .sg_buildings import (
    SGBuildingsAgentConnector,
    get_sgBuildlings_agentConnector,
)
from .sg_ships import (
    SGShipsAgentConnector,
    get_sgShips_agentConnector,
)
from .sg_data_centres import (
    SGDataCentresAgentConnector,
    get_sgDataCentres_agentConnector,
)
from .sg_factories import (
    SGFactoriesAgentConnector,
    get_sgFactories_agentConnector,
)
from .sg_land_lots import (
    SGLandLotsAgentConnector,
    get_sgLandLots_agentConnector,
)
from .sg_dispersion import (
    SGDispersionAgentConnector,
    get_sgDispersion_agentConnector,
)
from .ontospecies import (
    OntoSpeciesAgentConnector,
    get_ontospecies_agentConnector,
)
from .sg_carparks import (
    SGCarparksAgentConnector,
    get_sgCarparks_agentConnector,
)


logger = logging.getLogger(__name__)


class AgentConnectorMediator(DataSupporter):
    def __init__(
        self,
        func_call_predictor: IFuncCaller,
        connectors: List[AgentConnectorBase],
    ):
        self.func_call_predictor = func_call_predictor
        # TODO: validate that no connectors have the same methodname
        self.funcname2connector = {
            methodname: connector
            for connector in connectors
            for methodname in connector.name2method.keys()
        }
        self.funcs = [func for connector in connectors for func in connector.funcs]

    def query(self, query: str):
        steps: List[QAStep] = []

        logger.info("Predicting function to call...")
        timestamp = time.time()
        func_name, func_args = self.func_call_predictor.predict(
            funcs=self.funcs, query=query
        )
        latency = time.time() - timestamp
        logger.info(
            "Predicted function: {name}({args})".format(name=func_name, args=func_args)
        )
        steps.append(
            QAStep(
                action="predict_func_call",
                arguments=query,
                results=dict(func_name=func_name, args=func_args),
                latency=latency,
            )
        )

        connector_steps, data = self.funcname2connector[func_name].exec(
            method_name=func_name, args=func_args
        )

        return steps + connector_steps, data


@cache
def get_chemistry_agentMediator(
    func_call_predictor: Annotated[IFuncCaller, Depends(get_func_caller)],
    retrieval_agent_connector: Annotated[
        RetrievalAgentConnector, Depends(get_retrieval_agentConnector)
    ],
    ontospecies_agent_connector: Annotated[
        OntoSpeciesAgentConnector,
        Depends(get_ontospecies_agentConnector),
    ],
):
    return AgentConnectorMediator(
        func_call_predictor=func_call_predictor,
        connectors=[retrieval_agent_connector, ontospecies_agent_connector],
    )


@cache
def get_singapore_agentMediator(
    func_call_predictor: Annotated[IFuncCaller, Depends(get_func_caller)],
    retrieval_agent_connector: Annotated[
        RetrievalAgentConnector, Depends(get_retrieval_agentConnector)
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
    sg_dispersion_agent_connector: Annotated[
        SGDispersionAgentConnector, Depends(get_sgDispersion_agentConnector)
    ],
    sg_ships_agent_connector: Annotated[
        SGShipsAgentConnector, Depends(get_sgShips_agentConnector)
    ],
    sg_carparks_agent_connector: Annotated[
        SGCarparksAgentConnector, Depends(get_sgCarparks_agentConnector)
    ],
    sg_buildings_agent_connector: Annotated[
        SGBuildingsAgentConnector, Depends(get_sgBuildlings_agentConnector)
    ],
):
    return AgentConnectorMediator(
        func_call_predictor=func_call_predictor,
        connectors=[
            retrieval_agent_connector,
            sg_land_lots_agent_connector,
            sg_factories_agent_connector,
            sg_data_centres_agent_connector,
            sg_dispersion_agent_connector,
            sg_ships_agent_connector,
            sg_carparks_agent_connector,
            sg_buildings_agent_connector,
        ],
    )
