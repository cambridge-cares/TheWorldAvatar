from functools import cache
import logging
import os
import time
from typing import Annotated, List

from fastapi import Depends

from model.qa import QAStep
from services.connector.ontospecies import (
    OntoSpeciesAgentConnector,
    get_ontospecies_agent_connector,
)
from services.connector.singapore import (
    SingaporeLandPlotsAgentConnector,
    get_singapore_land_plots_agent_connector,
)
from services.connector.agent_connector import IAgentConnector
from services.func_call import IFuncCaller, get_func_caller


logger = logging.getLogger(__name__)


class AgentConnectorMediator:
    def __init__(self, func_call_predictor: IFuncCaller, agents: List[IAgentConnector]):
        self.func_call_predictor = func_call_predictor
        self.funcs = [x for agent in agents for x in agent.get_funcs()]
        self.funcname2agent = {
            method_name: agent
            for agent in agents
            for method_name in agent.get_name2method().keys()
        }

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

        connector_steps, data = self.funcname2agent[func_name].exec(
            method_name=func_name, args=func_args
        )

        return steps + connector_steps, data


@cache
def get_chemistry_agents(
    ontospecies_agent_connector: Annotated[
        OntoSpeciesAgentConnector,
        Depends(get_ontospecies_agent_connector),
    ]
):
    return [ontospecies_agent_connector]


@cache
def get_cities_agents(
    singapore_land_plots_agent_connector: Annotated[
        SingaporeLandPlotsAgentConnector,
        Depends(get_singapore_land_plots_agent_connector),
    ]
):
    return [singapore_land_plots_agent_connector]


@cache
def get_superdomain():
    return os.getenv("QA_SUPERDOMAIN", "chemistry")


@cache
def get_agents(
    superdomain: Annotated[str, Depends(get_superdomain)],
    chemistry_agents: Annotated[List[IAgentConnector], Depends(get_chemistry_agents)],
    cities_agents: Annotated[List[IAgentConnector], Depends(get_cities_agents)],
):
    if superdomain == "cities":
        return cities_agents
    return chemistry_agents


def get_agent_connector_mediator(
    func_call_predictor: Annotated[IFuncCaller, Depends(get_func_caller)],
    agents: Annotated[List[IAgentConnector], Depends(get_agents)],
):
    return AgentConnectorMediator(
        func_call_predictor=func_call_predictor, agents=agents
    )
