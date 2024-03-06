from functools import cache
import logging
from typing import Annotated, List

from fastapi import Depends
from services.nearest_neighbor import NNRetriever, get_nn_retriever
from services.connector.ontospecies import OntoSpeciesAgent
from services.connector.agent import IAgent
from services.func_call import IFuncCaller, get_func_caller


logger = logging.getLogger(__name__)


class AgentConnector:
    def __init__(self, func_call_predictor: IFuncCaller, agents: List[IAgent]):
        self.func_call_predictor = func_call_predictor
        self.funcs = [x for agent in agents for x in agent.get_funcs()]
        self.funcname2agent = {
            method_name: agent
            for agent in agents
            for method_name in agent.get_name2method().keys()
        }

    def query(self, query: str):
        logger.info("Predicting function to call...")
        func_name, func_args = self.func_call_predictor.predict(
            funcs=self.funcs, query=query
        )
        logger.info(
            "Predicted function: {name}({args})".format(name=func_name, args=func_args)
        )
        return self.funcname2agent[func_name].exec(
            method_name=func_name, args=func_args
        )


@cache
def get_agents(nn_retriever: Annotated[NNRetriever, Depends(get_nn_retriever)]):
    return tuple([OntoSpeciesAgent(nn_retriever)])


@cache
def get_agent_connector(
    func_call_predictor: Annotated[
        IFuncCaller, Depends(get_func_caller)
    ],
    agents: Annotated[List[IAgent], Depends(get_agents)],
):
    return AgentConnector(func_call_predictor=func_call_predictor, agents=agents)
