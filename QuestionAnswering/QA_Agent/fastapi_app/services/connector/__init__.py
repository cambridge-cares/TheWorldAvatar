from functools import cache
import logging
import time
from typing import Annotated, Dict, List, Optional

from fastapi import Depends

from model.qa import QAStep
from services.func_call import IFuncCaller, get_func_caller
from .agent_connector import IAgentConnector
from .ontospecies import (
    OntoSpeciesAgentConnector,
    get_ontospecies_agent_connector,
)
from .singapore import (
    SingaporeLandLotsAgentConnector,
    get_singapore_land_lots_agent_connector,
)


logger = logging.getLogger(__name__)


class AgentConnectorMediator:
    def __init__(
        self,
        func_call_predictor: IFuncCaller,
        domain2connector: Dict[str, IAgentConnector],
    ):
        self.func_call_predictor = func_call_predictor
        self.domain2connector = domain2connector
        self.funcname2connector = {
            methodname: connector
            for connector in domain2connector.values()
            for methodname in connector.get_name2method().keys()
        }

    @cache
    def _get_funcs(self, domain: Optional[str]):
        if domain is None:
            return [
                func
                for agent in self.domain2connector.values()
                for func in agent.get_funcs()
            ]
        return self.domain2connector[domain].get_funcs()

    def query(self, query: str, domain: Optional[str] = None):
        steps: List[QAStep] = []

        logger.info("Predicting function to call...")
        timestamp = time.time()
        func_name, func_args = self.func_call_predictor.predict(
            funcs=self._get_funcs(domain), query=query
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

        qa_mode, connector_steps, data = self.funcname2connector[func_name].exec(
            method_name=func_name, args=func_args
        )

        return qa_mode, steps + connector_steps, data


@cache
def get_chemistry_mediator(
    func_call_predictor: Annotated[IFuncCaller, Depends(get_func_caller)],
    ontospecies_agent_connector: Annotated[
        OntoSpeciesAgentConnector,
        Depends(get_ontospecies_agent_connector),
    ],
):
    return AgentConnectorMediator(
        func_call_predictor=func_call_predictor,
        domain2connector=dict(ontospecies=ontospecies_agent_connector),
    )


@cache
def get_cities_mediator(
    func_call_predictor: Annotated[IFuncCaller, Depends(get_func_caller)],
    singapore_land_lots_agent_connector: Annotated[
        SingaporeLandLotsAgentConnector,
        Depends(get_singapore_land_lots_agent_connector),
    ],
):
    return AgentConnectorMediator(
        func_call_predictor=func_call_predictor,
        domain2connector=dict(singapore_land_lots=singapore_land_lots_agent_connector),
    )
