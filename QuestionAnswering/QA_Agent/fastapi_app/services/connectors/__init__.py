from functools import cache
import logging
import time
from typing import Dict, List, Optional


from model.qa import QAStep
from services.core.func_call import IFuncCaller
from .agent_connector import AgentConnectorBase


logger = logging.getLogger(__name__)



class AgentConnectorMediator:
    def __init__(
        self,
        func_call_predictor: IFuncCaller,
        domain2connectors: Dict[str, List[AgentConnectorBase]],
    ):
        self.func_call_predictor = func_call_predictor
        self.domain2connectors = domain2connectors
        # TODO: validate that no connectors have the same methodname
        self.funcname2connector = {
            methodname: connector
            for connectors in domain2connectors.values()
            for connector in connectors
            for methodname in connector.name2method.keys()
        }

    @cache
    def _get_funcs(self, domain: str):
        return [
            func
            for connector in self.domain2connectors[domain]
            for func in connector.funcs
        ]

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
