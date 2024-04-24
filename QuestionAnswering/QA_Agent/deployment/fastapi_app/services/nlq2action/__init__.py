import time
from typing import Annotated, List

from fastapi import Depends

from model.qa import QAStep
from services.nlq2action.retrieve import (
    Nlq2ActionRetriever,
    get_sg_nlq2action_retriever,
)
from services.support_data import DataSupporter
from .execute_action import ActionExecMediator, get_actionExec_mediator
from .translate import Nlq2ActionTranslator, get_nlq2action_translator


class Nlq2Action2Data(DataSupporter):
    def __init__(
        self,
        retriever: Nlq2ActionRetriever,
        translator: Nlq2ActionTranslator,
        executor: ActionExecMediator,
    ):
        self.retriever = retriever
        self.translator = translator
        self.executor = executor

    def query(self, query: str):
        steps: List[QAStep] = []

        timestamp = time.time()
        examples = self.retriever.retrieve_examples(nlq=query, k=10)
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="retrieve_examples",
                arguments=dict(nlq=query, k=10),
                results=examples[:3] + ["..."],
                latency=latency,
            )
        )

        timestamp = time.time()
        action = self.translator.translate(nlq=query, examples=examples)
        latency = time.time() - timestamp
        steps.append(
            QAStep(
                action="nlq2action", arguments=query, results=action, latency=latency
            )
        )

        _steps, data = self.executor.exec(action)
        steps.extend(_steps)

        return steps, data


def get_singapore_nlq2action2data(
    retriever: Annotated[Nlq2ActionRetriever, Depends(get_sg_nlq2action_retriever)],
    translator: Annotated[Nlq2ActionTranslator, Depends(get_nlq2action_translator)],
    executor: Annotated[ActionExecMediator, Depends(get_actionExec_mediator)],
):
    return Nlq2Action2Data(
        retriever=retriever, translator=translator, executor=executor
    )
