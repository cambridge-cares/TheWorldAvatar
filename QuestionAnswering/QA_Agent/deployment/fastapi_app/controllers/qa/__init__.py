import logging
import time
from typing import Annotated, List

from fastapi import Depends

from controllers.qa.model import QAStep
from controllers.qa.retrieve import (
    Nlq2ActionRetriever,
    get_nlq2action_retriever,
)
from .execute_action import ActionExecMediator, get_actionExec_mediator
from .translate import Nlq2ActionTranslator, get_nlq2action_translator


logger = logging.getLogger(__name__)


class DataSupporter:
    def __init__(
        self,
        retriever: Nlq2ActionRetriever,
        translator: Nlq2ActionTranslator,
        executor: ActionExecMediator,
    ):
        self.retriever = retriever
        self.translator = translator
        self.executor = executor

    def query(self, qa_domain: str, query: str):
        steps: List[QAStep] = []

        logger.info("Retrieve examples for: " + query)
        timestamp = time.time()
        examples = self.retriever.retrieve_examples(
            qa_domain=qa_domain, nlq=query, k=10
        )
        latency = time.time() - timestamp
        logger.info("Retrieved examples: " + str(examples))
        steps.append(
            QAStep(
                action="retrieve_examples",
                arguments=dict(nlq=query, k=10),
                results=examples[:3] + ["..."],
                latency=latency,
            )
        )

        timestamp = time.time()
        # KIV: example permutation
        action = self.translator.translate(nlq=query, examples=examples)
        latency = time.time() - timestamp
        logger.info("Predicted action: " + str(action))
        steps.append(
            QAStep(
                action="nlq2action", arguments=query, results=action, latency=latency
            )
        )

        _steps, data = self.executor.exec(action)
        steps.extend(_steps)

        return steps, data


def get_data_supporter(
    retriever: Annotated[Nlq2ActionRetriever, Depends(get_nlq2action_retriever)],
    translator: Annotated[Nlq2ActionTranslator, Depends(get_nlq2action_translator)],
    executor: Annotated[ActionExecMediator, Depends(get_actionExec_mediator)],
):
    return DataSupporter(
        retriever=retriever, translator=translator, executor=executor
    )
