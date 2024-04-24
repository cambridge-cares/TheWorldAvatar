from typing import Annotated

from fastapi import Depends
from .execute_action import ActionExecMediator, get_actionExec_mediator
from .translate import Nlq2ActionTranslator, get_nlq2action_translator


class SupportingDataRetriever:
    def __init__(self, translator: Nlq2ActionTranslator, executor: ActionExecMediator):
        self.translator = translator
        self.executor = executor

    def retrieve(self, nlq: str):
        action = self.translator.translate(nlq)
        return self.executor.exec(action)


def get_supportData_retriever(
    translator: Annotated[Nlq2ActionTranslator, Depends(get_nlq2action_translator)],
    executor: Annotated[ActionExecMediator, Depends(get_actionExec_mediator)],
):
    return SupportingDataRetriever(translator=translator, executor=executor)
