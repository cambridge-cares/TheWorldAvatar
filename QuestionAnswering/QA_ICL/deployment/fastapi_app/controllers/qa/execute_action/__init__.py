from functools import cache
from typing import Annotated

from fastapi import Depends

from services.example_store.model import DataRetrievalAction, FuncAction, SparqlAction

from .func import FuncActionExecutor, get_funcAction_executor
from .sparql import SparqlActionExecutor, get_sparqlAction_executor


class ActionExecMediator:
    def __init__(
        self, sparql_executor: SparqlActionExecutor, func_executor: FuncActionExecutor
    ):
        self.sparql_executor = sparql_executor
        self.func_executor = func_executor

    def exec(self, action: DataRetrievalAction):
        if isinstance(action, SparqlAction):
            return self.sparql_executor.exec(action)
        elif isinstance(action, FuncAction):
            return self.func_executor.exec(action)
        else:
            raise ValueError("Unsupported: " + str(action))


@cache
def get_actionExec_mediator(
    sparql_executor: Annotated[
        SparqlActionExecutor, Depends(get_sparqlAction_executor)
    ],
    func_executor: Annotated[FuncActionExecutor, Depends(get_funcAction_executor)],
):
    return ActionExecMediator(
        sparql_executor=sparql_executor, func_executor=func_executor
    )
