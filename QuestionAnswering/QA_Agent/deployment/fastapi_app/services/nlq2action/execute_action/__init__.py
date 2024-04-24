from functools import cache
from typing import Annotated

from fastapi import Depends
from .model import ActionBase, SparqlAction

from .sparql import SparqlActionExecutor, get_sparqlAction_executor


class ActionExecMediator:
    def __init__(self, sparql_executor: SparqlActionExecutor):
        self.sparql_executor = sparql_executor

    def exec(self, action: ActionBase):
        if isinstance(action, SparqlAction):
            return self.sparql_executor.exec(action)
        else:
            raise ValueError("Unsupported: " + str(action))


@cache
def get_actionExec_mediator(
    sparql_executor: Annotated[SparqlActionExecutor, Depends(get_sparqlAction_executor)]
):
    return ActionExecMediator(sparql_executor=sparql_executor)
