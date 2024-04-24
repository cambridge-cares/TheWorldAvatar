from .model import ActionBase, SparqlAction

from .sparql import SparqlActionExecutor


class ActionExecMediator:
    def __init__(self, sparql_executor: SparqlActionExecutor):
        self.sparql_executor = sparql_executor

    def exec(self, action: ActionBase):
        if isinstance(action, SparqlAction):
            return self.sparql_executor.exec(action)
        else:
            raise ValueError("Unsupported: " + str(action))
