from typing import Any, Callable, Dict
from model.qa import QAData
from services.nlq2action.execute_action.model import FuncAction


class FuncActionExecutor:
    def __init__(self, name2func: Dict[str, Callable[..., QAData]]):
        self.name2func = name2func

    def exec(self, action: FuncAction):
        return self.name2func[action.name](action.args)