from typing import Annotated, Tuple

from fastapi import Depends
from services.nlq2action.execute_action.model import FuncAction
from .base import Name2Func
from .sg_dispersion import SGDispersionFuncExecutor, get_sgDispersion_funcExecutor


class FuncActionExecutor:
    def __init__(self, name2func_instances: Tuple[Name2Func, ...]):
        self.name2func = {
            name: func
            for instance in name2func_instances
            for name, func in instance.get_name2func().items()
        }

    def exec(self, action: FuncAction):
        return self.name2func[action.name](**action.args)


def get_funcAction_executor(
    sg_dispersion_name2func: Annotated[
        SGDispersionFuncExecutor, Depends(get_sgDispersion_funcExecutor)
    ]
):
    return FuncActionExecutor(name2func_instances=(sg_dispersion_name2func,))
