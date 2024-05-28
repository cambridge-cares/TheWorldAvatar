from typing import Annotated, Tuple

from fastapi import Depends
from services.example_store.model import FuncAction
from services.funcs.base import Name2Func
from services.funcs.sg_building import SGBuildingFuncExecutor, get_sgBuilding_funcExec
from services.funcs.sg_dispersion import (
    SGDispersionFuncExecutor,
    get_sgDispersion_funcExec,
)
from services.funcs.sg_carpark import SGCarparkFuncExecutor, get_sgCarpark_funcExec


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
    sg_building_func_exec: Annotated[
        SGBuildingFuncExecutor, Depends(get_sgBuilding_funcExec)
    ],
    sg_dispersion_func_exec: Annotated[
        SGDispersionFuncExecutor, Depends(get_sgDispersion_funcExec)
    ],
    sg_carpark_func_exec: Annotated[
        SGCarparkFuncExecutor, Depends(get_sgCarpark_funcExec)
    ],
):
    return FuncActionExecutor(
        name2func_instances=(
            sg_building_func_exec,
            sg_dispersion_func_exec,
            sg_carpark_func_exec,
        )
    )
