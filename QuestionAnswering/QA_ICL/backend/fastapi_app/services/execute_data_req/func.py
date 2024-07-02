from typing import Annotated, Sequence

from fastapi import Depends

from model.nlq2datareq import FuncDataReqForm
from services.funcs.base import Name2Func
from services.funcs.sg_building import SGBuildingFuncExecutor, get_sgBuilding_funcExec
from services.funcs.sg_dispersion import (
    SGDispersionFuncExecutor,
    get_sgDispersion_funcExec,
)
from services.funcs.sg_carpark import SGCarparkFuncExecutor, get_sgCarpark_funcExec


class FuncDataReqExecutor:
    def __init__(self, name2func_instances: Sequence[Name2Func]):
        self.name2func = {
            name: func
            for instance in name2func_instances
            for name, func in instance.get_name2func().items()
        }

    def exec(
        self,
        entity_bindings: dict[str, list[str]],
        const_bindings: dict[str, object],
        req_form: FuncDataReqForm,
        vis_vars: list[str]
    ):
        data = self.name2func[req_form.name](**entity_bindings, **const_bindings)
        artifact = data
        vis_var2iris: dict[str, list[str]] = dict()
        return data, artifact, vis_var2iris


def get_funcReq_executor(
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
    return FuncDataReqExecutor(
        name2func_instances=(
            sg_building_func_exec,
            sg_dispersion_func_exec,
            sg_carpark_func_exec,
        )
    )
