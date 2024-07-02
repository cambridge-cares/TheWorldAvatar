from functools import cache
from typing import Annotated

from fastapi import Depends

from model.nlq2datareq import (
    DataRequestForm,
    FuncDataReqForm,
    SparqlDataReqForm,
)
from services.execute_data_req.fallback import (
    FallbackDataReqExecutor,
    get_fallback_executor,
)

from .func import FuncDataReqExecutor, get_funcReq_executor
from .sparql import SparqlDataReqExecutor, get_sparqlReq_executor


class DataReqExecutor:
    def __init__(
        self,
        sparql_executor: SparqlDataReqExecutor,
        func_executor: FuncDataReqExecutor,
        fallback_executor: FallbackDataReqExecutor,
    ):
        self.sparql_executor = sparql_executor
        self.func_executor = func_executor
        self.fallback_exeuctor = fallback_executor

    def exec(
        self,
        var2cls: dict[str, str],
        entity_bindings: dict[str, list[str]],
        const_bindings: dict[str, object],
        req_form: DataRequestForm | None,
        vis_vars: list[str],
    ):
        if isinstance(req_form, SparqlDataReqForm):
            return self.sparql_executor.exec(
                var2cls=var2cls,
                entity_bindings=entity_bindings,
                const_bindings=const_bindings,
                req_form=req_form,
                vis_vars=vis_vars,
            )
        elif isinstance(req_form, FuncDataReqForm):
            return self.func_executor.exec(
                var2cls=var2cls,
                entity_bindings=entity_bindings,
                const_bindings=const_bindings,
                req_form=req_form,
                vis_vars=vis_vars,
            )
        else:
            return self.fallback_exeuctor.exec(
                var2cls=var2cls,
                entity_bindings=entity_bindings,
                const_bindings=const_bindings,
                vis_vars=vis_vars,
            )


@cache
def get_dataReq_executor(
    sparql_executor: Annotated[SparqlDataReqExecutor, Depends(get_sparqlReq_executor)],
    func_executor: Annotated[FuncDataReqExecutor, Depends(get_funcReq_executor)],
    fallback_exeuctor: Annotated[
        FallbackDataReqExecutor, Depends(get_fallback_executor)
    ],
):
    return DataReqExecutor(
        sparql_executor=sparql_executor,
        func_executor=func_executor,
        fallback_executor=fallback_exeuctor,
    )
