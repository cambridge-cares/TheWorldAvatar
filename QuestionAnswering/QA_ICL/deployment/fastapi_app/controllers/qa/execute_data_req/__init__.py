from functools import cache
from typing import Annotated

from fastapi import Depends

from services.example_store.model import DataRequest, FuncDataRequest, SparqlDataRequest

from .func import FuncDataReqExecutor, get_funcReq_executor
from .sparql import SparqlDataReqExecutor, get_sparqlReq_executor


class DataReqExecutor:
    def __init__(
        self, sparql_executor: SparqlDataReqExecutor, func_executor: FuncDataReqExecutor
    ):
        self.sparql_executor = sparql_executor
        self.func_executor = func_executor

    def exec(self, req: DataRequest):
        if isinstance(req, SparqlDataRequest):
            return self.sparql_executor.exec(req)
        elif isinstance(req, FuncDataRequest):
            return self.func_executor.exec(req)
        else:
            raise ValueError("Unsupported: " + str(req))


@cache
def get_dataReq_executor(
    sparql_executor: Annotated[
        SparqlDataReqExecutor, Depends(get_sparqlReq_executor)
    ],
    func_executor: Annotated[FuncDataReqExecutor, Depends(get_funcReq_executor)],
):
    return DataReqExecutor(
        sparql_executor=sparql_executor, func_executor=func_executor
    )
