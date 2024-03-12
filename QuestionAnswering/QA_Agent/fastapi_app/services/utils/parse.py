from typing import Annotated

from fastapi import Depends
from services.func_call import IFuncCaller, get_func_caller
from model.constraint import (
    AtomicNumericalConstraint,
    CompoundNumericalConstraint,
    LogicalOperator,
)


class SchemaParser:
    def __init__(self, func_caller: IFuncCaller):
        self.func_call_predictor = func_caller

    def parse(self, text: str, schema: dict) -> dict:
        _, args = self.func_call_predictor.predict(
            funcs=[
                {
                    "name": "find",
                    "description": "Find entities given some constraints",
                    "parameters": schema,
                }
            ],
            query="Find entities where " + text,
        )
        return args


def get_schema_parser(func_caller: Annotated[IFuncCaller, Depends(get_func_caller)]):
    return SchemaParser(func_caller)
