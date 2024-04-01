from enum import Enum
from functools import cache
from typing import Annotated, Generic, Type, TypeVar

from fastapi import Depends

from model.aggregate import AggregateOperator
from services.core.func_call import IFuncCaller, get_func_caller


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


E = TypeVar("E", bound=Enum)


class KeyAggregateParser(Generic[E]):
    def __init__(self, schema_parser: SchemaParser, enum_cls: Type[E]):
        self.schema_parser = schema_parser
        self.enum_cls = enum_cls

    def parse(self, text: str):
        args = self.schema_parser.parse(
            text=text,
            schema={
                "type": "object",
                "properties": {
                    "key": {
                        "type": "string",
                        "enum": [x.value for x in self.enum_cls],
                    },
                    "aggregate": {
                        "type": "string",
                        "enum": [x.value for x in AggregateOperator],
                    },
                },
            },
        )
        return self.enum_cls(args.get("key")), AggregateOperator(args.get("aggregate"))


@cache
def get_schema_parser(func_caller: Annotated[IFuncCaller, Depends(get_func_caller)]):
    return SchemaParser(func_caller)
