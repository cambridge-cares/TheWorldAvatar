from enum import Enum
from functools import cache
from typing import Annotated, Dict, Generic, Type, TypeVar

from fastapi import Depends

from model.constraint import (
    CompoundComparativeConstraint,
    ExtremeValueConstraint,
    LogicalOperator,
    UnaryComparativeConstraint,
)
from services.core.exceptions import InferenceWithUnsatisfiedConstraint
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


class NumericalConstraintParser:
    def __init__(self, schema_parser: SchemaParser):
        self.schema_parser = schema_parser

    def parse(self, text: str):
        args = self.schema_parser.parse(
            text=text,
            schema={
                "type": "object",
                "properties": {
                    "extreme_value": {
                        "type": "string",
                        "description": "Whether to take the smallest or largest value",
                        "enum": ["MIN", "MAX"],
                    },
                    "comparative_constraint": {
                        "type": "object",
                        "properties": {
                            "logical_operator": {
                                "type": "string",
                                "enum": ["AND", "OR"],
                            },
                            "clauses": {
                                "type": "array",
                                "items": {
                                    "type": "object",
                                    "properties": {
                                        "operator": {
                                            "type": "string",
                                            "enum": ["=", "<", "<=", ">", ">="],
                                            "description": "The numerical operator of the constraint",
                                        },
                                        "operand": {
                                            "type": "number",
                                            "description": "The reference value of the constraint operator",
                                        },
                                        "unit": {
                                            "type": "string",
                                            "description": "Unit of the physical property e.g. Kelvin, meter",
                                        },
                                    },
                                },
                            },
                        },
                    },
                },
            },
        )
        if "comparative_constraint" in args:
            args = args["comparative_constraint"]

            logical_operator = (
                args.get("logical_operator") if isinstance(args, dict) else None
            )
            if logical_operator == "AND":
                logical_operator = LogicalOperator.AND
            elif logical_operator == "OR":
                logical_operator = LogicalOperator.OR
            else:
                logical_operator = None

            if not isinstance(args, dict) or "clauses" not in args:
                raise InferenceWithUnsatisfiedConstraint(args)

            try:
                constraints = [
                    UnaryComparativeConstraint(**constraint)
                    for constraint in args["clauses"]
                ]
            except TypeError:
                raise InferenceWithUnsatisfiedConstraint(args)

            return CompoundComparativeConstraint(
                logical_operator=logical_operator,
                constraints=constraints,
            )

        if "extreme_value" in args:
            return ExtremeValueConstraint(args["extreme_value"])

        return None


@cache
def get_schema_parser(func_caller: Annotated[IFuncCaller, Depends(get_func_caller)]):
    return SchemaParser(func_caller)


@cache
def get_numConstraint_parser(
    schema_parser: Annotated[SchemaParser, Depends(get_schema_parser)]
):
    return NumericalConstraintParser(schema_parser=schema_parser)
