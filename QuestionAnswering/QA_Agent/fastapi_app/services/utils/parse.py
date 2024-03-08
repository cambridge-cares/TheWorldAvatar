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


class ConstraintParser:
    SCHEMA = {
        "type": "object",
        "properties": {
            "key": {
                "type": "string",
                "description": "Property name e.g. boiling point, heat of vaporization",
            },
            "logical_operator": {
                "type": "string",
                "enum": ["AND", "OR"],
            },
            "constraints": {
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
    }

    def __init__(self, schema_parser: SchemaParser):
        self.schema_parser = schema_parser

    def parse(self, text: str):
        args = self.schema_parser.parse(text, self.SCHEMA)
        # TODO: handle error
        key: str = args["key"]

        if "logical_operator" not in args:
            logical_operator = None
        if args.get("logical_operator") == "AND":
            logical_operator = LogicalOperator.AND
        elif args.get("logical_operator") == "OR":
            logical_operator = LogicalOperator.OR
        else:
            logical_operator = None

        constraints = [
            AtomicNumericalConstraint(
                operator=constraint["operator"],
                operand=constraint["operand"],
                unit=constraint.get("unit"),
            )
            for constraint in args["constraints"]
        ]

        constraint = CompoundNumericalConstraint(
            logical_operator=logical_operator,
            constraints=constraints,
        )
        return key, constraint


def get_constraint_parser(
    func_caller: Annotated[IFuncCaller, Depends(get_func_caller)]
):
    return ConstraintParser(SchemaParser(func_caller))
