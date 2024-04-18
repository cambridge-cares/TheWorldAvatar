from typing import Annotated

from fastapi import Depends

from model.constraint import (
    UnaryComparativeConstraint,
    CompoundComparativeConstraint,
    LogicalOperator,
)
from services.core.parse import SchemaParser, get_schema_parser


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
            UnaryComparativeConstraint(
                operator=constraint["operator"],
                operand=constraint["operand"],
                unit=constraint.get("unit"),
            )
            for constraint in args["constraints"]
        ]

        constraint = CompoundComparativeConstraint(
            logical_operator=logical_operator,
            constraints=constraints,
        )
        return key, constraint


def get_constraint_parser(
    schema_parser: Annotated[SchemaParser, Depends(get_schema_parser)]
):
    return ConstraintParser(schema_parser)
