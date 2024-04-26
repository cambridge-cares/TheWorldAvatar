from typing import Annotated

from fastapi import Depends

from model.constraint import ExtremeValueConstraint
from core.parse import KeyAggregateParser, SchemaParser, get_schema_parser
from .model import DataCentreAttrKey


class DataCentreConstraintsParser:
    def __init__(self, schema_parser: SchemaParser):
        self.schema_parser = schema_parser

    def parse(self, text: str):
        args = self.schema_parser.parse(
            text=text,
            schema={
                "type": "object",
                "properties": {
                    "GeneratedHeat": {
                        "type": "string",
                        "description": "Whether to take lowest or greatest heat emission value",
                        "enum": ["MIN", "MAX"],
                    },
                    "MaximumITCapacity": {
                        "type": "string",
                        "description": "Whether to take lowest or greatest maximum IT capacity",
                        "enum": ["MIN", "MAX"],
                    },
                    "UtilizationRate": {
                        "type": "string",
                        "description": "Whether to take lowest or greatest utilization rate",
                        "enum": ["MIN", "MAX"],
                    },
                    "FloorArea": {
                        "type": "string",
                        "description": "Whether to take lowest or greatest floor area",
                        "enum": ["MIN", "MAX"],
                    },
                },
            },
        )

        return {
            DataCentreAttrKey(k): ExtremeValueConstraint(v) for k, v in args.items()
        }


def get_dataCentreConstraints_parser(
    schema_parser: Annotated[SchemaParser, Depends(get_schema_parser)]
):
    return DataCentreConstraintsParser(schema_parser)


def get_dataCentreAttr_aggParser(
    schema_parser: Annotated[SchemaParser, Depends(get_schema_parser)]
):
    return KeyAggregateParser(schema_parser=schema_parser, enum_cls=DataCentreAttrKey)
