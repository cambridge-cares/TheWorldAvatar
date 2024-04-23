from typing import Annotated, Dict

from fastapi import Depends

from model.constraint import ExtremeValueConstraint
from services.core.parse import KeyAggregateParser, SchemaParser, get_schema_parser
from .model import FactoryNumAttrKey


def get_factoryAttr_aggParser(
    schema_parser: Annotated[SchemaParser, Depends(get_schema_parser)]
):
    return KeyAggregateParser(schema_parser=schema_parser, enum_cls=FactoryNumAttrKey)


class FactoryConstraintsParser:
    def __init__(self, schema_parser: SchemaParser):
        self.schema_parser = schema_parser

    def parse(self, text: str) -> Dict[FactoryNumAttrKey, ExtremeValueConstraint]:
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
                    "SpecificEnergyConsumption": {
                        "type": "string",
                        "description": "Whether to take lowest or greatest specific energy consumption",
                        "enum": ["MIN", "MAX"],
                    },
                    "ThermalEfficiency": {
                        "type": "string",
                        "description": "Whether to take lowest or greatest thermal efficiency",
                        "enum": ["MIN", "MAX"],
                    },
                    "DesignCapacity": {
                        "type": "string",
                        "description": "Whether to take lowest or greatest design capacity aka production volume",
                        "enum": ["MIN", "MAX"],
                    },
                },
            },
        )

        return {
            FactoryNumAttrKey(k): ExtremeValueConstraint(v) for k, v in args.items()
        }


def get_factoryConstraints_parser(
    schema_parser: Annotated[SchemaParser, Depends(get_schema_parser)],
):
    return FactoryConstraintsParser(schema_parser=schema_parser)
