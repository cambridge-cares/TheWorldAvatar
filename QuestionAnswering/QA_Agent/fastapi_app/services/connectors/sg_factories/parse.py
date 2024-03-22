from typing import Annotated, Dict, Optional

from fastapi import Depends

from model.constraint import ExtremeValueConstraint
from services.core.align_enum import EnumAligner
from services.core.parse import KeyAggregateParser, SchemaParser, get_schema_parser
from services.connectors.sg_factories.align import get_industry_aligner
from .model import FactoryNumAttrKey, Industry


def get_factoryAttr_aggParser(
    schema_parser: Annotated[SchemaParser, Depends(get_schema_parser)]
):
    return KeyAggregateParser(schema_parser=schema_parser, enum_cls=FactoryNumAttrKey)


class FactoryConstraintsParser:
    def __init__(
        self, schema_parser: SchemaParser, industry_aligner: EnumAligner[Industry]
    ):
        self.schema_parser = schema_parser
        self.industry_aligner = industry_aligner

    def parse(
        self, text: Optional[str]
    ) -> Dict[FactoryNumAttrKey, ExtremeValueConstraint]:
        if not text:
            return dict()

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
    industry_aligner: Annotated[EnumAligner[Industry], Depends(get_industry_aligner)],
):
    return FactoryConstraintsParser(
        schema_parser=schema_parser, industry_aligner=industry_aligner
    )
