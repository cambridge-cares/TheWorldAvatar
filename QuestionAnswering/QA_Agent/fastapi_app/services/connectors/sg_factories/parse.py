from typing import Annotated, Optional

from fastapi import Depends

from model.constraint import ExtremeValueConstraint
from services.core.align_enum import EnumAligner
from services.core.parse import KeyAggregateParser, SchemaParser, get_schema_parser
from .model import FactoryAttrKey, FactoryConstraints, Industry


def get_factoryAttr_aggParser(
    schema_parser: Annotated[SchemaParser, Depends(get_schema_parser)]
):
    return KeyAggregateParser(schema_parser=schema_parser, enum_cls=FactoryAttrKey)


class FactoryConstraintsParser:
    def __init__(
        self, schema_parser: SchemaParser, industry_aligner: EnumAligner[Industry]
    ):
        self.schema_parser = schema_parser
        self.industry_aligner = industry_aligner

    def parse(self, text: Optional[str] = None):
        if not text:
            return None

        args = self.schema_parser.parse(
            text=text,
            schema={
                "type": "object",
                "properties": {
                    "industry": {
                        "type": "string",
                        "description": "A manufacturing industry e.g. chemical, food, semicconductor",
                    },
                    "generated_heat": {
                        "type": "string",
                        "description": "Whether to take lowest or greatest heat emission value",
                        "enum": ["MIN", "MAX"],
                    },
                    "specific_energy_consumption": {
                        "type": "string",
                        "description": "Whether to take lowest or greatest specific energy consumption",
                        "enum": ["MIN", "MAX"],
                    },
                    "thermal_efficiency": {
                        "type": "string",
                        "description": "Whether to take lowest or greatest thermal efficiency",
                        "enum": ["MIN", "MAX"],
                    },
                    "design_capacity": {
                        "type": "string",
                        "description": "Whether to take lowest or greatest design capacity aka production volume",
                        "enum": ["MIN", "MAX"],
                    },
                },
            },
        )

        industry = args.get("industry")
        generated_heat = args.get("generated_heat")
        specific_energy_consumption = args.get("specific_energy_consumption")
        thermal_efficiency = args.get("thermal_efficiency")
        design_capacity = args.get("design_capacity")

        return FactoryConstraints(
            industry=self.industry_aligner.align(industry) if industry else None,
            generated_heat=(
                ExtremeValueConstraint(generated_heat) if generated_heat else None
            ),
            specific_energy_consumption=(
                ExtremeValueConstraint(specific_energy_consumption)
                if specific_energy_consumption
                else None
            ),
            thermal_efficiency=(
                ExtremeValueConstraint(thermal_efficiency)
                if thermal_efficiency
                else None
            ),
            design_capacity=(
                ExtremeValueConstraint(design_capacity) if design_capacity else None
            ),
        )
