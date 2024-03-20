from typing import Annotated

from fastapi import Depends

from model.constraint import (
    AtomicNumericalConstraint,
    CompoundNumericalConstraint,
    ExtremeValueConstraint,
    LogicalOperator,
)
from services.core.parse import (
    KeyAggregateParser,
    SchemaParser,
    get_schema_parser,
)
from .constants import PlotAttrKey
from .agent import PlotConstraints
from .match import LandUseTypeMatcher, get_land_use_type_matcher


class PlotNumArgConstraintParser:
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
                    "numerical_constraints": {
                        "type": "object",
                        "properties": {
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
                    },
                },
            },
        )
        if args.get("numerical_constraints"):
            args = args["numerical_constraints"]
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

            return CompoundNumericalConstraint(
                logical_operator=logical_operator,
                constraints=constraints,
            )

        if args.get("extreme_value"):
            return ExtremeValueConstraint(args["extreme_value"])

        return None


class PlotConstraintsParser:
    def __init__(
        self,
        schema_parser: SchemaParser,
        constraint_parser: PlotNumArgConstraintParser,
        land_use_type_matcher: LandUseTypeMatcher,
    ):
        self.schema_parser = schema_parser
        self.constraint_parser = constraint_parser
        self.land_use_type_matcher = land_use_type_matcher

    def parse(self, text: str):
        args = self.schema_parser.parse(
            text=text,
            schema={
                "type": "object",
                "properties": {
                    "land_use_type": {
                        "type": "string",
                        "description": "Land use classification e.g. agriculture, business, commercial, educational institution",
                    },
                    "gross_plot_ratio": {
                        "type": "string",
                        "description": "Constraint on gross plot ratio e.g. larger than 2, lowest gross plot ratio",
                    },
                    "plot_area": {
                        "type": "string",
                        "description": "Constraint on plot area e.g. between 500 and 750 sqm, largest plot area",
                    },
                    "gross_floor_area": {
                        "type": "string",
                        "description": "Constraint on gross floor area e.g. between 500 and 750 sqm, smallest gross floor area",
                    },
                },
            },
        )

        return PlotConstraints(
            land_use_type_iri=(
                self.land_use_type_matcher.match(args["land_use_type"])
                if args.get("land_use_type")
                else None
            ),
            gross_plot_ratio=(
                self.constraint_parser.parse(args["gross_plot_ratio"])
                if args.get("gross_plot_ratio")
                else None
            ),
            plot_area=(
                self.constraint_parser.parse(args["plot_area"])
                if args.get("plot_area")
                else None
            ),
            gross_floor_area=(
                self.constraint_parser.parse(args["gross_floor_area"])
                if args.get("gross_floor_area")
                else None
            ),
        )


def get_numerical_arg_constraint_parser(
    schema_parser: Annotated[SchemaParser, Depends(get_schema_parser)]
):
    return PlotNumArgConstraintParser(schema_parser)


def get_plot_constraint_parser(
    schema_parser: Annotated[SchemaParser, Depends(get_schema_parser)],
    constraint_parser: Annotated[
        PlotNumArgConstraintParser, Depends(get_numerical_arg_constraint_parser)
    ],
    land_use_type_matcher: Annotated[
        LandUseTypeMatcher, Depends(get_land_use_type_matcher)
    ],
):
    return PlotConstraintsParser(
        schema_parser=schema_parser,
        constraint_parser=constraint_parser,
        land_use_type_matcher=land_use_type_matcher,
    )


def get_plot_attr_agg_parser(
    schema_parser: Annotated[SchemaParser, Depends(get_schema_parser)]
):
    return KeyAggregateParser(schema_parser=schema_parser, enum_cls=PlotAttrKey)
