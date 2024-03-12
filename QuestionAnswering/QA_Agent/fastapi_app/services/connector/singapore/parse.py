from model.constraint import (
    AtomicNumericalConstraint,
    CompoundNumericalConstraint,
    ExtremeValueConstraint,
    LogicalOperator,
)
from services.utils.parse import SchemaParser
from services.connector.singapore.agent import PlotConstraints
from .match import LandUseTypeMatcher


class NumericalArgConstraintParser:
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
        constraint_parser: NumericalArgConstraintParser,
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
                self.land_use_type_matcher.match(args["land_use_type"])[0]
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
