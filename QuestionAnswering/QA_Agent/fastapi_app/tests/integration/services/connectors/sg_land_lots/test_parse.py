import os

import pytest

from services.connectors.sg_land_lots.agent import PlotConstraints
from services.connectors.sg_land_lots.match import LandUseTypeMatcher
from services.kg_client import KgClient
from model.constraint import (
    AtomicNumericalConstraint,
    ExtremeValueConstraint,
    NumericalOperator,
)
from services.func_call import OpenAIFuncCaller
from services.utils.parse import SchemaParser
from services.connectors.sg_land_lots.parse import (
    NumericalArgConstraintParser,
    PlotConstraintsParser,
)


@pytest.fixture
def schema_parser():
    func_caller = OpenAIFuncCaller()
    yield SchemaParser(func_caller)


@pytest.fixture
def numerical_arg_constraint_parser(schema_parser):
    yield NumericalArgConstraintParser(schema_parser)


@pytest.fixture
def land_use_type_matcher(docs_retriever):
    kg_client = KgClient(os.getenv("KG_ENDPOINT_SG_LAND_LOTS"))
    matcher = LandUseTypeMatcher(kg_client=kg_client, docs_retriever=docs_retriever)

    yield matcher


class TestPlotConstraintsParser:
    def test_parse_extremeArg(
        self,
        schema_parser,
        numerical_arg_constraint_parser,
        land_use_type_matcher,
    ):
        # Arrange
        parser = PlotConstraintsParser(
            schema_parser=schema_parser,
            constraint_parser=numerical_arg_constraint_parser,
            land_use_type_matcher=land_use_type_matcher,
        )

        # Act
        actual = parser.parse("smallest plot designated for commercial use")

        # Assert
        assert actual == PlotConstraints(
            land_use_type_iri="https://www.theworldavatar.com/kg/landplot/LandUseType_f45d365c-1d59-4fda-b240-afb0066f2d61",
            plot_area=ExtremeValueConstraint.MIN,
        )

    def test_parse_extremeArg(
        self,
        schema_parser,
        numerical_arg_constraint_parser,
        land_use_type_matcher,
    ):
        # Arrange
        parser = PlotConstraintsParser(
            schema_parser=schema_parser,
            constraint_parser=numerical_arg_constraint_parser,
            land_use_type_matcher=land_use_type_matcher,
        )

        # Act
        actual = parser.parse("plots with gross plot ratio below 2")

        # Assert
        assert actual.land_use_type_iri is None
        assert actual.gross_floor_area is None
        assert actual.plot_area is None
        assert actual.gross_plot_ratio is not None
        assert actual.gross_plot_ratio.constraints == tuple(
            [AtomicNumericalConstraint(operator=NumericalOperator.LT, operand=2.0)]
        )
