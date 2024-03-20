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
from services.connectors.sg_land_lots.parse import (
    PlotNumArgConstraintParser,
    PlotConstraintsParser,
)


@pytest.fixture
def plot_num_arg_constraint_parser(schema_parser):
    yield PlotNumArgConstraintParser(schema_parser)


@pytest.fixture
def land_use_type_matcher(embedder, redis_client):
    kg_client = KgClient(os.getenv("KG_ENDPOINT_SG_LAND_LOTS"))
    matcher = LandUseTypeMatcher(
        kg_client=kg_client, embedder=embedder, redis_client=redis_client
    )

    yield matcher


class TestPlotConstraintsParser:
    def test_parse_extremeArg(
        self,
        schema_parser,
        plot_num_arg_constraint_parser,
        land_use_type_matcher,
    ):
        # Arrange
        parser = PlotConstraintsParser(
            schema_parser=schema_parser,
            constraint_parser=plot_num_arg_constraint_parser,
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
        plot_num_arg_constraint_parser,
        land_use_type_matcher,
    ):
        # Arrange
        parser = PlotConstraintsParser(
            schema_parser=schema_parser,
            constraint_parser=plot_num_arg_constraint_parser,
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
