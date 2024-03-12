import os

import pytest
from redis import Redis

from services.connector.singapore.agent import PlotConstraints
from services.connector.singapore.match import LandUseTypeMatcher
from services.embed import TritonMPNetEmbedder
from services.kg_client import KgClient
from model.constraint import (
    AtomicNumericalConstraint,
    ExtremeValueConstraint,
    NumericalOperator,
)
from services.func_call import OpenAIFuncCaller
from services.utils.parse import SchemaParser
from services.connector.singapore.parse import (
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
def land_use_type_matcher():
    redis_client = Redis()
    kg_client = KgClient(os.getenv("KG_ENDPOINT_SINGAPORE"))
    embedder = TritonMPNetEmbedder()
    matcher = LandUseTypeMatcher(
        kg_client=kg_client, embedder=embedder, redis_client=redis_client
    )

    yield matcher

    redis_client.flushdb()


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
