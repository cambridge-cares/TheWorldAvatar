from enum import Enum

import pytest

from model.constraint import (
    ComparativeOperator,
    CompoundComparativeConstraint,
    ExtremeValueConstraint,
    LogicalOperator,
    UnaryComparativeConstraint,
)
from model.aggregate import AggregateOperator
from services.core.parse import (
    KeyAggregateParser,
    MultiExtremeValueParser,
    NumericalConstraintParser,
)


class SampleEnum(Enum):
    HEIGHT = "Height"
    WEIGHT = "Weight"
    SPEED = "Speed"


class TestKeyAggregateParser:
    def test_parse(self, schema_parser):
        # Arrange
        parser = KeyAggregateParser(schema_parser=schema_parser, enum_cls=SampleEnum)

        # Act
        actual = parser.parse("average height")

        # Assert
        assert actual == (SampleEnum.HEIGHT, AggregateOperator.AVG)


@pytest.fixture(scope="class")
def numerical_constraint_parser(schema_parser):
    yield NumericalConstraintParser(schema_parser)


class TestNumericalConstraintParser:
    def test_parse_extremeValue(
        self, numerical_constraint_parser: NumericalConstraintParser
    ):
        # Act
        actual = numerical_constraint_parser.parse("lowest")

        # Assert
        assert actual == ExtremeValueConstraint.MIN

    def test_parse_comparativeClauses(
        self, numerical_constraint_parser: NumericalConstraintParser
    ):
        # Act
        actual = numerical_constraint_parser.parse("between 100 and 120")

        # Assert
        assert actual == CompoundComparativeConstraint(
            logical_operator=LogicalOperator.AND,
            constraints=(
                UnaryComparativeConstraint(
                    operator=ComparativeOperator.GE, operand=100
                ),
                UnaryComparativeConstraint(
                    operator=ComparativeOperator.LE, operand=120
                ),
            ),
        )
