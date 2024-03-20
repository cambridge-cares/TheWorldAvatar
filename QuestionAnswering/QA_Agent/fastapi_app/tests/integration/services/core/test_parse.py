from enum import Enum

from model.aggregate import AggregateOperator
from services.core.parse import KeyAggregateParser


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
