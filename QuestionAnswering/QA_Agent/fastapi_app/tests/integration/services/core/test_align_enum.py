from enum import Enum

from services.core.align_enum import EnumAligner


class SampleEnum(Enum):
    ONE = "one"
    TWO = "two"
    THREE = "three"


class TestEnumAligner:
    def test_align(self, embedder, redis_client):
        # Arrange
        enum_aligner = EnumAligner(
            embedder=embedder, redis_client=redis_client, key="test", enum_cls=SampleEnum
        )

        # Act
        actual = enum_aligner.align("Two")

        # Assert
        assert actual == SampleEnum.TWO
