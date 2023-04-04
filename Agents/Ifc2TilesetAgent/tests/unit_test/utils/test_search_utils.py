"""
# Author: qhouyee #

A test suite for the agent.utils.search_utils submodule.
"""
import pytest

# Self import
from agent.utils import find_word


@pytest.mark.parametrize(
    "input_string, expected",
    [
        ("starting a long test", True),
        ("this is a monday", True),
        ("wrong answer only", False)
    ]
)
def test_find_word(input_string, expected):
    # Arrange
    sample_words = ["test", "monday", "tuesday", "wednesday"]

    # Act
    actual = find_word(sample_words, input_string)

    # Assert
    assert actual == expected
