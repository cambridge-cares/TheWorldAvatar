"""
# Author: qhouyee #

A test suite for the agent.utils.validate_input submodule.
"""

# Third party import
import pytest

# Self import
from agent.utils import validate_asset_url


@pytest.mark.parametrize(
    "asset_url, expected",
    [
        (".", True),
        ("..", True),
        ("./dir", True),
        ("../prev/dir/path", True),
        ("http://www.example.org", True),
        ("http://www.example.com/ns", True),
        ("./", False),
        ("dir", False),
        ("/dir/", False),
        ("../../", False),
        ("www.example.org", False),
        ("http://www.example.com/ns/", False)
    ]
)
def test_validate_asset_url(asset_url, expected):
    # Act
    actual = validate_asset_url(asset_url)

    # Assert
    assert actual == expected
