"""
# Author: qhouyee #

A test suite for the agent.utils.validate_input submodule.
"""

# Third party import
import pytest

# Self import
from agent.utils import validate_asset_url
from agent.exceptions import InvalidInputError


def test_validate_asset_url():
    """
    Tests valid inputs for validate_asset_url()
    """
    valid_input = [".", "..", "./dir", "../prev/dir/path",
        "http://www.example.org", "http://www.example.com/ns"]
    for url in valid_input:
        assert url+"/" == validate_asset_url(url)


def test_validate_asset_url_fails():
    """
    Tests invalid inputs for validate_asset_url()
    """
    invalid_input = ["./", "dir", "/dir/", "../../",
        "www.example.org", "http://www.example.com/ns/"]
    for url in invalid_input:
        # Assert expected error is raised
        with pytest.raises(InvalidInputError) as exc_info:
            validate_asset_url(url)
        expected_msg = "`assetUrl` parameter <" + url
        expected_msg += "> is invalid. It must start with `.`, `..`, or `http://`, and must not end with `/`"
        # Check error message
        assert exc_info.match(expected_msg)
