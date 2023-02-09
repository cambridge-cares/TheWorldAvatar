"""
# Author: qhouyee #

A test suite for the agent.utils.validate_input submodule.
"""

# Third party import
import pytest

# Self import
from agent.utils import validate_asset_url


def test_validate_asset_url():
    """
    Tests valid inputs for validate_asset_url()
    """
    valid_input = [".", "..", "./dir", "../prev/dir/path",
        "http://www.example.org", "http://www.example.com/ns"]
    for url in valid_input:
        assert validate_asset_url(url)


def test_validate_asset_url_fails():
    """
    Tests invalid inputs for validate_asset_url()
    """
    invalid_input = ["./", "dir", "/dir/", "../../",
        "www.example.org", "http://www.example.com/ns/"]
    for url in invalid_input:
        # Assert that validation method returns false
        assert not validate_asset_url(url)
