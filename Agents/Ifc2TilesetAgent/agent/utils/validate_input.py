"""
# Author: qhouyee #

This module provides utility functions for running operations on system,
such as shell command and file operations.
"""

# Third party imports
from py4jps import agentlogging

# Retrieve logger
logger = agentlogging.get_logger("dev")


def validate_asset_url(asset_url: str):
    """Validates the assetUrl parameter sent by the POST request."""
    valid_url_prefixes = [".", "..", "http://"]
    return any(map(asset_url.startswith, valid_url_prefixes)) and not asset_url.endswith("/")
