"""
# Author: qhouyee #

This module provides utility functions for running operations on system,
such as shell command and file operations.
"""

# Third party imports
from py4jps import agentlogging

# Self imports
from agent.exceptions import InvalidInputError

# Retrieve logger
logger = agentlogging.get_logger("dev")

def validate_asset_url(asset_url):
    """
    Validates the assetUrl parameter sent by the POST request.

    Argument:
    asset_url - `assetUrl` request parameter
    Returns:
    Required url format
    """
    valid_urlstart = [".", "..", "http://"]
    if any(map(asset_url.startswith, valid_urlstart)) and not asset_url.endswith("/"):
        return asset_url + "/"
    else:
        raise InvalidInputError("`assetUrl` parameter <" + asset_url +
        "> is invalid. It must start with `.`, `..`, or `http://`, and must not end with `/`")
