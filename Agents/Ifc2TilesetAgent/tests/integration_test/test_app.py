"""
# Author: qhouyee #

An integration test suite for the entire app.
"""

# Third party import
import pytest

# Self import
from . import testconsts


@pytest.mark.parametrize(
    "expected_response",
    [
        (testconsts.DEFAULT_RESPONSE),
    ]
)
def test_default(expected_response, client):
    """
    Tests the GET request for default route
    """
    # Assert client is operational at route
    assert client.get('/').status_code == 200
    # Perform GET request
    response = client.get("/")
    assert bytes(expected_response, 'utf-8') == response.data
