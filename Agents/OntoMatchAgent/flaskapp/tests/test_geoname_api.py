import json
import pytest

from flaskapp import create_app

@pytest.fixture
def client():
    app = create_app({'TESTING': True})

    with app.test_client() as client:
        yield client

def test_geonames_api_empty(client):
    """Test empty query."""
    rv = client.post('/api/geonames/query')
    rv = json.loads(rv.data)
    assert rv["status"] == "500"
