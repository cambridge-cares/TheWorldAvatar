import json
import os
import tempfile

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

def test_enrichment_api_empty(client):
    """Test empty query."""
    rv = client.post('/api/enrichment')
    rv = json.loads(rv.data)
    assert rv["status"] == "500"

def test_enrichment_api(client):
    """Test empty query."""
    rv = client.post('/api/enrichment', query_string =dict(
        addr="../data/power_plant_DEU/kwl.ttl",
        add_knowledge="ontomatch.knowledge.geocoding"
    ))
    rv = json.loads(rv.data)
    print(rv)
    assert rv["status"] == "200"