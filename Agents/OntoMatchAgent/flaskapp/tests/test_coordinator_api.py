import json
import pytest

from flaskapp import create_app

@pytest.fixture
def client():
    app = create_app({'TESTING': True})

    with app.test_client() as client:
        yield client


def test_coordinator_api(client):
    """Test empty query."""
    handle = ".._conf_power_plant_DEU_conf_test_webagent.json_1638946181.100492"

    rv = client.post('/api/coordinator', query_string =dict(
        config=handle
    ))
    re = json.loads(rv.data)
    print(re)
    assert rv.status == "200 OK"
