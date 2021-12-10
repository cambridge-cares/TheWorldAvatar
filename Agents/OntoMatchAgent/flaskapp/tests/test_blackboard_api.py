#'./conf/conf_power_plant_DEU_auto.json'
import json
import pytest

from flaskapp import create_app

@pytest.fixture
def client():
    app = create_app({'TESTING': True})

    with app.test_client() as client:
        yield client

def test_blackboard_get_empty(client):
    """Test empty query."""
    rv = client.get('/api/blackboard')
    re = json.loads(rv.data)
    assert rv.status == "400 BAD REQUEST"

def test_blackboard_post_missing_params(client):
    """Test empty query."""
    addr = "test_write.json"

    rv = client.post('/api/blackboard', query_string=dict(
    addr=addr))
    assert rv.status == "400 BAD REQUEST"

def test_blackboard_post_get(client):
    """Test post."""
    data = {"test":True}
    serialized_object = json.dumps(data)
    addr = "test_write.json"

    rv = client.post('/api/blackboard', query_string=dict(
    addr=addr, serialized_object=serialized_object
    ))
    assert rv.status == "200 OK"
    rd = json.loads(rv.data)
    print(rd)
    handle = rd["result"]["handle"]
    rv = client.get('/api/blackboard', query_string=dict(handle=handle))
    assert rv.status == "200 OK"
    rd = json.loads(rv.data)
    content = json.dumps({"test":True})
    assert "result" in rd
    assert "object" in rd["result"]
    assert "test" in rd["result"]["object"]
    assert rd["result"]["object"] == content
