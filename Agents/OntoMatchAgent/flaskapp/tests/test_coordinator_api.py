import json
import pytest

import ontomatch
from flaskapp.wsgi import create_app

@pytest.fixture
def client():
    app = create_app({'TESTING': True})

    with app.test_client() as client:
        yield client

'''
This test needs to be run from /flaskapp instead of /flaskapp/tests directory
due to relative path issue. 
An alternative way will be changing the src/tgt/csv relative path in the config files. 
'''
def test_coordinator_api(client):
    config_file = "../conf/http_test/conf_test_webagent.json"
    params = ontomatch.utils.util.read_json_from_path(config_file)
    params_str = json.dumps(params)

    rv = client.post('/api/blackboard', query_string=dict(
        addr=config_file, serialized_object=params_str
    ))
    assert rv.status == "200 OK"
    rd = json.loads(rv.data)
    print(rd)
    handle = rd["result"]["handle"]
    print(handle)

    rv = client.post('/api/coordinator', query_string =dict(
        config=handle
    ))
    re = json.loads(rv.data)
    print(re)
    assert rv.status == "200 OK"
