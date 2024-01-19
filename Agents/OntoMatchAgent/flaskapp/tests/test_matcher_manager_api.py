import json
import pytest

from flaskapp.wsgi import create_app
import ontomatch.utils.util
@pytest.fixture
def client():
    app = create_app({'TESTING': True})

    with app.test_client() as client:
        yield client


def test_matchmanager_wrong_params(client):
    rv = client.post('/api/matchmanager', query_string=dict(choice="notReal",
        config_handle="doesnotmatter.json",
        src_graph_handle="../data/power_plant_DEU/kwl.pkl",
        tgt_graph_handle="../data/power_plant_DEU/gppd_DEU.pkl"
    ))

    assert rv.status == "400 BAD REQUEST"


def test_matchmanager_autocalibration(client):
    """Test empty query."""
    #get handle from blackboard
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
    rv = client.post('/api/matchmanager', query_string=dict(choice="autocalibration",
        config_handle=handle,
        src_graph_handle="../data/power_plant_DEU/kwl.pkl",
        tgt_graph_handle="../data/power_plant_DEU/gppd_DEU.pkl"
    ))
    print(rv.data)
    print(rv)
    rd = json.loads(rv.data)
    assert rv.status == "200 OK"
