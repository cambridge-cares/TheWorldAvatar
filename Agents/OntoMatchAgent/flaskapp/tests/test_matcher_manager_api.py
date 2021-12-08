import json
import pytest

from flaskapp import create_app

@pytest.fixture
def client():
    app = create_app({'TESTING': True})

    with app.test_client() as client:
        yield client

def test_matchmanager_autocalibration(client):
    """Test empty query."""
    #TODO: wrong params
    rv = client.post('/api/matchmanager', query_string=dict(choice="autocalibration",
        config_handle="._conf_conf_power_plant_DEU_auto.json_1638862285.1254559",
        src_graph_handle="../data/power_plant_DEU/kwl.pkl",
        tgt_graph_handle="../data/power_plant_DEU/gppd_DEU.pkl"
    ))
    print(rv.data)
    print(rv)
    rd = json.loads(rv.data)
    assert rv.status == "200 OK"
