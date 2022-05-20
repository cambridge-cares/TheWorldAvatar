import json
import requests
import ontomatch.utils.util
import unittest
class TestGeocoding(unittest.TestCase):

#The set of tests only works when flask server is online
    def test_coordinator_api(self):


        config_file = "../conf/http_test/conf_test_webagent.json"
        #config_file ="../conf/power_plant_DEU/conf_power_plant_DEU_auto_5_geo_http_link.json"
        params = ontomatch.utils.util.read_json_from_path(config_file)
        params_str = json.dumps(params)

        rv = requests.post('http://localhost:5000/api/blackboard', params={
            'addr':config_file, 'serialized_object':params_str
        })

        print(rv.request.url)
        assert rv.status_code == 200
        rd = rv.json()
        print(rd)
        handle = rd["result"]["handle"]
        print(handle)

        rv = requests.post('http://localhost:5000/api/coordinator', params ={
            "config":handle
        })
        re = rv.json()
        print(rv.request.url)
        print(re)
        assert rv.status_code == 200

    def test_enrichment_api(self):
        rv = requests.post('http://localhost:5000/api/enrichment', params =dict(
            addr="./data/power_plant_DEU/kwl.ttl",
            add_knowledge="ontomatch.knowledge.geocoding"
        ))
        re = rv.json()
        print(re)
        assert rv.status_code == 200
        assert 'result' in re
        assert 'enriched' in re['result']
        assert re['result']['enriched'] is True
        assert 'handle' in re['result']

    def test_matchmanager_api(self):
        #get handle from blackboard
        config_file = "../conf/http_test/conf_test_webagent.json"
        params = ontomatch.utils.util.read_json_from_path(config_file)
        params_str = json.dumps(params)

        rv = requests.post('http://localhost:5000/api/blackboard', params=dict(
        addr=config_file, serialized_object=params_str
        ))
        assert rv.status_code == 200
        rd = rv.json()
        handle = rd["result"]["handle"]
        print("handle: "+handle)
        rv = requests.post('http://localhost:5000/api/matchmanager', params=dict(choice="autocalibration",
            config_handle=handle,
            src_graph_handle="./data/power_plant_DEU/kwl.pkl",
            tgt_graph_handle="./data/power_plant_DEU/gppd_DEU.pkl"
        ))
        rd = rv.json()
        assert rv.status_code == 200