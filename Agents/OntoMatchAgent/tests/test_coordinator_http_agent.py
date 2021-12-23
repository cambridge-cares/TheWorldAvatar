import json
import requests
import ontomatch.utils.util
import unittest
class TestGeocoding(unittest.TestCase):


    def test_coordinator_api(self):
        config_file = "../conf/http_test/conf_test_webagent.json"
        params = ontomatch.utils.util.read_json_from_path(config_file)
        params_str = json.dumps(params)

        rv = requests.post('http://localhost:5000/api/blackboard', params={
            'addr':config_file, 'serialized_object':params_str
        })

        assert rv.status_code == 200
        rd = rv.json()
        print(rd)
        handle = rd["result"]["handle"]
        print(handle)


        rv = requests.post('http://localhost:5000/api/coordinator', params ={
            "config":handle
        })
        re = rv.json()
        print(re)
        assert rv.status_code == 200
