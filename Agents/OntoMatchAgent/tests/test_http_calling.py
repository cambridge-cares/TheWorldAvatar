import logging
import unittest
import json
import ontomatch.blocking
import ontomatch.knowledge.enrichment
import tests.utils_for_testing
import ontomatch.utils.util
class TestGeocoding(unittest.TestCase):

    def test_enrichement_nohttp(self):
        addr = "../data/power_plant_DEU/kwl.ttl"
        add_knowledge = "ontomatch.knowledge.geocoding"
        enriched, handle = ontomatch.knowledge.enrichment.Agent().start(addr, add_knowledge, http=False)
        print(enriched)
        print(handle)

    def test_blackboard_write_nohttp(self):
        addr = './conf/conf_power_plant_DEU_auto.json'
        params = ontomatch.utils.util.read_json_from_path(addr)
        params_str = json.dumps(params)
        config_handle = ontomatch.utils.util.call_agent_blackboard_for_writing(addr, params_str, http=False)
        print(config_handle)
        assert "_conf_conf_power_plant_DEU_auto.json" in config_handle

    def test_blackboard_read_nohttp(self):
        handle = '._conf_conf_power_plant_DEU_auto.json_1638437022.391991'
        object = ontomatch.utils.util.call_agent_blackboard_for_reading(handle, http=False)
        print("object")
        #TODO: assert content

    def test_blackboard_write_http(self):
        addr = './conf/conf_power_plant_DEU_auto.json'
        params = ontomatch.utils.util.read_json_from_path(addr)
        params_str = json.dumps(params)
        config_handle = ontomatch.utils.util.call_agent_blackboard_for_writing(addr, params_str, http=True)
        assert "_conf_conf_power_plant_DEU_auto.json" in config_handle

    def test_blackboard_read_http(self):
        handle = '._conf_conf_power_plant_DEU_auto.json_1638437022.391991'
        object = ontomatch.utils.util.call_agent_blackboard_for_reading(handle, http=True)
        print("object")
        #TODO: assert content

    def test_coordinator_http(self):
        import time
        addr = './conf/power_plant_DEU/conf_test_webagent.json'
        params = ontomatch.utils.util.read_json_from_path(addr)
        # write the config params to the blackboard
        params_str = json.dumps(params)
        config_handle = ontomatch.utils.util.call_agent_blackboard_for_writing(addr, params_str, http=False)

        starttime = time.time()
        ontomatch.coordinator.Agent().start(config_handle, http=True)
        timenow = time.time() - starttime
        logging.info('elapsed time in seconds=%s', timenow)
