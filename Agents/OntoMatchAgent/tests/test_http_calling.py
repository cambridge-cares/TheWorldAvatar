import logging
import unittest
import json
import ontomatch.blocking
import ontomatch.knowledge.enrichment
import tests.utils_for_testing
import ontomatch.utils.util
class TestGeocoding(tests.utils_for_testing.TestCaseOntoMatch):

    def test_enrichement_nohttp(self):
        addr = "./data/power_plant_DEU/kwl.ttl"
        add_knowledge = "ontomatch.knowledge.geocoding"
        enriched, handle = ontomatch.knowledge.enrichment.Agent().start(addr, add_knowledge, http=False)
        print(enriched)
        print(handle)
        assert  enriched is True
        assert handle is not None

    def test_blackboard_write_read_nohttp(self):
        addr = './tests/conf/conf_power_plant_DEU_auto.json'
        params = ontomatch.utils.util.read_json_from_path(addr)
        params_str = json.dumps(params)
        config_handle = ontomatch.utils.util.call_agent_blackboard_for_writing(addr, params_str, http=False)
        print(config_handle)
        assert "_conf_conf_power_plant_DEU_auto.json" in config_handle
        object = ontomatch.utils.util.call_agent_blackboard_for_reading(config_handle, http=False)
        assert object is not None
