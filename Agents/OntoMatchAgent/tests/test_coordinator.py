import json
import logging

import ontomatch.coordinator
import ontomatch.utils.util
import tests.utils_for_testing

class TestCoordinator(tests.utils_for_testing.TestCaseOntoMatch):

    def test_coordinator_step_1_loading_and_step_2_adding_knowledge(self):

        config_file = tests.utils_for_testing.PATH_CONF_PP_DEU_AUTO
        params = ontomatch.utils.util.read_json_from_path(config_file)
        # the config file defines the pickled files as src and tgt and thus will not add geocoordinates
        # thus we replace them here by the original turtle files
        params['dataset']['src'] = './data/power_plant_DEU/kwl.ttl'
        params['dataset']['tgt'] = './data/power_plant_DEU/gppd_DEU.ttl'
        params['pre_processing']['add_knowledge'] = 'ontomatch.knowledge.geocoding'
        # outcomment the next line if the full pipeline should be performed
        # None means that no instance matching is performed
        params['matching']['name'] = None

        # write the config params to the blackboard
        params_str = json.dumps(params)
        config_handle = ontomatch.utils.util.call_agent_blackboard_for_writing(config_file, params_str, http=False)

        agent = ontomatch.coordinator.Agent()
        agent.start(config_handle, http=False)

    def test_coordinator_step_3_instance_matching_and_step_4_evaluation(self):

        config_file = tests.utils_for_testing.PATH_CONF_PP_DEU_AUTO
        params = ontomatch.utils.util.read_json_from_path(config_file)
        # since the config file defines pickled files as src and tgt,
        # step 1 loading and step 2 adding geo coordinates are omitted

        # write the config params to the blackboard
        params_str = json.dumps(params)
        config_handle = ontomatch.utils.util.call_agent_blackboard_for_writing(config_file, params_str, http=False)

        agent = ontomatch.coordinator.Agent()
        agent.start(config_handle, http=False)

    def test_instance_matching_with_XGB(self):

        config_file = tests.utils_for_testing.PATH_CONF_PP_DEU_AUTO
        params = ontomatch.utils.util.read_json_from_path(config_file)
        # since the config file defines pickled files as src and tgt,
        # step 1 loading and step 2 adding geo coordinates are omitted

        params['matching']['name'] = 'instancematching.InstanceMatcherXGB'

        # write the config params to the blackboard
        params_str = json.dumps(params)
        config_handle = ontomatch.utils.util.call_agent_blackboard_for_writing(config_file, params_str, http=False)

        agent = ontomatch.coordinator.Agent()
        agent.start(config_handle, http=False)
