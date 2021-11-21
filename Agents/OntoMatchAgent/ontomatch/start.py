import json
import logging
import sys

import ontomatch.coordinator
import ontomatch.evaluate
import ontomatch.instancematching
import ontomatch.scoring
import ontomatch.utils.blackboard
import ontomatch.utils.util

def start_pickle_dump():
    file = './data/power_plant_DEU/kwl.ttl'
    tgt_file = ontomatch.utils.blackboard.LOCAL_BLACKBOARD_DIR + '/kwl.pkl'
    onto = ontomatch.utils.util.load_ontology(file, blackboard=False)
    ontomatch.utils.util.pickle_dump(tgt_file, onto)

def start_coordinate():
    # http = False: agent call each other by direct Python function calls instead of HTTP requests
    # http = True: HTTP requests are used for calling agents
    http = False

    #use this config file to test the complete pipeline including knowledge enrichment
    config_file = './tests/conf/conf_power_plant_DEU_auto_no_pickl.json'
    #use this config file to test instance matching with the pickled RDF graph including geocoordinates
    #config_file = './tests/conf/conf_power_plant_DEU_auto.json'

    sys.argv.extend(['--config', config_file])
    params, _ = ontomatch.utils.util.init()

    # set matching name to None to finish without instance matching
    # this option is for testing knowledge enrichment only
    #params['matching']['name'] = None

    # write the config params to the blackboard
    params_str = json.dumps(params)
    config_handle = ontomatch.utils.util.call_agent_blackboard_for_writing(config_file, params_str, http)

    call_agent_coordinator(config_handle, http)

def call_agent_coordinator(config_handle, http=False):
    logging.info('calling ontomatch.coordinator.Agent, config_handle=%s, http=%s', config_handle, http)
    if http:
        raise NotImplementedError()
    else:
        ontomatch.coordinator.Agent().start(config_handle, http=False)
    logging.info('called ontomatch.coordinator.Agent')

if __name__ == '__main__':

    #start_pickle_dump()

    start_coordinate()
