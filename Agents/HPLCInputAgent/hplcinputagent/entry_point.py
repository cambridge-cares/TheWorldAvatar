from hplcinputagent.agent import *

import logging

# Avoid unnecessary logging information from py4j package
logging.getLogger("py4j").setLevel(logging.INFO)

def create_app():
    hplc_input_agent_config = HPLCInputAgentConfig(str(Path(__file__).absolute().parent) + '/conf/agent_properties.json')

    app = HPLCInputAgent(hplc_input_agent_config.ONTOAGENT_SERVICE, hplc_input_agent_config.DERIVATION_PERIODIC_TIMESCALE, hplc_input_agent_config.DERIVATION_INSTANCE_BASE_URL, hplc_input_agent_config.SPARQL_QUERY_ENDPOINT, logger_name='prod')
    app.add_url_pattern('/', 'root', default, methods=['GET'])

    app.start_monitoring_local_report_folder()
    flask_app = app.app
    return flask_app
