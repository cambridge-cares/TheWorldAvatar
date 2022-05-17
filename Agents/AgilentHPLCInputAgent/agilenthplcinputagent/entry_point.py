from agilenthplcinputagent.agent import *

import logging

# Avoid unnecessary logging information from py4j package
logging.getLogger("py4j").setLevel(logging.INFO)

def create_app():
    hplc_input_agent_config = HPLCInputAgentConfig(str(Path(__file__).absolute().parent) + '/conf/agent_properties.json')

    app = AgilentHPLCInputAgent(
        hplc_digital_twin=hplc_input_agent_config.HPLC_DIGITAL_TWIN, hplc_report_periodic_timescale=hplc_input_agent_config.HPLC_REPORT_PERIODIC_TIMESCALE,
        file_server_upload=hplc_input_agent_config.FILE_SERVER_UPLOAD,
        agent_iri=hplc_input_agent_config.ONTOAGENT_SERVICE, time_interval=hplc_input_agent_config.DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=hplc_input_agent_config.DERIVATION_INSTANCE_BASE_URL, kg_url=hplc_input_agent_config.SPARQL_QUERY_ENDPOINT, logger_name='prod'
    )

    app.add_url_pattern('/', 'root', default, methods=['GET'])

    app.start_monitoring_local_report_folder()
    flask_app = app.app
    return flask_app
