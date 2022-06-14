from pyderivationagent.conf import config_derivation_agent
from agilentagent.agent import AgilentAgent
from agilentagent.agent import default
from agilentagent.conf import config_agilent

import logging

# Avoid unnecessary logging information from py4j package
logging.getLogger("py4j").setLevel(logging.INFO)

def create_app():
    derivation_agent_config = config_derivation_agent()
    hplc_config = config_agilent()

    agent = AgilentAgent(
        hplc_digital_twin=hplc_config.HPLC_DIGITAL_TWIN,
        hplc_report_periodic_timescale=hplc_config.HPLC_REPORT_PERIODIC_TIMESCALE,
        hplc_report_container_dir=hplc_config.HPLC_REPORT_CONTAINER_DIR,
        hplc_report_file_extension=hplc_config.HPLC_REPORT_FILE_EXTENSION,
        register_agent=hplc_config.REGISTER_AGENT,
        agent_iri=derivation_agent_config.ONTOAGENT_SERVICE_IRI,
        time_interval=derivation_agent_config.DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=derivation_agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=derivation_agent_config.SPARQL_QUERY_ENDPOINT,
        kg_update_url=derivation_agent_config.SPARQL_UPDATE_ENDPOINT,
        kg_user=derivation_agent_config.KG_USERNAME,
        kg_password=derivation_agent_config.KG_PASSWORD,
        fs_url=derivation_agent_config.FILE_SERVER_ENDPOINT,
        fs_user=derivation_agent_config.FILE_SERVER_USERNAME,
        fs_password=derivation_agent_config.FILE_SERVER_PASSWORD,
        agent_endpoint=derivation_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        # logger_name='prod'
    )

    agent.add_url_pattern('/', 'root', default, methods=['GET'])

    agent.register()
    agent.start_monitoring_derivations()
    agent.start_monitoring_local_report_folder()

    return agent.app
