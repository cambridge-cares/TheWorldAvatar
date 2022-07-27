from pyderivationagent.conf import config_derivation_agent
from vapourtecagent.agent import VapourtecAgent
from vapourtecagent.agent import default
from vapourtecagent.conf import config_vapourtec

import logging

# Avoid unnecessary logging information from py4j package
logging.getLogger("py4j").setLevel(logging.INFO)


def create_app():
    derivation_agent_config = config_derivation_agent()
    vapourtec_config = config_vapourtec()

    agent = VapourtecAgent(
        vapourtec_digital_twin=vapourtec_config.VAPOURTEC_DIGITAL_TWIN,
        vapourtec_state_periodic_timescale=vapourtec_config.VAPOURTEC_STATE_PERIODIC_TIMESCALE,
        vapourtec_ip_address=vapourtec_config.VAPOURTEC_IP_ADDRESS,
        fcexp_file_container_folder=vapourtec_config.FCEXP_FILE_CONTAINER_FOLDER,
        fcexp_file_host_folder=vapourtec_config.FCEXP_FILE_HOST_FOLDER,
        fcexp_template_filename=vapourtec_config.FCEXP_TEMPLATE_FILENAME,
        dry_run=vapourtec_config.DRY_RUN,
        register_agent=vapourtec_config.REGISTER_AGENT,
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
    agent.add_job_monitoring_derivations()
    agent.add_job_monitoring_vapourtec_rs400_state(start=True)

    return agent.app
