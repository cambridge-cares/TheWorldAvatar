from pyderivationagent.conf import config_derivation_agent
from vapourtecscheduleagent.conf import config_vapourtec_schedule_agent
from vapourtecscheduleagent.agent import *

import logging

# Avoid unnecessary logging information from py4j package
logging.getLogger("py4j").setLevel(logging.INFO)


def create_app():
    exe_agent_config = config_vapourtec_schedule_agent()

    agent = VapourtecScheduleAgent(
        maximum_concurrent_experiment=exe_agent_config.MAXIMUM_CONCURRENT_EXPERIMENT,
        register_agent=exe_agent_config.REGISTER_AGENT,
        agent_iri=exe_agent_config.ONTOAGENT_SERVICE_IRI,
        time_interval=exe_agent_config.DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=exe_agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=exe_agent_config.SPARQL_QUERY_ENDPOINT,
        kg_update_url=exe_agent_config.SPARQL_UPDATE_ENDPOINT,
        kg_user=exe_agent_config.KG_USERNAME,
        kg_password=exe_agent_config.KG_PASSWORD,
        agent_endpoint=exe_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        logger_name="prod"
    )

    agent.add_url_pattern('/', 'root', default, methods=['GET'])

    agent.start_all_periodical_job()

    return agent.app
