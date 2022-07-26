from pyderivationagent.conf import config_derivation_agent
from vtexeagent.conf import config_vapourtec_execution
from vtexeagent.agent import *

import logging

# Avoid unnecessary logging information from py4j package
logging.getLogger("py4j").setLevel(logging.INFO)


def create_app():
    agent_config = config_derivation_agent()
    exe_config = config_vapourtec_execution()

    agent = VapourtecExecutionAgent(
        maximum_concurrent_experiment=exe_config.MAXIMUM_CONCURRENT_EXPERIMENT,
        register_agent=exe_config.REGISTER_AGENT,
        agent_iri=agent_config.ONTOAGENT_SERVICE_IRI,
        time_interval=agent_config.DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=agent_config.SPARQL_QUERY_ENDPOINT,
        kg_update_url=agent_config.SPARQL_UPDATE_ENDPOINT,
        kg_user=agent_config.KG_USERNAME,
        kg_password=agent_config.KG_PASSWORD,
        agent_endpoint=agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        logger_name="prod"
    )

    agent.add_url_pattern('/', 'root', default, methods=['GET'])

    agent.register()
    agent.add_job_monitoring_derivations(start=True)

    return agent.app
