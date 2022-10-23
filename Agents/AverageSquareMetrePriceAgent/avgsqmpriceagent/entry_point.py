# To avoid unnecessary logging information from py4j package, set logger level before 
# first creation of JPS_BASE_LIB module view (i.e. jpsBaseLibView = jpsBaseLibGW.createModuleView())
import logging
logging.getLogger("py4j").setLevel(logging.ERROR)

from pyderivationagent.conf import config_derivation_agent

from avgsqmpriceagent.agent import AvgSqmPriceAgent
from avgsqmpriceagent.agent import default


def create_app():
    agent_config = config_derivation_agent()

    agent = AvgSqmPriceAgent(
        register_agent=agent_config.REGISTER_AGENT,
        agent_iri=agent_config.ONTOAGENT_SERVICE_IRI, 
        time_interval=agent_config.DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=agent_config.SPARQL_QUERY_ENDPOINT,
        kg_update_url=agent_config.SPARQL_UPDATE_ENDPOINT,
        agent_endpoint=agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        logger_name='prod',
        max_thread_monitor_async_derivations=1

    )

    agent.add_url_pattern('/', 'root', default, methods=['GET'])

    agent.start_all_periodical_job()

    return agent.app
