# To avoid unnecessary logging information from py4j package, set logger level before 
# first creation of JPS_BASE_LIB module view (i.e. jpsBaseLibView = jpsBaseLibGW.createModuleView())

import warnings
import logging
logging.getLogger("py4j").setLevel(logging.ERROR)
# Set the logging level to suppress findfont messages
logging.getLogger('matplotlib').setLevel(logging.WARNING)
# Specify logger level to be used throughout the agent: dev, prod
logger_level = 'prod'

from py4jps import agentlogging
from pyderivationagent.conf import config_derivation_agent

from dhoptimisation.agent import default
from dhoptimisation.agent import DHOptimisationAgent
from dhoptimisation.utils.env_configs import SPARQL_QUERY_ENDPOINT, \
                                             SPARQL_UPDATE_ENDPOINT

# Initialise logger for this module (to avoid circular imports)
logger = agentlogging.get_logger(logger_level)
# Suppress FutureWarnings (i.e., from Pandas)
warnings.simplefilter(action='ignore', category=FutureWarning)


def create_app():
    # Retrieve environment variables defined in `docker-compose.yml` file
    agent_config = config_derivation_agent()

    agent = DHOptimisationAgent(
        # For details on required parameters and potential defaults, see:
        # https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_BASE_LIB/python_derivation_agent/pyderivationagent/agent/derivation_agent.py
        
        # Settings read from environment variables        
        agent_iri=agent_config.ONTOAGENT_SERVICE_IRI, 
        agent_endpoint=agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        derivation_instance_base_url=agent_config.DERIVATION_INSTANCE_BASE_URL,
        time_interval=agent_config.DERIVATION_PERIODIC_TIMESCALE,        
        register_agent=agent_config.REGISTER_AGENT,
        # Settings read from env vars/stack clients (depending on deployment mode)
        kg_url=SPARQL_QUERY_ENDPOINT,
        kg_update_url=SPARQL_UPDATE_ENDPOINT,      
        # Miscellaneous settings
        logger_name=logger_level
    )

    # Add a root web page to the agent
    agent.add_url_pattern('/', 'root', default, methods=['GET'])
    # Add a URL route to assess optimisation cost improvement
    agent.add_url_pattern('/compare_cost', 'cost', agent.compare_generation_cost, methods=['GET'])

    # Start all periodical jobs
    agent.start_all_periodical_job()

    # Expose flask app of agent
    return agent.app


if __name__ == "__main__":
    # Start the app
    app = create_app()
    app.run(host='localhost', port="5000")
    logger.info('App started')
