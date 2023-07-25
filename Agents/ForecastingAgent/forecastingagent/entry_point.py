# To avoid unnecessary logging information from py4j package, set logger level before 
# first creation of JPS_BASE_LIB module view (i.e. jpsBaseLibView = jpsBaseLibGW.createModuleView())

import logging
logging.getLogger("py4j").setLevel(logging.ERROR)

from py4jps import agentlogging
from pyderivationagent.conf import config_derivation_agent

from forecastingagent.utils.env_configs import SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT, \
                                               OVERWRITE_FORECAST
from forecastingagent.agent import ForecastingAgent
from forecastingagent.agent import default

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


def create_app():
    # Retrieve environment variables defined in `docker-compose.yml` file
    agent_config = config_derivation_agent()

    agent = ForecastingAgent(
        # For details on required parameters and potential defaults, see:
        # https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_BASE_LIB/python_derivation_agent/pyderivationagent/agent/derivation_agent.py
        
        # Settings read from environment variables        
        agent_iri=agent_config.ONTOAGENT_SERVICE_IRI, 
        agent_endpoint=agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        derivation_instance_base_url=agent_config.DERIVATION_INSTANCE_BASE_URL,
        time_interval=agent_config.DERIVATION_PERIODIC_TIMESCALE,        
        register_agent=agent_config.REGISTER_AGENT,
        overwrite_fc=OVERWRITE_FORECAST,
        # Settings read from env vars/stack clients (depending on deployment mode)
        kg_url=SPARQL_QUERY_ENDPOINT,
        kg_update_url=SPARQL_UPDATE_ENDPOINT,      
        # Miscellaneous settings
        logger_name='prod'
    )

    agent.add_url_pattern('/', 'root', default, methods=['GET'])

    # Expose flask app of agent
    return agent.app


if __name__ == "__main__":
    # Start the app
    app = create_app()
    app.run(host='localhost', port="5000")
    logger.info('App started')
