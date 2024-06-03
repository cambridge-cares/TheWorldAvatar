#####################################
# Authors: Jiying Chen              #
# May, 2024                         #
#####################################

from py4jps import agentlogging
from agent.utils.stack_gateway import stackClientsGw
from agent.utils.env_configs import DATABASE, NAMESPACE

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')

def retrieve_stack_settings():
    """
    Reads settings from Stack clients with detailed error reporting
    """
     # Define global scope for global variables
    global DB_URL, DB_USER, DB_PASSWORD, \
           SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT
    try:
        stackClientsView = stackClientsGw.createModuleView()
        stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.docker.ContainerClient")
        stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig")
        stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig")

        containerClient = stackClientsView.ContainerClient()
        bg = stackClientsView.BlazegraphEndpointConfig("", "", "", "", "")
        bg_conf = containerClient.readEndpointConfig("blazegraph", bg.getClass())
        pg = stackClientsView.PostGISEndpointConfig("", "", "", "", "")
        pg_conf = containerClient.readEndpointConfig("postgis", pg.getClass())

        DB_URL = pg_conf.getJdbcURL(DATABASE)
        DB_USER = pg_conf.getUsername()
        DB_PASSWORD = pg_conf.getPassword()
        SPARQL_QUERY_ENDPOINT = bg_conf.getUrl(NAMESPACE)
        SPARQL_UPDATE_ENDPOINT = SPARQL_QUERY_ENDPOINT

        return DB_URL, DB_USER, DB_PASSWORD, SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT
    except Exception as e:
        logger.error("General Stack client parameter extraction error: {}".format(str(e)))
        raise Exception("General Stack client parameter extraction error: {}".format(str(e)))
    
# Run when module is imported
retrieve_stack_settings()