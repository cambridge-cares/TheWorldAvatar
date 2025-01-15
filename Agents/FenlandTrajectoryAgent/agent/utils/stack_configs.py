###########################################
# Authors: Jiying Chen (jc2341@cam.ac.uk) #
# May, 2024                               #
###########################################

from twa import agentlogging
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
        #stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig")
        
        # Retrieve endpoint configurations from Stack clients
        containerClient = stackClientsView.ContainerClient()

        # Blazegraph container
        bg = stackClientsView.BlazegraphEndpointConfig("", "", "", "", "")
        bg_conf = containerClient.readEndpointConfig("blazegraph", bg.getClass())

        # PostgreSQL/PostGIS container
        pg = stackClientsView.PostGISEndpointConfig("", "", "", "", "")
        pg_conf = containerClient.readEndpointConfig("postgis", pg.getClass())

        # Ontop container (not necessary in this agent)
        # ont = stackClientsView.OntopEndpointConfig("","","","","")
        # ont_conf = containerClient.readEndpointConfig("ontop", ont.getClass())

        DB_URL = pg_conf.getJdbcURL(DATABASE)
        DB_USER = pg_conf.getUsername()
        DB_PASSWORD = pg_conf.getPassword()
        SPARQL_QUERY_ENDPOINT = bg_conf.getUrl(NAMESPACE)
        SPARQL_UPDATE_ENDPOINT = SPARQL_QUERY_ENDPOINT
        # ONTOP_URL = ont_conf.getUrl()

        return DB_URL, DB_USER, DB_PASSWORD, SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT
    except Exception as e:
        logger.error("General Stack client parameter extraction error: {}".format(str(e)))
        raise Exception("General Stack client parameter extraction error: {}".format(str(e)))
    
# Run when module is imported
retrieve_stack_settings()