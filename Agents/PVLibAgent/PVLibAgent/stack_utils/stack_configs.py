# The purpose of this module is to retrieve relevant properties and settings 
# (e.g. for the Time Series Client) from Stack clients

from PVLibAgent.kg_utils.jpsSingletons import stackClientsGw

# Initialise logger
from py4jps import agentlogging
logger = agentlogging.get_logger("prod")


def retrieve_settings():
    """
        Reads settings Stack clients (as global variables).
    """

    # Define global scope for global variables
    global DB_UPDATE_URL_STACK, DB_UPDATE_USER_STACK, DB_UPDATE_PASSWORD_STACK, DB_QUERY_URL_STACK, DB_QUERY_USER_STACK, DB_QUERY_PASSWORD_STACK, QUERY_ENDPOINT_STACK, UPDATE_ENDPOINT_STACK, ONTOP_URL_STACK
    
    # Create module views to relevant Stack clients
    stackClientsView = stackClientsGw.createModuleView()
    stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.docker.ContainerClient")
    stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig")
    stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig")
    stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig")

    # Retrieve endpoint configurations from Stack clients
    containerClient = stackClientsView.ContainerClient()
    # Blazegraph
    bg = stackClientsView.BlazegraphEndpointConfig("","","","","")
    bg_conf = containerClient.readEndpointConfig("blazegraph", bg.getClass())
    # PostgreSQL/PostGIS
    pg = stackClientsView.PostGISEndpointConfig("","","","","")
    pg_conf_query = containerClient.readEndpointConfig("postgis", pg.getClass())
    pg_conf_update = containerClient.readEndpointConfig("postgis", pg.getClass())
    # Ontop
    ont = stackClientsView.OntopEndpointConfig("","","","","")
    ont_conf = containerClient.readEndpointConfig("ontop", ont.getClass())

    # Extract PostgreSQL database URL for querying weather data
    DB_QUERY_URL_STACK = pg_conf_query.getJdbcURL('openmeteo')
    # Extract PostgreSQL database username and password
    DB_QUERY_USER_STACK = pg_conf_query.getUsername()
    DB_QUERY_PASSWORD_STACK = pg_conf_query.getPassword()
    # Extract PostgreSQL database URL for updating computed power value
    #DB_UPDATE_URL_STACK = pg_conf_update.getJdbcURL('PVLib')
    DB_UPDATE_URL_STACK = pg_conf_update.getJdbcURL('ntuenergy')
    DB_UPDATE_USER_STACK = pg_conf_update.getUsername()
    DB_UPDATE_PASSWORD_STACK = pg_conf_update.getPassword()

    # Extract SPARQL endpoints of KG 
    # (i.e. Query and Update endpoints are equivalent for Blazegraph)
    QUERY_ENDPOINT_STACK = bg_conf.getUrl('openmeteo')
    UPDATE_ENDPOINT_STACK = bg_conf.getUrl('ntuenergy')

    # Extract ONTOP endpoint
    ONTOP_URL_STACK = ont_conf.getUrl()


# Run when module is imported
retrieve_settings()