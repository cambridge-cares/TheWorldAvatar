################################################
# Authors: Jiying Chen  #
#                       #
################################################

# # # Define the database name and namespace
# DATABASE_NAME = 'postgres'
# NAMESPACE = 'gps_trajectory'


# def retrieve_stack_settings(DATABASE_NAME, NAMESPACE):
#     """
#     Reads settings from Stack clients with detailed error reporting
#     """
#     # Import stack gateway module only when needed to avoid import issues/
#     # potentially unnecessary installation of py4jps StackClients resource
#     from agent.utils.stack_gateway import stackClientsGw
#     try:
#         # Create module views to relevant Stack clients
#         try:
#             stackClientsView = stackClientsGw.createModuleView()
#         except Exception as e:
#             logger.error("Failed to create module view: {}".format(str(e)))
#             raise Exception("Failed to create module view: {}".format(str(e)))

#         try:
#             stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.docker.ContainerClient")
#             stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig")
#             stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig")
#         except Exception as e:
#             logger.error("Failed to import stack client packages: {}".format(str(e)))
#             raise Exception("Failed to import stack client packages: {}".format(str(e)))

#         # Retrieve endpoint configurations from Stack clients
#         try:
#             containerClient = stackClientsView.ContainerClient()
#         except Exception as e:
#             logger.error("Failed to instantiate ContainerClient: {}".format(str(e)))
#             raise Exception("Failed to instantiate ContainerClient: {}".format(str(e)))

#         try:
#             bg = stackClientsView.BlazegraphEndpointConfig("","","","","")
#             bg_conf = containerClient.readEndpointConfig("blazegraph", bg.getClass())
#         except Exception as e:
#             logger.error("Failed to retrieve Blazegraph configuration: {}".format(str(e)))
#             raise Exception("Failed to retrieve Blazegraph configuration: {}".format(str(e)))

#         try:
#             pg = stackClientsView.PostGISEndpointConfig("","","","","")
#             pg_conf = containerClient.readEndpointConfig("postgis", pg.getClass())
#         except Exception as e:
#             logger.error("Failed to retrieve PostGIS configuration: {}".format(str(e)))
#             raise Exception("Failed to retrieve PostGIS configuration: {}".format(str(e)))

#         try:
#             # Extract PostgreSQL database URL
#             DB_URL = pg_conf.getJdbcURL(DATABASE_NAME)
#             # Extract PostgreSQL database username and password
#             DB_USER = pg_conf.getUsername()
#             DB_PASSWORD = pg_conf.getPassword()
#         except Exception as e:
#             logger.error("Failed to extract database credentials: {}".format(str(e)))
#             raise Exception("Failed to extract database credentials: {}".format(str(e)))

#         try:
#             # Extract SPARQL endpoints of KG
#             SPARQL_QUERY_ENDPOINT = bg_conf.getUrl(NAMESPACE)
#             SPARQL_UPDATE_ENDPOINT = SPARQL_QUERY_ENDPOINT
#         except Exception as e:
#             logger.error("Failed to extract SPARQL endpoints: {}".format(str(e)))
#             raise Exception("Failed to extract SPARQL endpoints: {}".format(str(e)))

#         return DB_URL, DB_USER, DB_PASSWORD, SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT
#     except Exception as e:
#         logger.error("General Stack client parameter extraction error: {}".format(str(e)))
#         raise Exception("General Stack client parameter extraction error: {}".format(str(e)))

# The purpose of this module is to retrieve relevant settings from Stack clients

from py4jps import agentlogging


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


def retrieve_stack_settings(database, namespace):
    """
    Reads settings from Stack clients with detailed error reporting
    """
    from agent.utils.stack_gateway import stackClientsGw
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

        DB_URL = pg_conf.getJdbcURL(database)
        DB_USER = pg_conf.getUsername()
        DB_PASSWORD = pg_conf.getPassword()
        SPARQL_QUERY_ENDPOINT = bg_conf.getUrl(namespace)
        SPARQL_UPDATE_ENDPOINT = SPARQL_QUERY_ENDPOINT

        return DB_URL, DB_USER, DB_PASSWORD, SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT
    except Exception as e:
        logger.error("General Stack client parameter extraction error: {}".format(str(e)))
        raise Exception("General Stack client parameter extraction error: {}".format(str(e)))
