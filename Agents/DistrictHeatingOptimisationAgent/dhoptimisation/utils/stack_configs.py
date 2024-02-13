################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #
# Date: 08 Dec 2022                            #
################################################

# The purpose of this module is to retrieve relevant settings from Stack clients


def retrieve_stack_settings(database, namespace):
    """
    Reads settings from Stack clients
    """

    # Import stack gateway module only when needed to avoid import issues/
    # potentially unnecessary installation of py4jps StackClients resource
    from .stack_gateway import stackClientsGw

    # Create module views to relevant Stack clients
    stackClientsView = stackClientsGw.createModuleView()
    stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.docker.ContainerClient")
    stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig")
    stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig")

    # Retrieve endpoint configurations from Stack clients
    containerClient = stackClientsView.ContainerClient()
    # Blazegraph
    bg = stackClientsView.BlazegraphEndpointConfig("","","","","")
    bg_conf = containerClient.readEndpointConfig("blazegraph", bg.getClass())
    # PostgreSQL/PostGIS
    pg = stackClientsView.PostGISEndpointConfig("","","","","")
    pg_conf = containerClient.readEndpointConfig("postgis", pg.getClass())

    # Extract PostgreSQL database URL
    DB_URL = pg_conf.getJdbcURL(database)
    # Extract PostgreSQL database username and password
    DB_USER = pg_conf.getUsername()
    DB_PASSWORD = pg_conf.getPassword()

    # Extract SPARQL endpoints of KG 
    # (i.e. Query and Update endpoints are equivalent for Blazegraph)
    SPARQL_QUERY_ENDPOINT = bg_conf.getUrl(namespace)
    SPARQL_UPDATE_ENDPOINT = SPARQL_QUERY_ENDPOINT

    return DB_URL, DB_USER, DB_PASSWORD, SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT
