################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 11 Jul 2022                            #
################################################

# The purpose of this module is to retrieve relevant settings from Stack clients

from py4jps import agentlogging

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


def retrieve_stack_settings(namespace):
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

    # Retrieve endpoint configurations from Stack clients
    containerClient = stackClientsView.ContainerClient()
    # Blazegraph
    bg = stackClientsView.BlazegraphEndpointConfig("","","","","")
    bg_conf = containerClient.readEndpointConfig("blazegraph", bg.getClass())

    # Extract SPARQL endpoints of KG 
    # (i.e. Query and Update endpoints are equivalent for Blazegraph)
    QUERY_ENDPOINT = bg_conf.getUrl(namespace)
    UPDATE_ENDPOINT = QUERY_ENDPOINT

    return QUERY_ENDPOINT, UPDATE_ENDPOINT
