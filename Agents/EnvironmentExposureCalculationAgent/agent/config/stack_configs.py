###########################################
# Authors: Jiying Chen (jc2341@cam.ac.uk) #
# May, 2024                               #
###########################################
from typing import cast
from flask import current_app
from twa import agentlogging, JPSGateway

from agent.stack.postgis_client import DBConfig

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')      

def retrieve_stack_settings(stack_client: JPSGateway):
    """
    Reads settings from Stack clients with detailed error reporting
    """
    try:
        stack_clients_view = stack_client.createModuleView()
        stack_client.importPackages(stack_clients_view, "com.cmclinnovations.stack.clients.docker.ContainerClient")
        stack_client.importPackages(stack_clients_view, "com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig")
        stack_client.importPackages(stack_clients_view, "com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig")
        stack_client.importPackages(stack_clients_view, "com.cmclinnovations.stack.clients.ontop.OntopEndpointConfig")
        stack_client.importPackages(stack_clients_view, "com.cmclinnovations.stack.clients.ontop.OntopClient")
        
        # Retrieve endpoint configurations from Stack clients
        containerClient = stack_clients_view.ContainerClient()

        # PostgreSQL/PostGIS container
        pg = stack_clients_view.PostGISEndpointConfig("", "", "", "", "")
        pg_conf = containerClient.readEndpointConfig("postgis", pg.getClass())
        
        # ontop_conf = 'http://sea-level-ontop:8080'

        DB_CONF = DBConfig(pg_conf=pg_conf)
        # ONTOP_URL = ontop_conf.getUrl()
        ONTOP_URL = 'http://sea-level-ontop:8080/sparql'

        return DB_CONF, ONTOP_URL
    except Exception as e:
        logger.error("General Stack client parameter extraction error: {}".format(str(e)))
        raise Exception("General Stack client parameter extraction error: {}".format(str(e)))