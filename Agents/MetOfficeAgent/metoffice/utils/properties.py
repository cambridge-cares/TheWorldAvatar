################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 08 Sep 2022                            #
################################################

# The purpose of this module is to retrieve relevant properties and settings 
# (i.e. for the Time Series Client) from environment variables and Stack clients

import os
import warnings

#import agentlogging

from metoffice.kgutils.javagateway import stackClientsGw

# Initialise logger
#logger = agentlogging.get_logger("prod")



# Initialise global variables to be read from properties file
global DATAPOINT_API_KEY
global DB_URL, DB_USER, DB_PASSWORD
global QUERY_ENDPOINT, UPDATE_ENDPOINT


def retrieve_settings():
    """
        Reads settings from environment variables and Stack clients 
        (as global variables).
    """

    # Define global scope for global variables
    global DATAPOINT_API_KEY, DB_URL, DB_USER, DB_PASSWORD, QUERY_ENDPOINT, UPDATE_ENDPOINT

    # Extract MetOffice API key
    DATAPOINT_API_KEY = os.getenv('API_KEY')    
    if DATAPOINT_API_KEY is None:
        #logger.error('"API_KEY" is missing in environment variables.')
        raise ValueError('"API_KEY" is missing in environment variables.')
    if DATAPOINT_API_KEY == '':
        #logger.error('No "API_KEY" value has been provided in environment variables.')
        raise ValueError('No "API_KEY" value has been provided in environment variables.')
    

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


    # Extract PostgreSQL/PostGIS database URL
    db_name = os.getenv('DATABASE')
    if db_name is None:
        #logger.error('"DATABASE" name is missing in environment variables.')
        raise ValueError('"DATABASE" name is missing in environment variables.')
    if db_name == '':
        #logger.error('No "DATABASE" value has been provided in environment variables.')
        raise ValueError('No "DATABASE" value has been provided in environment variables.')
    if db_name != 'postgres':
        #logger.warning(f'Provided "DATABASE" name {db_name} does not match default database name "postgres".')
        warnings.warn(f'Provided "DATABASE" name {db_name} does not match default database name "postgres".')
    DB_URL = pg_conf.getJdbcURL(db_name)
    # Extract PostgreSQL database username and password
    DB_USER = pg_conf.getUsername()
    DB_PASSWORD = pg_conf.getPassword()


    # Extract SPARQL endpoints of KG (Query and Update endpoints are equivalent
    # for Blazegraph)
    QUERY_ENDPOINT = bg_conf.getUrl("kb")
    UPDATE_ENDPOINT = QUERY_ENDPOINT


# Run when module is imported
retrieve_settings()
