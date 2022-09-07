################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 31 Mar 2022                            #
################################################

# The purpose of this module is to read the properties file with relevant
# settings (i.e. for the Time Series Client) and the MetOffice API key

import os

#import agentlogging
from configobj import ConfigObj
from pathlib import Path

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
        #logger.error('API_KEY is missing in environment variables')
        raise ValueError('API_KEY is missing in environment variables')
    if DATAPOINT_API_KEY == '':
        #logger.error('No "API_KEY" value has been provided in environment variables')
        raise ValueError('No "API_KEY" value has been provided in environment variables')
    

    # Create module views to relevant Stack clients
    stackClientsView = stackClientsGw.createModuleView()
    stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.docker.ContainerClient")
    stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.blazegraph.BlazegraphEndpointConfig")
    stackClientsGw.importPackages(stackClientsView, "com.cmclinnovations.stack.clients.postgis.PostGISEndpointConfig")

    # Retrieve endpoint configurations from Stack clients
    containerClient = stackClientsView.ContainerClient()
    bg = stackClientsView.BlazegraphEndpointConfig("","","","","")
    bg_conf = containerClient.readEndpointConfig("blazegraph", bg.getClass())

    pg = stackClientsView.PostGISEndpointConfig("","","","","")
    pg_conf = containerClient.readEndpointConfig("postgis", pg.getClass())


    # Extract PostgreSQL database URL
    db_name = os.getenv('DATABASE')
    #TODO: elaborate
    # if db_name is None:
    #     #logger.error('API_KEY is missing in environment variables')
    #     raise ValueError('API_KEY is missing in environment variables')
    # if db_name != '':
    #     #logger.error('No "API_KEY" value has been provided in environment variables')
    #     raise ValueError('No "API_KEY" value has been provided in environment variables')
    DB_URL = pg_conf.getJdbcURL(db_name)
    # except KeyError:
    #     #logger.error('Key "db.url" is missing in properties file: ' + filepath)
    #     raise KeyError('Key "db.url" is missing in properties file: ' + filepath)
    # if DB_URL == '':
    #     #logger.error('No "db.url" value has been provided in properties file: ' + filepath)
    #     raise KeyError('No "db.url" value has been provided in properties file: ' + filepath)

    # Extract PostgreSQL database username
    DB_USER = pg_conf.getUsername()
    # except KeyError:
    #     #logger.error('Key "db.user" is missing in properties file: ' + filepath)
    #     raise KeyError('Key "db.user" is missing in properties file: ' + filepath)
    # if DB_USER == '':
    #     #logger.error('No "db.user" value has been provided in properties file: ' + filepath)
    #     raise KeyError('No "db.user" value has been provided in properties file: ' + filepath)

    # Extract PostgreSQL database password
    DB_PASSWORD = pg_conf.getPassword()
    # except KeyError:
    #     #logger.error('Key "db.password" is missing in properties file: ' + filepath)
    #     raise KeyError('Key "db.password" is missing in properties file: ' + filepath)
    # if DB_PASSWORD == '':
    #     #logger.error('No "db.password" value has been provided in properties file: ' + filepath)
    #     raise KeyError('No "db.password" value has been provided in properties file: ' + filepath)

    # Extract SPARQL Query endpoint of KG
    QUERY_ENDPOINT = bg_conf.getUrl("kb")
    # except KeyError:
    #     #logger.error('Key "sparql.query.endpoint" is missing in properties file: ' + filepath)
    #     raise KeyError('Key "sparql.query.endpoint" is missing in properties file: ' + filepath)
    # if QUERY_ENDPOINT == '':
    #     #logger.error('No "sparql.query.endpoint" value has been provided in properties file: ' + filepath)
    #     raise KeyError('No "sparql.query.endpoint" value has been provided in properties file: ' + filepath)

    # Extract SPARQL Update endpoint of KG
    UPDATE_ENDPOINT = bg_conf.getUrl("kb")
    # except KeyError:
    #     #logger.error('Key "sparql.update.endpoint" is missing in properties file: ' + filepath)
    #     raise KeyError('Key "sparql.update.endpoint" is missing in properties file: ' + filepath)
    # if UPDATE_ENDPOINT == '':
    #     #logger.error('No "sparql.update.endpoint" value has been provided in properties file: ' + filepath)
    #     raise KeyError('No "sparql.update.endpoint" value has been provided in properties file: ' + filepath)


# Run when module is imported
retrieve_settings()
