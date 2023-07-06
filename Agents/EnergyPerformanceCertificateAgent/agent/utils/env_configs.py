################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 08 Sep 2022                            #
################################################

# The purpose of this module is to retrieve relevant properties and settings 
# (i.e. for the Time Series Client) from environment variables

import os
import warnings

from py4jps import agentlogging

# Initialise logger
logger = agentlogging.get_logger("prod")


# Initialise global variables to be read from Docker compose file
global DATABASE, ONTOP_FILE, LAYERNAME, GEOSERVER_WORKSPACE, \
       OCGML_ENDPOINT, NAMESPACE, ENCODED_AUTH


def retrieve_settings():
    """
        Reads settings from environment variables (as global variables).
    """

    # Define global scope for global variables
    global DATABASE, ONTOP_FILE, LAYERNAME, GEOSERVER_WORKSPACE, \
           OCGML_ENDPOINT, NAMESPACE, ENCODED_AUTH

    # Retrieve OCGML_ENDPOINT with instantiated OntoCityGml data
    OCGML_ENDPOINT = os.getenv('OCGML_ENDPOINT')
    if OCGML_ENDPOINT is None:
        logger.error('"OCGML_ENDPOINT" is missing in environment variables.')
        raise ValueError('"OCGML_ENDPOINT" is missing in environment variables.')
    if OCGML_ENDPOINT == '':
        logger.error('No "OCGML_ENDPOINT" value has been provided in environment variables.')
        raise ValueError('No "OCGML_ENDPOINT" value has been provided in environment variables.')

    # Retrieve PostgreSQL/PostGIS database name
    DATABASE = os.getenv('DATABASE')
    if DATABASE is None:
        logger.error('"DATABASE" name is missing in environment variables.')
        raise ValueError('"DATABASE" name is missing in environment variables.')
    if DATABASE == '':
        logger.error('No "DATABASE" value has been provided in environment variables.')
        raise ValueError('No "DATABASE" value has been provided in environment variables.')
    if DATABASE != 'postgres':
        logger.warning(f'Provided "DATABASE" name {DATABASE} does not match default database name "postgres".')
        warnings.warn(f'Provided "DATABASE" name {DATABASE} does not match default database name "postgres".')

    # Retrieve PostgreSQL/PostGIS table name for geospatial information
    # PostGIS table and Geoserver layer will have same name
    LAYERNAME = os.getenv('LAYERNAME')
    if LAYERNAME is None:
        logger.error('"LAYERNAME" is missing in environment variables.')
        raise ValueError('"LAYERNAME" is missing in environment variables.')
    if LAYERNAME == '':
        logger.error('No "LAYERNAME" value has been provided in environment variables.')
        raise ValueError('No "LAYERNAME" value has been provided in environment variables.')

    # Retrieve Geoserver workspace name
    GEOSERVER_WORKSPACE = os.getenv('GEOSERVER_WORKSPACE')
    if GEOSERVER_WORKSPACE is None:
        logger.error('"GEOSERVER_WORKSPACE" name is missing in environment variables.')
        raise ValueError('"GEOSERVER_WORKSPACE" name is missing in environment variables.')
    if GEOSERVER_WORKSPACE == '':
        logger.error('No "GEOSERVER_WORKSPACE" value has been provided in environment variables.')
        raise ValueError('No "GEOSERVER_WORKSPACE" value has been provided in environment variables.')

    # Retrieve ONTOP mapping file
    ONTOP_FILE = os.getenv('ONTOP_FILE')
    if ONTOP_FILE is None:
        logger.error('"ONTOP_FILE" is missing in environment variables.')
        raise ValueError('"ONTOP_FILE" is missing in environment variables.')
    elif not os.path.exists(ONTOP_FILE):
        logger.error('Invalid "ONTOP_FILE" has been provided in environment variables.')
        raise ValueError('Invalid "ONTOP_FILE" has been provided in environment variables.')

    # Retrieve target Blazegraph name for data to instantiate
    NAMESPACE = os.getenv('NAMESPACE')
    if NAMESPACE is None:
        logger.error('"NAMESPACE" name is missing in environment variables.')
        raise ValueError('"NAMESPACE" name is missing in environment variables.')
    if NAMESPACE == '':
        logger.error('No "NAMESPACE" value has been provided in environment variables.')
        raise ValueError('No "NAMESPACE" value has been provided in environment variables.')

    # Retrieve encoded key for EPC API
    ENCODED_AUTH = os.getenv('ENCODED_AUTH')    
    if ENCODED_AUTH is None:
        logger.error('"ENCODED_AUTH" is missing in environment variables.')
        raise ValueError('"ENCODED_AUTH" is missing in environment variables.')
    if ENCODED_AUTH == '':
        logger.error('No "ENCODED_AUTH" value has been provided in environment variables.')
        raise ValueError('No "ENCODED_AUTH" value has been provided in environment variables.')

# Run when module is imported
retrieve_settings()
