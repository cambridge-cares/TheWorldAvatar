################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 12 Oct 2022                            #
################################################

# The purpose of this module is to retrieve relevant properties and settings 
# (i.e. for the Time Series Client) from environment variables

import os
import warnings

# Initialise logger
from py4jps import agentlogging
logger = agentlogging.get_logger("prod")


def retrieve_settings():
    """
    Reads settings from environment variables (as global variables).
    """

    # Define global scope for global variables
    global DATABASE, NAMESPACE, LAYERNAME, GEOSERVER_WORKSPACE, BUILDINGS_TABLE,\
           AVERAGE_SQUARE_METRE_PRICE_AGENT_IRI, AVERAGE_SQUARE_METRE_PRICE_AGENT_URL,\
           PROPERTY_VALUE_ESTIMATION_AGENT_IRI, PROPERTY_VALUE_ESTIMATION_AGENT_URL

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

    # Retrieve target Blazegraph name for data to instantiate
    NAMESPACE = os.getenv('NAMESPACE')
    if NAMESPACE is None:
        logger.error('"NAMESPACE" name is missing in environment variables.')
        raise ValueError('"NAMESPACE" name is missing in environment variables.')
    if NAMESPACE == '':
        logger.error('No "NAMESPACE" value has been provided in environment variables.')
        raise ValueError('No "NAMESPACE" value has been provided in environment variables.')

    # Retrieve target PostgreSQL/PostGIS table name for building sales information
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
    
    # Retrieve PostGIS table name holding the (previously instantiated) geospatial 
    # building information (i.e. building footprint polygons) 
    BUILDINGS_TABLE = os.getenv('BUILDINGS_TABLE')
    if BUILDINGS_TABLE is None:
        logger.error('"BUILDINGS_TABLE" is missing in environment variables.')
        raise ValueError('"BUILDINGS_TABLE" is missing in environment variables.')
    if BUILDINGS_TABLE == '':
        logger.error('No "BUILDINGS_TABLE" value has been provided in environment variables.')
        raise ValueError('No "BUILDINGS_TABLE" value has been provided in environment variables.')
    
    # Retrieve derivation agent settings
    AVERAGE_SQUARE_METRE_PRICE_AGENT_IRI = os.getenv('AVERAGE_SQUARE_METRE_PRICE_AGENT_IRI')
    if AVERAGE_SQUARE_METRE_PRICE_AGENT_IRI is None or AVERAGE_SQUARE_METRE_PRICE_AGENT_IRI == '':
        logger.error('No "AVERAGE_SQUARE_METRE_PRICE_AGENT_IRI" could be retrieved from environment variables.')
        raise ValueError('No "AVERAGE_SQUARE_METRE_PRICE_AGENT_IRI" could be retrieved from environment variables.')
    AVERAGE_SQUARE_METRE_PRICE_AGENT_URL = os.getenv('AVERAGE_SQUARE_METRE_PRICE_AGENT_URL')
    if AVERAGE_SQUARE_METRE_PRICE_AGENT_URL is None or AVERAGE_SQUARE_METRE_PRICE_AGENT_URL == '':
        logger.error('No "AVERAGE_SQUARE_METRE_PRICE_AGENT_URL" could be retrieved from environment variables.')
        raise ValueError('No "AVERAGE_SQUARE_METRE_PRICE_AGENT_URL" could be retrieved from environment variables.')
    PROPERTY_VALUE_ESTIMATION_AGENT_IRI = os.getenv('PROPERTY_VALUE_ESTIMATION_AGENT_IRI')
    if PROPERTY_VALUE_ESTIMATION_AGENT_IRI is None or PROPERTY_VALUE_ESTIMATION_AGENT_IRI == '':
        logger.error('No "PROPERTY_VALUE_ESTIMATION_AGENT_IRI" could be retrieved from environment variables.')
        raise ValueError('No "PROPERTY_VALUE_ESTIMATION_AGENT_IRI" could be retrieved from environment variables.')
    PROPERTY_VALUE_ESTIMATION_AGENT_URL = os.getenv('PROPERTY_VALUE_ESTIMATION_AGENT_URL')
    if PROPERTY_VALUE_ESTIMATION_AGENT_URL is None or PROPERTY_VALUE_ESTIMATION_AGENT_URL == '':
        logger.error('No "PROPERTY_VALUE_ESTIMATION_AGENT_URL" could be retrieved from environment variables.')
        raise ValueError('No "PROPERTY_VALUE_ESTIMATION_AGENT_URL" could be retrieved from environment variables.')


# Run when module is imported
retrieve_settings()
