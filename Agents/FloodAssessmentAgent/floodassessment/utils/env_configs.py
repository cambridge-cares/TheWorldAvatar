################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 29 Nov 2022                            #
################################################

# The purpose of this module is to retrieve relevant properties and settings 
# from environment variables (i.e. set in docker-compose file)

import os
import warnings


# Initialise logger instance (ensure consistent logger level with `entrypoint.py`)
from py4jps import agentlogging
logger = agentlogging.get_logger('prod')


def retrieve_settings():
    """
        Reads settings from environment variables (as global variables).
    """

    # Define global scope for global variables
    global DATABASE, NAMESPACE, FLOODWARNINGS_TABLE, POPULATION_TABLE

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

    # Retrieve Blazegraph namespace
    NAMESPACE = os.getenv('NAMESPACE')
    if NAMESPACE is None:
        logger.error('"NAMESPACE" name is missing in environment variables.')
        raise ValueError('"NAMESPACE" name is missing in environment variables.')
    if NAMESPACE == '':
        logger.error('No "NAMESPACE" value has been provided in environment variables.')
        raise ValueError('No "NAMESPACE" value has been provided in environment variables.')
    
    # Retrieve PostGIS table name with flood warnings
    FLOODWARNINGS_TABLE = os.getenv('FLOODWARNINGS_TABLE')
    if FLOODWARNINGS_TABLE is None:
        logger.error('"FLOODWARNINGS_TABLE" name is missing in environment variables.')
        raise ValueError('"FLOODWARNINGS_TABLE" name is missing in environment variables.')
    if FLOODWARNINGS_TABLE == '':
        logger.error('No "FLOODWARNINGS_TABLE" value has been provided in environment variables.')
        raise ValueError('No "FLOODWARNINGS_TABLE" value has been provided in environment variables.')
    
    # Retrieve PostGIS table name with population density raster data
    POPULATION_TABLE = os.getenv('POPULATION_TABLE')
    if POPULATION_TABLE is None:
        logger.error('"POPULATION_TABLE" name is missing in environment variables.')
        raise ValueError('"POPULATION_TABLE" name is missing in environment variables.')
    if POPULATION_TABLE == '':
        logger.error('No "POPULATION_TABLE" value has been provided in environment variables.')
        raise ValueError('No "POPULATION_TABLE" value has been provided in environment variables.')


# Run when module is imported
retrieve_settings()
