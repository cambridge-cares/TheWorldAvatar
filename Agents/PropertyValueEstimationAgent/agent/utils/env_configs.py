################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 23 Oct 2022                            #
################################################

# The purpose of this module is to retrieve relevant properties and settings 
# (e.g. for the Time Series Client) from environment variables (i.e. set in 
# docker-compose file)

import os
import warnings

from py4jps import agentlogging

# Initialise logger instance (ensure consistent logger level with `entrypoint.py`)
logger = agentlogging.get_logger('prod')


def retrieve_settings():
    """
        Reads settings from environment variables (as global variables).
    """

    # Define global scope for global variables
    global DATABASE, NAMESPACE, THRESHOLD

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

    # Retrieve threshold of required number of sales transactions (i.e. before
    # retrieving further data from nearest postcodes according to ONS)
    THRESHOLD = os.getenv('THRESHOLD')
    if THRESHOLD is None:
        THRESHOLD = 5
        logger.warning(f'No "THRESHOLD" value has been provided in environment variables. Using default value {THRESHOLD}.')
        warnings.warn(f'No "THRESHOLD" value has been provided in environment variables. Using default value {THRESHOLD}.')
    else:
        try:
            THRESHOLD = int(THRESHOLD)
        except ValueError:
            THRESHOLD = 5
            logger.warning(f'Invalid "THRESHOLD" value has been provided in environment variables. Using default value {THRESHOLD}.')
            warnings.warn(f'Invalid "THRESHOLD" value has been provided in environment variables. Using default value {THRESHOLD}.')


# Run when module is imported
retrieve_settings()
