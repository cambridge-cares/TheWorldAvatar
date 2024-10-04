################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk)        #    
# Date: 17 May 2023                            #
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
    global DATABASE, NAMESPACE, ELECTRICITY_UNIT_COST, GAS_UNIT_COST, YEAR
    
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

    # Retrieve the ELECTRICITY_UNIT_COST 
    ELECTRICITY_UNIT_COST = os.getenv('ELECTRICITY_UNIT_COST')    
    if ELECTRICITY_UNIT_COST is None:
        logger.error('"ELECTRICITY_UNIT_COST" is missing in environment variables.')
        raise ValueError('"ELECTRICITY_UNIT_COST" is missing in environment variables.')
    if ELECTRICITY_UNIT_COST == '':
        logger.error('No "ELECTRICITY_UNIT_COST" value has been provided in environment variables.')
        raise ValueError('No "ELECTRICITY_UNIT_COST" value has been provided in environment variables.')
    ELECTRICITY_UNIT_COST = float(ELECTRICITY_UNIT_COST)

    # Retrieve the GAS_UNIT_COST 
    GAS_UNIT_COST = os.getenv('GAS_UNIT_COST')    
    if GAS_UNIT_COST is None:
        logger.error('"GAS_UNIT_COST" is missing in environment variables.')
        raise ValueError('"GAS_UNIT_COST" is missing in environment variables.')
    if GAS_UNIT_COST == '':
        logger.error('No "GAS_UNIT_COST" value has been provided in environment variables.')
        raise ValueError('No "GAS_UNIT_COST" value has been provided in environment variables.')
    GAS_UNIT_COST = float(GAS_UNIT_COST)

    # Retrieve the YEAR 
    YEAR = os.getenv('YEAR')    
    if YEAR is None:
        logger.error('"YEAR" is missing in environment variables.')
        raise ValueError('"YEAR" is missing in environment variables.')
    if YEAR == '':
        logger.error('No "YEAR" value has been provided in environment variables.')
        raise ValueError('No "YEAR" value has been provided in environment variables.')
    YEAR = str(YEAR)


# Run when module is imported
retrieve_settings()
