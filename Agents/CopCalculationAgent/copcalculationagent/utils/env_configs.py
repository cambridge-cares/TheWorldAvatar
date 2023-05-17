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
    global DATABASE, NAMESPACE, HEATPUMP_EFFICIENCY, HOTSIDE_TEMPERATURE

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

    # Retrieve the HEATPUMP_EFFICIENCY 
    HEATPUMP_EFFICIENCY_STR = os.getenv('HEATPUMP_EFFICIENCY')    
    if HEATPUMP_EFFICIENCY_STR is None:
        logger.error('"HEATPUMP_EFFICIENCY" is missing in environment variables.')
        raise ValueError('"HEATPUMP_EFFICIENCY" is missing in environment variables.')
    if HEATPUMP_EFFICIENCY_STR == '':
        logger.error('No "HEATPUMP_EFFICIENCY" value has been provided in environment variables.')
        raise ValueError('No "HEATPUMP_EFFICIENCY" value has been provided in environment variables.')
    HEATPUMP_EFFICIENCY = float(HEATPUMP_EFFICIENCY_STR)
    
    # Retrieve the HOTSIDE_TEMPERATURE 
    HOTSIDE_TEMPERATURE_STR = os.getenv('HOTSIDE_TEMPERATURE')    
    if HOTSIDE_TEMPERATURE_STR is None:
        logger.error('"HOTSIDE_TEMPERATURE" is missing in environment variables.')
        raise ValueError('"HOTSIDE_TEMPERATURE" is missing in environment variables.')
    if HOTSIDE_TEMPERATURE_STR == '':
        logger.error('No "HOTSIDE_TEMPERATURE" value has been provided in environment variables.')
        raise ValueError('No "HOTSIDE_TEMPERATURE" value has been provided in environment variables.')
    HOTSIDE_TEMPERATURE = float(HOTSIDE_TEMPERATURE_STR)

# Run when module is imported
# retrieve_settings()
DATABASE = 'buildings'
NAMESPACE = 'heatpump'
HEATPUMP_EFFICIENCY =  0.35
HOTSIDE_TEMPERATURE = 318.15
