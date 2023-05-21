################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk)        #    
# Date: 17 May 2023                            #
################################################

# The purpose of this module is to retrieve relevant properties and settings 
# (e.g. for the Time Series Client) from environment variables (i.e. set in 
# docker-compose file)

import os
import warnings
import ast

from py4jps import agentlogging

# Initialise logger instance (ensure consistent logger level with `entrypoint.py`)
logger = agentlogging.get_logger('prod')


def retrieve_settings():
    """
        Reads settings from environment variables (as global variables).
    """

    # Define global scope for global variables
    global DATABASE, NAMESPACE, ELECTRICITY_CONSUMPTION_PROFILE, GAS_CONSUMPTION_PROFILE, COP_VAR, UPTAKE, PROPORTION_OF_HEATING, BOILER_EFFICIENCY

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
    
    # Retrieve the ELECTRICITY_CONSUMPTION_PROFILE 
    ELECTRICITY_CONSUMPTION_PROFILE = os.getenv('ELECTRICITY_CONSUMPTION_PROFILE')    
    if ELECTRICITY_CONSUMPTION_PROFILE is None:
        logger.error('"ELECTRICITY_CONSUMPTION_PROFILE" is missing in environment variables.')
        raise ValueError('"ELECTRICITY_CONSUMPTION_PROFILE" is missing in environment variables.')
    if ELECTRICITY_CONSUMPTION_PROFILE == '':
        logger.error('No "ELECTRICITY_CONSUMPTION_PROFILE" value has been provided in environment variables.')
        raise ValueError('No "ELECTRICITY_CONSUMPTION_PROFILE" value has been provided in environment variables.')
    ELECTRICITY_CONSUMPTION_PROFILE = ast.literal_eval(ELECTRICITY_CONSUMPTION_PROFILE)
    if len(ELECTRICITY_CONSUMPTION_PROFILE) != 12:
        logger.error('"ELECTRICITY_CONSUMPTION_PROFILE" value has incorrect number of length! it can only contain 12 data -- go docker-compose file to check your data')
        raise ValueError('"ELECTRICITY_CONSUMPTION_PROFILE" value has incorrect number of length! it can only contain 12 data -- go docker-compose file to check your data')
    
    # Retrieve the GAS_CONSUMPTION_PROFILE 
    GAS_CONSUMPTION_PROFILE = os.getenv('GAS_CONSUMPTION_PROFILE')    
    if GAS_CONSUMPTION_PROFILE is None:
        logger.error('"GAS_CONSUMPTION_PROFILE" is missing in environment variables.')
        raise ValueError('"GAS_CONSUMPTION_PROFILE" is missing in environment variables.')
    if GAS_CONSUMPTION_PROFILE == '':
        logger.error('No "GAS_CONSUMPTION_PROFILE" value has been provided in environment variables.')
        raise ValueError('No "GAS_CONSUMPTION_PROFILE" value has been provided in environment variables.')
    GAS_CONSUMPTION_PROFILE = ast.literal_eval(GAS_CONSUMPTION_PROFILE)
    if len(GAS_CONSUMPTION_PROFILE) != 12:
        logger.error('"GAS_CONSUMPTION_PROFILE" value has incorrect number of length! it can only contain 12 data -- go docker-compose file to check your data')
        raise ValueError('"GAS_CONSUMPTION_PROFILE" value has incorrect number of length! it can only contain 12 data -- go docker-compose file to check your data')

    # Retrieve the COP_VAR 
    COP_VAR = os.getenv('COP_VAR')    
    if COP_VAR is None:
        logger.error('"COP_VAR" is missing in environment variables.')
        raise ValueError('"COP_VAR" is missing in environment variables.')
    if COP_VAR == '':
        logger.error('No "COP_VAR" value has been provided in environment variables.')
        raise ValueError('No "COP_VAR" value has been provided in environment variables.')
    COP_VAR = str(COP_VAR)

    # Retrieve the UPTAKE 
    UPTAKE = os.getenv('UPTAKE')    
    if UPTAKE is None:
        logger.error('"UPTAKE" is missing in environment variables.')
        raise ValueError('"UPTAKE" is missing in environment variables.')
    if UPTAKE == '':
        logger.error('No "UPTAKE" value has been provided in environment variables.')
        raise ValueError('No "UPTAKE" value has been provided in environment variables.')
    UPTAKE = float(UPTAKE)

    # Retrieve the PROPORTION_OF_HEATING 
    PROPORTION_OF_HEATING = os.getenv('PROPORTION_OF_HEATING')    
    if PROPORTION_OF_HEATING is None:
        logger.error('"PROPORTION_OF_HEATING" is missing in environment variables.')
        raise ValueError('"PROPORTION_OF_HEATING" is missing in environment variables.')
    if PROPORTION_OF_HEATING == '':
        logger.error('No "PROPORTION_OF_HEATING" value has been provided in environment variables.')
        raise ValueError('No "PROPORTION_OF_HEATING" value has been provided in environment variables.')
    PROPORTION_OF_HEATING = float(PROPORTION_OF_HEATING)

    # Retrieve the BOILER_EFFICIENCY 
    BOILER_EFFICIENCY = os.getenv('BOILER_EFFICIENCY')    
    if BOILER_EFFICIENCY is None:
        logger.error('"HEATPUMP_EFFICIENCY" is missing in environment variables.')
        raise ValueError('"HEATPUMP_EFFICIENCY" is missing in environment variables.')
    if BOILER_EFFICIENCY == '':
        logger.error('No "HEATPUMP_EFFICIENCY" value has been provided in environment variables.')
        raise ValueError('No "HEATPUMP_EFFICIENCY" value has been provided in environment variables.')
    BOILER_EFFICIENCY = float(BOILER_EFFICIENCY)

# Run when module is imported
#retrieve_settings()
DATABASE = 'heatpump'
NAMESPACE = 'test'
ELECTRICITY_CONSUMPTION_PROFILE = [28.19,26.08,26.82,20.73,20.48,20.36,21.38,21.95,22.39,25.14,25.91,27.89]
GAS_CONSUMPTION_PROFILE=[7.88,7.54,7.54,4.86,4.14,3.78,3.78,3.64,4.05,6.09,6.74,8.46]
COP_VAR='Mean'
UPTAKE=0.5
PROPORTION_OF_HEATING=0.9
BOILER_EFFICIENCY=0.8