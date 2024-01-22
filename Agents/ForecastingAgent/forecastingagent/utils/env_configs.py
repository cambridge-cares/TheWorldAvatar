################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 14 Jul 2022                            #
################################################

# The purpose of this module is to retrieve relevant properties and settings 
# from environment variables and the stack clients (if applicable)

import os
from distutils.util import strtobool

from py4jps import agentlogging

from .stack_configs import retrieve_stack_settings


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


def retrieve_default_settings():
    """
    Reads default settings from environment variables as global variables, 
    i.e. only global within this sub-module
    """

    # Define global scope for global variables
    global STACK_NAME, NAMESPACE, DATABASE, \
           DB_URL, DB_USER, DB_PASSWORD, SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT, \
           OVERWRITE_FORECAST, ROUNDING
    
    # Initialise variables (to ensure working imports even if not defined in env vars)
    STACK_NAME = None
    NAMESPACE = None
    DATABASE = None     
    DB_URL = None
    DB_USER = None
    DB_PASSWORD = None
    SPARQL_QUERY_ENDPOINT = None
    SPARQL_UPDATE_ENDPOINT = None

    # Retrieve Docker Stack name
    STACK_NAME = os.getenv('STACK_NAME')
    if STACK_NAME is None:
        logger.error('"STACK_NAME" is missing in environment variables.')
        raise ValueError('"STACK_NAME" is missing in environment variables.')
    
    # Retrieve boolean flag whether to instantiate output forecast as new instance
    # or add data to existing instance (i.e., overwriting existing data)
    # NOTE: Overwriting/replacing previous forecast is DEFAULT behavior and most in
    #       line with intended derivation framework, meaning that an update actually
    #       replaces the previous output instead of creating a new one
    #       Hence, this setting is only here to provide more flexibility in creating 
    #       new forecasts, but should not be reproduced/copied in other derivation agents
    OVERWRITE_FORECAST = os.getenv('OVERWRITE_FORECAST')
    if OVERWRITE_FORECAST is None:
        OVERWRITE_FORECAST = True
        logger.warning(f'No "OVERWRITE_FORECAST" value has been provided in environment variables. Using default value: {OVERWRITE_FORECAST}.')
    else:
        try:
           # Cast string from docker-compose to boolean
           OVERWRITE_FORECAST = bool(strtobool(OVERWRITE_FORECAST))
        except ValueError:
            OVERWRITE_FORECAST = True
            logger.warning(f'Invalid "OVERWRITE_FORECAST" value has been provided in environment variables. Using default value: {OVERWRITE_FORECAST}.')

    # Retrieve default rounding behavior for forecast values
    # (i.e. number of target decimal places; no rounding if not defined)
    ROUNDING = os.getenv('ROUNDING')
    if ROUNDING is None:
        logger.warning('No "ROUNDING" value has been provided in environment variables. Forecasts will not be rounded.')
    else:
        try:
           # Cast string from docker-compose to int
           ROUNDING = int(ROUNDING)
        except ValueError:
            ROUNDING = None
            logger.error('Invalid "ROUNDING" value has been provided in environment variables. Forecasts will not be rounded.')

    # Retrieve Blazegraph and PostgreSQL settings depending on deployment mode
    if not STACK_NAME:
        # 1) Standalone: retrieve settings from docker compose env vars
        logger.info('No "STACK_NAME" value has been provided in environment variables. '
                    'Deploying agent in "standalone" mode.')
        
        # Retrieve global variables for default connection settings
        # Required variables
        vars_names = ['DB_USER', 'DB_PASSWORD', 'SPARQL_QUERY_ENDPOINT', 'SPARQL_UPDATE_ENDPOINT']
        for v in vars_names:
            globals()[v] = os.getenv(v)
            if not globals()[v]:
                # In case variable key is missing or empty value provided
                logger.error(f'No default "{v}" value has been provided in environment variables.')
                raise ValueError(f'No default "{v}" value has been provided in environment variables.')

        # Optional variables
        # Default "DB_URL" is optional as value will be queried from KG for time series to be forecasted
        DB_URL = os.getenv('DB_URL')
        if DB_URL is None:
            logger.warning(f'No default "DB_URL" value has been provided in environment variables.')

    else:
        # 2) Stack deployment: retrieve settings from Stack Clients
        logger.info(f'Deploying agent to stack "{STACK_NAME}".')

        # Retrieve target Blazegraph namespace to monitor and instantiate data into
        NAMESPACE = os.getenv('NAMESPACE')
        if not NAMESPACE:
            # In case variable key is missing or empty value provided
            logger.error(f'No "NAMESPACE" value has been provided in environment variables.')
            raise ValueError(f'No "NAMESPACE" value has been provided in environment variables.')

        # Initialise boolean flag whether database is given
        database_given = True

        # Retrieve target PostgreSQL database name
        DATABASE = os.getenv('DATABASE')
        if not DATABASE:
            # In case no DATABASE is provided (i.e. missing key or left blank),
            # set to empty string to avoid exception when calling stack client functions
            DATABASE = ''
            database_given = False
            logger.warning(f'No default "DB_URL" value has been provided in environment variables.')
        elif DATABASE != 'postgres':
            logger.warning(f'Provided "DATABASE" name {DATABASE} does not match default database name "postgres".')
        
        # Retrieve settings from Stack Clients
        db_url, DB_USER, DB_PASSWORD, SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT = \
        retrieve_stack_settings(database=DATABASE, namespace=NAMESPACE)
        # Assign retrieved settings to global variables
        if database_given:
            DB_URL = db_url


    # Log retrieved default settings
    logger.info('Retrieved default connection parameters:')
    logger.info(f"DB_URL: {DB_URL}")
    logger.info(f"DB_USER: {DB_USER}")
    logger.info(f"DB_PASSWORD: {DB_PASSWORD}")
    logger.info(f"SPARQL_QUERY_ENDPOINT: {SPARQL_QUERY_ENDPOINT}")
    logger.info(f"SPARQL_UPDATE_ENDPOINT: {SPARQL_UPDATE_ENDPOINT}")


# Run when module is imported
retrieve_default_settings()
