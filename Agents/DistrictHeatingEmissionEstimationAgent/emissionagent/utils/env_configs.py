################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 21 Aug 2022                            #
################################################

# The purpose of this module is to retrieve relevant properties and settings 
# from environment variables and the stack clients (if applicable)

import os

from py4jps import agentlogging

from .stack_configs import retrieve_stack_settings


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


def retrieve_default_settings():
    """
    Reads connection settings from environment variables as global variables, 
    i.e. only global within this sub-module
    """

    # Define global scope for global variables
    global STACK_NAME, NAMESPACE, DATABASE, \
           DB_URL, DB_USER, DB_PASSWORD, SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT
    
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
    
    # Retrieve Blazegraph and PostgreSQL settings depending on deployment mode
    if not STACK_NAME:
        # 1) Standalone deployment: retrieve settings from docker compose env vars
        logger.info('No "STACK_NAME" value has been provided in environment variables. '
                    'Deploying agent in "standalone" mode.')
        
        # Retrieve global variables for connection settings
        vars_names = ['DB_URL', 'DB_USER', 'DB_PASSWORD', 
                      'SPARQL_QUERY_ENDPOINT', 'SPARQL_UPDATE_ENDPOINT']
        for v in vars_names:
            globals()[v] = os.getenv(v)
            if not globals()[v]:
                # In case variable key is missing or empty value provided
                logger.error(f'No default "{v}" value has been provided in environment variables.')
                raise ValueError(f'No default "{v}" value has been provided in environment variables.')

    else:
        # 2) Stack deployment: retrieve settings from Stack Clients
        logger.info(f'Deploying agent to stack "{STACK_NAME}".')

        # Retrieve target Blazegraph namespace to monitor and instantiate data into
        NAMESPACE = os.getenv('NAMESPACE')
        if not NAMESPACE:
            # In case variable key is missing or empty value provided
            logger.error(f'No "NAMESPACE" value has been provided in environment variables.')
            raise ValueError(f'No "NAMESPACE" value has been provided in environment variables.')

        # Retrieve target PostgreSQL database name
        DATABASE = os.getenv('DATABASE')
        if not DATABASE:
            logger.error(f'No "DB_URL" value has been provided in environment variables.')
            raise ValueError(f'No "DB_URL" value has been provided in environment variables.')
        elif DATABASE != 'postgres':
            logger.warning(f'Provided "DATABASE" name {DATABASE} does not match default database name "postgres".')
        
        # Retrieve settings from Stack Clients
        DB_URL, DB_USER, DB_PASSWORD, SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT = \
        retrieve_stack_settings(database=DATABASE, namespace=NAMESPACE)

    # Log retrieved default settings
    logger.info('Retrieved connection parameters:')
    logger.info(f"DB_URL: {DB_URL}")
    logger.info(f"DB_USER: {DB_USER}")
    logger.info(f"DB_PASSWORD: {DB_PASSWORD}")
    logger.info(f"SPARQL_QUERY_ENDPOINT: {SPARQL_QUERY_ENDPOINT}")
    logger.info(f"SPARQL_UPDATE_ENDPOINT: {SPARQL_UPDATE_ENDPOINT}")


# Run when module is imported
retrieve_default_settings()
