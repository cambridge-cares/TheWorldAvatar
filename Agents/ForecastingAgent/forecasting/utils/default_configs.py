################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 08 Dec 2022                            #
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
        Reads default settings from environment variables as global variables, 
        i.e. only global within this sub-module
    """

    # Define global scope for global variables
    global NAMESPACE, DATABASE, STACK_NAME, \
           DB_URL, DB_USER, DB_PASSWORD, QUERY_ENDPOINT, UPDATE_ENDPOINT
    
    # Initialise variables (to ensure working imports even if not defined in env vars)
    NAMESPACE = None
    DATABASE = None 
    STACK_NAME = None
    DB_URL = None
    DB_USER = None
    DB_PASSWORD = None
    QUERY_ENDPOINT = None
    UPDATE_ENDPOINT = None

    # Retrieve Docker Stack name
    STACK_NAME = os.getenv('STACK_NAME')
    if STACK_NAME is None:
        logger.error('"STACK_NAME" is missing in environment variables.')
        raise ValueError('"STACK_NAME" is missing in environment variables.')

    # Retrieve Blazegraph and PostgreSQL/PostGIS settings depending on deployment mode
    if STACK_NAME == '':
        # 1) Standalone: retrieve settings from docker compose env vars
        logger.info('No "STACK_NAME" value has been provided in environment variables. '
                    'Deploying agent in "standalone" mode.')
        
        # Retrieve global variables for default connection settings
        vars_names = ['DB_URL', 'DB_USER', 'DB_PASSWORD', 'QUERY_ENDPOINT', 'UPDATE_ENDPOINT']
        for v in vars_names:
            globals()[v] = os.getenv(v)
            if globals()[v] is None:
                logger.error(f'"{v}" is missing in environment variables.')
                raise ValueError(f'"{v}" is missing in environment variables.')
            if globals()[v] == '':
                warning_msg = f'No default "{v}" value has been provided in environment variables. '
                warning_msg += f'Ensure that "{v}" is provided in HTTP request to the agent to avoid issues.'
                logger.warning(warning_msg)

    else:
        # 2) Stack deployment: retrieve settings from Stack Clients
        logger.info('Deploying agent to stack "{STACK_NAME}".')

        # Retrieve target Blazegraph namespace for data to instantiate
        NAMESPACE = os.getenv('NAMESPACE')
        if NAMESPACE is None:
            logger.error('"NAMESPACE" name is missing in environment variables.')
            raise ValueError('"NAMESPACE" name is missing in environment variables.')
        if NAMESPACE == '':
            warning_msg = f'No default "NAMESPACE" value has been provided in environment variables. '
            warning_msg += f'Ensure that "NAMESPACE" is provided in HTTP request to the agent to avoid issues.'
            logger.warning(warning_msg)

        # Retrieve target PostgreSQL/PostGIS database name
        DATABASE = os.getenv('DATABASE')
        if DATABASE is None:
            logger.error('"DATABASE" name is missing in environment variables.')
            raise ValueError('"DATABASE" name is missing in environment variables.')
        if DATABASE == '':
            warning_msg = f'No default "DATABASE" value has been provided in environment variables. '
            warning_msg += f'Ensure that "DATABASE" is provided in HTTP request to the agent to avoid issues.'
            logger.warning(warning_msg)
        elif DATABASE != 'postgres':
            logger.warning(f'Provided "DATABASE" name {DATABASE} does not match default database name "postgres".')
        
        # Retrieve settings from Stack Clients
        # TODO: Review and check what happens/is returned if empty strings are provided
        DB_URL, DB_USER, DB_PASSWORD, QUERY_ENDPOINT, UPDATE_ENDPOINT = \
        retrieve_stack_settings(database=DATABASE,namespace=NAMESPACE)

        logger.info(f"DB_URL: {DB_URL}")
        logger.info(f"DB_USER: {DB_USER}")
        logger.info(f"DB_PASSWORD: {DB_PASSWORD}")
        logger.info(f"QUERY_ENDPOINT: {QUERY_ENDPOINT}")
        logger.info(f"UPDATE_ENDPOINT: {UPDATE_ENDPOINT}")


# Run when module is imported
retrieve_default_settings()
