################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 08 Dec 2022                            #
################################################

# The purpose of this module is to retrieve relevant properties and settings 
# from environment variables

import os

from py4jps import agentlogging

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')

def retrieve_settings():
    """
        Reads settings from environment variables (as global variables).
    """

    # Define global scope for global variables
    global NAMESPACE, DATABASE, STACK_NAME, \
           db_url, db_user, db_password, query_endpoint, update_endpoint
    
    # Initialise potentially not provided variables
    db_url = None
    db_user = None
    db_password = None
    query_endpoint = None
    update_endpoint = None

    # Retrieve target Blazegraph name for data to instantiate
    NAMESPACE = os.getenv('NAMESPACE')
    if NAMESPACE is None:
        logger.error('"NAMESPACE" name is missing in environment variables.')
        raise ValueError('"NAMESPACE" name is missing in environment variables.')
    if NAMESPACE == '':
        logger.error('No "NAMESPACE" value has been provided in environment variables.')
        raise ValueError('No "NAMESPACE" value has been provided in environment variables.')

    # Retrieve target PostgreSQL/PostGIS database name
    DATABASE = os.getenv('DATABASE')
    if DATABASE is None:
        logger.error('"DATABASE" name is missing in environment variables.')
        raise ValueError('"DATABASE" name is missing in environment variables.')
    if DATABASE == '':
        logger.error('No "DATABASE" value has been provided in environment variables.')
        raise ValueError('No "DATABASE" value has been provided in environment variables.')
    if DATABASE != 'postgres':
        logger.warning(f'Provided "DATABASE" name {DATABASE} does not match default database name "postgres".')

    # Retrieve Docker Stack name
    STACK_NAME = os.getenv('STACK_NAME')
    if STACK_NAME is None:
        logger.error('"STACK_NAME" is missing in environment variables.')
        raise ValueError('"STACK_NAME" is missing in environment variables.')
    if STACK_NAME == '':
        logger.info('No "STACK_NAME" value has been provided in environment variables. '
                    'Deploying agent in "standalone" mode.')

        # Retrieve Blazegraph and PostgreSQL/PostGIS settings from docker compose env vars
        # Otherwise they will be retrieved from Stack Clients
        vars_names = { 
            'db_url': 'DB_URL',
            'db_user': 'DB_USER',
            'db_password': 'DB_PASSWORD',
            'query_endpoint': 'QUERY_ENDPOINT',
            'update_endpoint': 'UPDATE_ENDPOINT'
        }
        # Retrieve as global variables
        for v in vars_names:
            globals()[v] = os.getenv(vars_names[v])
            if globals()[v] is None:
                logger.error(f'"{vars_names[v]}" is missing in environment variables.')
                raise ValueError(f'"{vars_names[v]}" is missing in environment variables.')
            if globals()[v] == '':
                logger.error(f'No "{vars_names[v]}" value has been provided in environment variables.')
                raise ValueError(f'No "{vars_names[v]}" value has been provided in environment variables.')


# Run when module is imported
retrieve_settings()
