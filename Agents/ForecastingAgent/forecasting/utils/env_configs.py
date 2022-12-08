################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 08 Dec 2022                            #
################################################

# The purpose of this module is to retrieve relevant properties and settings 
# from environment variables and the stack clients (if applicable)

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
           DB_URL, DB_USER, DB_PASSWORD, QUERY_ENDPOINT, UPDATE_ENDPOINT
    
    # Initialise potentially not provided variables
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
        
        # Retrieve global variables
        vars_names = ['DB_URL', 'DB_USER', 'DB_PASSWORD', 'QUERY_ENDPOINT', 'UPDATE_ENDPOINT']
        for v in vars_names:
            globals()[v] = os.getenv(v)
            if globals()[v] is None:
                logger.error(f'"{v}" is missing in environment variables.')
                raise ValueError(f'"{v}" is missing in environment variables.')
            if globals()[v] == '':
                logger.error(f'No "{v}" value has been provided in environment variables.')
                raise ValueError(f'No "{v}" value has been provided in environment variables.')
    else:
        # 2) Stack deployment: retrieve settings from Stack Clients
        logger.info('Deploying agent to stack "{STACK_NAME}".')

        # Import stack_configs module only when needed to avoid import issues/
        # potentially unnecessary installation of py4jps StackClients resource
        from .stack_configs import retrieve_stack_settings

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
        
        # Retrieve settings from Stack Clients
        retrieve_stack_settings(database=DATABASE,namespace=NAMESPACE)


#TODO: Implement properly, ideally as dockerised version
# Set environment variables for standalone testing (non-dockerised deployment)
os.environ["STACK_NAME"] = ""
os.environ["DB_URL"] = "jdbc:postgresql://localhost:9432/districtheating"
os.environ["DB_USER"] = "postgres"
os.environ["DB_PASSWORD"] = "postgres"
os.environ["QUERY_ENDPOINT"] = "http://localhost:9999/blazegraph/namespace/districtheating/sparql"
os.environ["UPDATE_ENDPOINT"] = "http://localhost:9999/blazegraph/namespace/districtheating/sparql"

# Run when module is imported
retrieve_settings()
