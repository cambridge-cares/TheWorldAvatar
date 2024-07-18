################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 11 Jul 2022                            #
################################################

# The purpose of this module is to retrieve relevant properties and settings 
# from environment variables and the stack clients (if applicable)

import os

from py4jps import agentlogging

from .stack_configs import retrieve_stack_settings

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


def retrieve_configs():
    """
    Reads (connection) settings from environment variables as global variables, 
    i.e. only global within this sub-module
    """

    # Define global scope for global variables
    global NAMESPACE, STACK_NAME, QUERY_ENDPOINT, UPDATE_ENDPOINT, \
           FORECASTING_AGENT_IRI, DH_OPTIMISATION_AGENT_IRI, EMISSION_ESTIMATION_AGENT_IRI, \
           DISPERSION_INTERACTOR_URL
    
    # Initialise variables (to ensure working imports even if not defined in env vars)
    NAMESPACE = None
    STACK_NAME = None
    QUERY_ENDPOINT = None
    UPDATE_ENDPOINT = None
    FORECASTING_AGENT_IRI = None
    DH_OPTIMISATION_AGENT_IRI = None
    EMISSION_ESTIMATION_AGENT_IRI = None
    DISPERSION_INTERACTOR_URL = None

    # Retrieve Docker Stack name
    STACK_NAME = os.getenv('STACK_NAME')
    if STACK_NAME is None:
        logger.error('"STACK_NAME" is missing in environment variables.')
        raise ValueError('"STACK_NAME" is missing in environment variables.')

    # Retrieve Blazegraph settings depending on deployment mode
    if not STACK_NAME:
        # 1) Standalone: retrieve settings from docker compose env vars
        logger.info('No "STACK_NAME" value has been provided in environment variables. '
                    'Deploying agent in "standalone" mode.')
        
        # Retrieve global variables for connection settings
        vars_names = ['QUERY_ENDPOINT', 'UPDATE_ENDPOINT']
        for v in vars_names:
            globals()[v] = os.getenv(v)
            if not globals()[v]:
                # In case variable key is missing or empty value provided
                logger.error(f'"{v}" is missing in environment variables.')
                raise ValueError(f'"{v}" is missing in environment variables.')

    else:
        # 2) Stack deployment: retrieve settings from Stack Clients
        logger.info('Deploying agent to stack "{STACK_NAME}".')

        # Retrieve target Blazegraph namespace for data to instantiate
        NAMESPACE = os.getenv('NAMESPACE')
        if not NAMESPACE:
            # In case no NAMESPACE is provided (i.e. missing key or left blank),
            logger.error(f'"{NAMESPACE}" is missing in environment variables.')
            raise ValueError(f'"{NAMESPACE}" is missing in environment variables.')
        
        # Retrieve settings from Stack Clients
        QUERY_ENDPOINT, UPDATE_ENDPOINT = retrieve_stack_settings(namespace=NAMESPACE)

    # Log retrieved default settings
    logger.info('Retrieved connection parameters:')
    logger.info(f"QUERY_ENDPOINT: {QUERY_ENDPOINT}")
    logger.info(f"UPDATE_ENDPOINT: {UPDATE_ENDPOINT}")

    # Retrieve derivation agent service IRIs
    vars_names = ['FORECASTING_AGENT_IRI', 'DH_OPTIMISATION_AGENT_IRI', 
                  'EMISSION_ESTIMATION_AGENT_IRI', 'DISPERSION_INTERACTOR_URL']
    for v in vars_names:
        globals()[v] = os.getenv(v)
        if not globals()[v]:
            # In case variable key is missing or empty value provided
            logger.error(f'"{v}" service IRI is missing in environment variables.')
            raise ValueError(f'"{v}" service IRI is missing in environment variables.')


# Run when module is imported
retrieve_configs()
