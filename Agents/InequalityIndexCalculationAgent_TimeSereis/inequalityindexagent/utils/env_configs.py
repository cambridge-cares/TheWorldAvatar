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
    global DATABASE, NAMESPACE, MIN_FP, MAX_FP, YEAR
    
    try: 
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

        # Retrieve the MIN_FP 
        MIN_FP = os.getenv('MIN_FP')    
        if MIN_FP is None:
            logger.error('"MIN_FP" is missing in environment variables.')
            raise ValueError('"MIN_FP" is missing in environment variables.')
        if MIN_FP == '':
            logger.error('No "MIN_FP" value has been provided in environment variables.')
            raise ValueError('No "MIN_FP" value has been provided in environment variables.')
        MIN_FP = float(MIN_FP)

        # Retrieve the MAX_FP 
        MAX_FP = os.getenv('MAX_FP')    
        if MAX_FP is None:
            logger.error('"MAX_FP" is missing in environment variables.')
            raise ValueError('"MAX_FP" is missing in environment variables.')
        if MAX_FP == '':
            logger.error('No "MAX_FP" value has been provided in environment variables.')
            raise ValueError('No "MAX_FP" value has been provided in environment variables.')
        MAX_FP = float(MAX_FP)

    # TODO
    except:
        DATABASE = 'heatpump'
        NAMESPACE = 'test'
        MIN_FP= 0
        MAX_FP=0.2
        YEAR = "2020"


# Run when module is imported
retrieve_settings()
