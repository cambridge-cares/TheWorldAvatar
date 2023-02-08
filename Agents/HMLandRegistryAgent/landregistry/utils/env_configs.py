################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 12 Oct 2022                            #
################################################

# The purpose of this module is to retrieve relevant properties and settings 
# (i.e. for the Time Series Client) from environment variables

import os
import warnings

import agentlogging

# Initialise logger
logger = agentlogging.get_logger("prod")


def retrieve_settings():
    """
        Reads settings from environment variables (as global variables).
    """

    # Define global scope for global variables
    global DATABASE

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


# Run when module is imported
retrieve_settings()
