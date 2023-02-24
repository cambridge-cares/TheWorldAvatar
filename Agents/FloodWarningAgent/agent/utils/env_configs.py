################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 10 Feb 2023                            #
################################################

# The purpose of this module is to retrieve relevant properties and settings 
# (e.g. for the Time Series Client) from environment variables

import os
import warnings

from py4jps import agentlogging

# Initialise logger
logger = agentlogging.get_logger("prod")


def retrieve_env_vars():
    """
        Reads settings from environment variables as global variables,
        i.e. only global within this sub-module
    """

    # Define global scope for global variables
    global NAMESPACE, DATABASE, LAYERNAME, GEOSERVER_WORKSPACE, \
           ONTOP_FILE

    # Create error message
    def _error_msg(variable):
        msg = f'No "{variable}" has been provided in environment variables. '
        msg += 'Either variable key is missing or no value has been provided.'
        return msg

    # Retrieve target Blazegraph name for data to instantiate
    NAMESPACE = os.getenv('NAMESPACE')
    if not NAMESPACE:
        # In case variable is missing (None) or empty ('')
        logger.error(_error_msg('NAMESPACE'))
        raise ValueError(_error_msg('NAMESPACE'))

    # Retrieve target PostGIS database name
    DATABASE = os.getenv('DATABASE')
    if not DATABASE:
        logger.error(_error_msg('DATABASE'))
        raise ValueError(_error_msg('DATABASE'))
    if DATABASE != 'postgres':
        logger.warning(f'Provided "DATABASE" name {DATABASE} does not match default database name "postgres".')
        warnings.warn(f'Provided "DATABASE" name {DATABASE} does not match default database name "postgres".')

    # Retrieve target PostGIS table name for geospatial information
    # PostGIS table and Geoserver layer will have same name
    LAYERNAME = os.getenv('LAYERNAME')
    if not LAYERNAME:
        logger.error(_error_msg('LAYERNAME'))
        raise ValueError(_error_msg('LAYERNAME'))

    # Retrieve Geoserver workspace name
    GEOSERVER_WORKSPACE = os.getenv('GEOSERVER_WORKSPACE')
    if not GEOSERVER_WORKSPACE:
        logger.error(_error_msg('GEOSERVER_WORKSPACE'))
        raise ValueError(_error_msg('GEOSERVER_WORKSPACE'))

    # Retrieve ONTOP mapping file
    ONTOP_FILE = os.getenv('ONTOP_FILE')
    if not ONTOP_FILE:
        logger.error(_error_msg('ONTOP_FILE'))
        raise ValueError(_error_msg('ONTOP_FILE'))
    elif not os.path.exists(ONTOP_FILE):
        logger.error('Invalid "ONTOP_FILE" has been provided in environment variables.')
        raise ValueError('Invalid "ONTOP_FILE" has been provided in environment variables.')


# Run when module is imported
retrieve_env_vars()
