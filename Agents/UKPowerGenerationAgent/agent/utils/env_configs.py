##############################################################################
# Authors: Feroz Farazi (msff2@cam.ac.uk), John Atherton (ja685@cam.ac.uk)   #
# Date: 30 June 2023                                                         #
##############################################################################

# The purpose of this module is to retrieve relevant properties and settings 
# (e.g. for the Time Series Client) from environment variables

import os
import warnings

from py4jps import agentlogging

# Initialise logger
logger = agentlogging.get_logger("prod")


def retrieve_settings():
    """
        Reads settings from environment variables (as global variables).
    """

    # Define global scope for global variables
    global MAPBOX_API_KEY, NAMESPACE, DATABASE, LAYERNAME, \
        GEOSERVER_WORKSPACE, ONTOP_FILE, OUTPUT_DIR

    # MapBox API key
    MAPBOX_API_KEY = os.getenv('MAPBOX_API_KEY')    
    if MAPBOX_API_KEY is None:
        logger.error('"MAPBOX_API_KEY" is missing in environment variables.')
        raise ValueError('"MAPBOX_API_KEY" is missing in environment variables.')
    if MAPBOX_API_KEY == '':
        logger.error('No "MAPBOX_API_KEY" value has been provided in environment variables.')
        raise ValueError('No "MAPBOX_API_KEY" value has been provided in environment variables.')

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
        warnings.warn(f'Provided "DATABASE" name {DATABASE} does not match default database name "postgres".')

    # Retrieve target PostgreSQL/PostGIS table name for geospatial information
    # PostGIS table and Geoserver layer will have same name
    LAYERNAME = os.getenv('LAYERNAME')
    if LAYERNAME is None:
        logger.error('"LAYERNAME" is missing in environment variables.')
        raise ValueError('"LAYERNAME" is missing in environment variables.')
    if LAYERNAME == '':
        logger.error('No "LAYERNAME" value has been provided in environment variables.')
        raise ValueError('No "LAYERNAME" value has been provided in environment variables.')

    # Retrieve Geoserver workspace name
    GEOSERVER_WORKSPACE = os.getenv('GEOSERVER_WORKSPACE')
    if GEOSERVER_WORKSPACE is None:
        logger.error('"GEOSERVER_WORKSPACE" name is missing in environment variables.')
        raise ValueError('"GEOSERVER_WORKSPACE" name is missing in environment variables.')
    if GEOSERVER_WORKSPACE == '':
        logger.error('No "GEOSERVER_WORKSPACE" value has been provided in environment variables.')
        raise ValueError('No "GEOSERVER_WORKSPACE" value has been provided in environment variables.')

    # Retrieve ONTOP mapping file
    ONTOP_FILE = os.getenv('ONTOP_FILE')
    if ONTOP_FILE is None:
        logger.error('"ONTOP_FILE" is missing in environment variables.')
        raise ValueError('"ONTOP_FILE" is missing in environment variables.')
    elif not os.path.exists(ONTOP_FILE):
        logger.error('Invalid "ONTOP_FILE" has been provided in environment variables.')
        raise ValueError('Invalid "ONTOP_FILE" has been provided in environment variables.')

    # Retrieve OUTPUT directory name
    OUTPUT_DIR = os.getenv('OUTPUT_DIR')
    if OUTPUT_DIR is None:
        logger.error('"OUTPUT_DIR" is missing in environment variables.')
        raise ValueError('"OUTPUT_DIR" is missing in environment variables.')
    elif not os.path.exists(OUTPUT_DIR):
        logger.error('Invalid "OUTPUT_DIR" has been provided in environment variables.')
        raise ValueError('Invalid "OUTPUT_DIR" has been provided in environment variables.')


# Run when module is imported
retrieve_settings()
