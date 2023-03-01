################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 31 Mar 2022                            #
################################################

# The purpose of this module is to read the properties file with relevant
# settings (i.e. for the Time Series Client)

import os
import warnings

from py4jps import agentlogging
from pathlib import Path


# Initialise logger
logger = agentlogging.get_logger("prod")


# Define location of properties file
# PROPERTIES_FILE = os.path.abspath(os.path.join(Path(__file__).parent.parent.parent, 
#                   "resources", "airquality.properties"))

# Initialise global variables to be read from properties file
# global DB_URL, DB_USER, DB_PASSWORD
# global QUERY_ENDPOINT, UPDATE_ENDPOINT


def retrieve_settings():
    """
        Reads settings from environment variables (as global variables).
    """

    # Define global scope for global variables
    global NAMESPACE, DATABASE, LAYERNAME, GEOSERVER_WORKSPACE, ONTOP_FILE

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


# Run when module is imported
retrieve_settings()
