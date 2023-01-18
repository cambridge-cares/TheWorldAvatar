################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 30 Oct 2022                            #
################################################
################################################
# Editor: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to retrieve relevant properties and settings 
# (e.g. for the Time Series Client) from environment variables

import os
import warnings
from cryptography.fernet import Fernet
from dotenv import load_dotenv

import agentlogging

# Initialise logger
logger = agentlogging.get_logger("prod")


def retrieve_settings():
    """
        Reads settings from environment variables (as global variables).
    """

    # Define global scope for global variables
    global YEAR, NAMESPACE, DATABASE, LAYERNAME, GEOSERVER_WORKSPACE, \
           ONTOP_FILE, CEDA_USERNAME, CEDA_PASSWORD

    # Retrieve the YEAR of which the user is interested in processing data about
    # the UK Lower-layer Super Output Area (LSOA)
    YEAR = os.getenv('YEAR')    
    if YEAR is None:
        logger.error('"YEAR" is missing in environment variables.')
        raise ValueError('"YEAR" is missing in environment variables.')
    if YEAR == '':
        logger.error('No "YEAR" value has been provided in environment variables.')
        raise ValueError('No "YEAR" value has been provided in environment variables.')

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

    load_dotenv('./downloads/.env')
    key = os.getenv('CEDA_KEY').encode()
    cipher = Fernet(key)
    # Retrieve the CEDA_USERNAME
    CEDA_USERNAME = os.getenv('CEDA_USERNAME')    
    if CEDA_USERNAME is None:
        logger.error('"CEDA_USERNAME" is missing in environment variables.')
        raise ValueError('"CEDA_USERNAME" is missing in environment variables.')
    if CEDA_USERNAME == '':
        logger.error('No "CEDA_USERNAME" value has been provided in environment variables.')
        raise ValueError('No "CEDA_USERNAME" value has been provided in environment variables.')

    # Retrieve the CEDA_PASSWORD
    encrypted_password = os.getenv('CEDA_PASSWORD')
    CEDA_PASSWORD = str(cipher.decrypt(encrypted_password), 'utf-8')
    if CEDA_PASSWORD is None:
        logger.error('"CEDA_PASSWORD" is missing in environment variables.')
        raise ValueError('"CEDA_PASSWORD" is missing in environment variables.')
    if CEDA_PASSWORD == '':
        logger.error('No "CEDA_PASSWORD" value has been provided in environment variables.')
        raise ValueError('No "CEDA_PASSWORD" value has been provided in environment variables.')

# Run when module is imported
retrieve_settings()
