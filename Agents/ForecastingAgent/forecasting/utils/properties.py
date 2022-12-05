# The purpose of this module is to read the properties file with relevant
# settings (i.e. for the Time Series Client)

import os

from configobj import ConfigObj
from pathlib import Path

from py4jps import agentlogging
logger = agentlogging.get_logger('prod')


# Define location of properties file
PROPERTIES_FILE = os.path.abspath(os.path.join(Path(__file__).parent.parent.parent, 
                  "resources", "timeseries.properties"))

# Initialise global variables to be read from properties file
global DB_URL, DB_USER, DB_PASSWORD
global QUERY_ENDPOINT, UPDATE_ENDPOINT


def read_properties_file(filepath):
    """
        Reads settings from properties file (as global variables).
        Arguments:
            filepath - absolute file path to properties file.
    """

    # Define global scope for global variables
    global DB_URL, DB_USER, DB_PASSWORD, QUERY_ENDPOINT, UPDATE_ENDPOINT

    # Read properties file
    props = ConfigObj(filepath)

     # Extract PostgreSQL database URL
    try:
        DB_URL = props['db.url']
    except KeyError:
        logger.error('Key "db.url" is missing in properties file: ' + filepath)
        raise KeyError('Key "db.url" is missing in properties file: ' + filepath)
    if DB_URL == '':
        logger.error('No "db.url" value has been provided in properties file: ' + filepath)
        raise KeyError('No "db.url" value has been provided in properties file: ' + filepath)

    # Extract PostgreSQL database username
    try:
        DB_USER = props['db.user']
    except KeyError:
        logger.error('Key "db.user" is missing in properties file: ' + filepath)
        raise KeyError('Key "db.user" is missing in properties file: ' + filepath)
    if DB_USER == '':
        logger.error('No "db.user" value has been provided in properties file: ' + filepath)
        raise KeyError('No "db.user" value has been provided in properties file: ' + filepath)

    # Extract PostgreSQL database password
    try:
        DB_PASSWORD = props['db.password']
    except KeyError:
        logger.error('Key "db.password" is missing in properties file: ' + filepath)
        raise KeyError('Key "db.password" is missing in properties file: ' + filepath)
    if DB_PASSWORD == '':
        logger.error('No "db.password" value has been provided in properties file: ' + filepath)
        raise KeyError('No "db.password" value has been provided in properties file: ' + filepath)

    # Extract SPARQL Query endpoint of KG
    try:
        QUERY_ENDPOINT = props['sparql.query.endpoint']
    except KeyError:
        logger.error('Key "sparql.query.endpoint" is missing in properties file: ' + filepath)
        raise KeyError('Key "sparql.query.endpoint" is missing in properties file: ' + filepath)
    if QUERY_ENDPOINT == '':
        logger.error('No "sparql.query.endpoint" value has been provided in properties file: ' + filepath)
        raise KeyError('No "sparql.query.endpoint" value has been provided in properties file: ' + filepath)

    # Extract SPARQL Update endpoint of KG
    try:
        UPDATE_ENDPOINT = props['sparql.update.endpoint']
    except KeyError:
        logger.error('Key "sparql.update.endpoint" is missing in properties file: ' + filepath)
        raise KeyError('Key "sparql.update.endpoint" is missing in properties file: ' + filepath)
    if UPDATE_ENDPOINT == '':
        logger.error('No "sparql.update.endpoint" value has been provided in properties file: ' + filepath)
        raise KeyError('No "sparql.update.endpoint" value has been provided in properties file: ' + filepath)


# Run when module is imported
read_properties_file(PROPERTIES_FILE)