################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 08 Sep 2022                            #
################################################

# The purpose of this module is to retrieve relevant properties and settings 
# (i.e. for the Time Series Client) from environment variables

import os
import warnings

#import agentlogging

# Initialise logger
#logger = agentlogging.get_logger("prod")



# Initialise global variables to be read from properties file
global DATAPOINT_API_KEY, DATABASE, ONTOP_FILE


def retrieve_settings():
    """
        Reads settings from environment variables (as global variables).
    """

    # Define global scope for global variables
    global DATAPOINT_API_KEY, DATABASE, ONTOP_FILE

    # Retrieve MetOffice API key
    DATAPOINT_API_KEY = os.getenv('API_KEY')    
    if DATAPOINT_API_KEY is None:
        #logger.error('"API_KEY" is missing in environment variables.')
        raise ValueError('"API_KEY" is missing in environment variables.')
    if DATAPOINT_API_KEY == '':
        #logger.error('No "API_KEY" value has been provided in environment variables.')
        raise ValueError('No "API_KEY" value has been provided in environment variables.')

    # Retrieve PostgreSQL/PostGIS database name
    DATABASE = os.getenv('DATABASE')
    if DATABASE is None:
        #logger.error('"DATABASE" name is missing in environment variables.')
        raise ValueError('"DATABASE" name is missing in environment variables.')
    if DATABASE == '':
        #logger.error('No "DATABASE" value has been provided in environment variables.')
        raise ValueError('No "DATABASE" value has been provided in environment variables.')
    if DATABASE != 'postgres':
        #logger.warning(f'Provided "DATABASE" name {db_name} does not match default database name "postgres".')
        warnings.warn(f'Provided "DATABASE" name {DATABASE} does not match default database name "postgres".')

    # Retrieve ONTOP mapping file
    ONTOP_FILE = os.getenv('ONTOP_FILE')
    if ONTOP_FILE is None:
        #logger.error('"ONTOP_FILE" is missing in environment variables.')
        raise ValueError('"ONTOP_FILE" is missing in environment variables.')
    elif not os.path.exists(ONTOP_FILE):
        #logger.error('Invalid "ONTOP_FILE" has been provided in environment variables.')
        raise ValueError('Invalid "ONTOP_FILE" has been provided in environment variables.')


# Run when module is imported
retrieve_settings()
