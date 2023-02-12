################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 12 Feb 2023                            #
################################################

# The purpose of this module is to provide functions to interact with the 
# Environment Agency Real Time flood-monitoring API:
# https://environment.data.gov.uk/flood-monitoring/doc/reference#flood-warnings

import requests
import time

from py4jps import agentlogging
from agent.errorhandling.exceptions import APIException

# Initialise logger
logger = agentlogging.get_logger("dev")
#logger = agentlogging.get_logger("prod")


# Define API URL
WARNINGS = 'https://environment.data.gov.uk/flood-monitoring/id/floods'


def retrieve_current_warnings(county: str = None):
    """
    Retrieve current flood warnings and alerts from the Environment Agency API

    Arguments:
        county (str): County name for which to retrieve flood warnings (e.g. 'Hampshire')
                      Retrieves ALL current warnings if county is None

    Returns:
        warnings (list): List of dicts with relevant flood warnings/alerts data
    """

    # Construct URL
    if county:
        url = WARNINGS + '?county={}'.format(county)
    else:
        url = WARNINGS

    # Retrieve dictionary of all current flood warnings
    data = retrieve_json_from_api(url)

    # Extract relevant information
    # API: The 'items' element contains an array even if the list only has one entry
    warnings = data.get('items', [])
    to_instantiate = []
    for w in warnings:
        # Initialise dict with relevant information for each warning
        if w.get('@id'):
            d = {}
            # API: Each flood warning and area has an identifier; however lifetime varies
            #   flood areas: stable long term reference identifiers
            #   flood alerts: URI for an individual flood is only transient
            d['iri'] = w['@id']
            # Replace non-utf-8 narrow space character from some messages
            d['label'] = None if not w.get('description') else w['description'].replace('\n', ' ').replace('\u202F', ' ')
            d['message'] = None if not w.get('message') else w['message'].replace('\n', ' ').replace('\u202F', ' ')
            d['timeRaised'] = None if not w.get('timeRaised') else w['timeRaised']
            d['timeMsgChanged'] = None if not w.get('timeMessageChanged') else w['timeMessageChanged']
            d['timeSevChanged'] = None if not w.get('timeSeverityChanged') else w['timeSeverityChanged']
            d['severity'] = None if not w.get('severity') else w['severity']
            d['floodArea'] = None if not w.get('floodArea') else w['floodArea'].get('@id')
            # Strip potential whitespace
            d = {k:v.strip() for k, v in d.items() if v}
            
            # Add to list of warnings to instantiate
            to_instantiate.append(d)
    
    return to_instantiate


def retrieve_json_from_api(url, max_attempts=3):
    """
    Retrieve data from the Environment Agency API and return as JSON dictionary

    Parameters:
        url (str): URL of the API endpoint to retrieve data from
        max_attempts (int): Maximum number of attempts to retrieve data from API
    
    Returns:
        data (dict): Dictionary containing the retrieved data
    """
    retrieved = False
    remaining_attempts = max_attempts
    logger.info('Retrieving data from EA API...')
    while not retrieved:
        response = requests.get(url)
        if response.status_code == 200:
            data = response.json()
            retrieved = True
            logger.info('Data successfully retrieved.')
        else:
            logger.error('Error retrieving data from API. Status code: {}'.format(response.status_code))
            remaining_attempts -= 1
            logger.info('Retrying in 60 seconds. {} attempts remaining.'.format(remaining_attempts))
            time.sleep(60)
            if remaining_attempts == 0:
                logger.error('Maximum number of retries reached. Aborting.')
                raise APIException('Error retrieving data from EA flood-monitoring API.')
    return data


if __name__ == '__main__':

    # Retrieve current flood warnings
    warnings = retrieve_current_warnings()
