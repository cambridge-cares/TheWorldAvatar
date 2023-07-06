################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 12 Feb 2023                            #
################################################

# The purpose of this module is to provide functions to interact with the 
# Office for National Statistics (ONS) Linked Data API:
# https://statistics.data.gov.uk/home
# https://statistics.data.gov.uk/sparql

import json
import requests
import urllib.parse

from agent.datamodel.data_mapping import *
from agent.errorhandling.exceptions import APIException
from agent.kgutils.querytemplates import ons_county

# Initialise logger
from py4jps import agentlogging
logger = agentlogging.get_logger("prod")


# Define API URL
ONS = 'http://statistics.data.gov.uk/sparql'


def retrieve_ons_county(county_name) -> str:
    """
        Retrieves county IRI for given county name from ONS API

        Arguments:
            county_name (str): County name as used in Flood Warnings and Alerts
                               (as well as FloodArea information)

        Returns:
            county_iri (str): ONS Linked Data IRI for given county
                              None if no (or no unique) matching county found
    """

    # Get query and URL-encode (as required by API)
    q = ons_county(county_name)
    query = urllib.parse.quote(q)

    # Perform GET request
    url = ONS + '.json?query=' + query
    res = requests.get(url)
    if res.status_code != 200:
        logger.error('Error retrieving data from ONS API.')
        raise APIException('Error retrieving data from ONS API.')

    # Extract and unwrap results
    data = json.loads(res.text)
    data = data.get('results', {}).get('bindings', [])
    if len(data) != 1:
        logger.warning(f'None or multiple matching ONS counties found for "{county_name}".')
        return None
    else:
        county_iri = data[0].get('district_iri', {}).get('value', None)
        return county_iri
