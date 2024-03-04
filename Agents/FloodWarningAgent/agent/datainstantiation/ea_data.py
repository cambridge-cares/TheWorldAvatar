################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 12 Feb 2023                            #
################################################

# The purpose of this module is to provide functions to interact with the 
# Environment Agency Real Time flood-monitoring API:
# https://environment.data.gov.uk/flood-monitoring/doc/reference#flood-warnings

import json
import re
import requests
import time
import unicodedata

from datetime import datetime as dt

from agent.datamodel.data_mapping import *
from agent.errorhandling.exceptions import APIException

# Initialise logger
from py4jps import agentlogging
logger = agentlogging.get_logger("prod")


# Define API URL
WARNINGS = 'https://environment.data.gov.uk/flood-monitoring/id/floods'
# API provides URIs for most of its resources (i.e. flood warning, areas, ...)
# Many of these URIs are stable long term reference identifiers; however, some items may only be transient:
#   flood alert/warning: URI for an individual flood is only transient, i.e. limited lifetime
#   flood area: stable long term reference identifiers (resolvable URI)
#   flood area polygon: stable long term reference identifiers (resolvable URI)


def retrieve_current_warnings(county: str = None, mock_api: str = None) -> list:
    """
    Retrieve current flood warnings and alerts from the Environment Agency API

    Arguments:
        county (str): County name for which to retrieve flood warnings (e.g. 'Hampshire')
                      Retrieves ALL current warnings if no county given
        mock_api (str): Path to local .json file to mock API response

    Returns:
        to_instantiate (list): List of dicts with relevant flood warnings/alerts data
    """

    if mock_api:
        # Load mocked API response from local .json file
        with open(mock_api, 'r') as f:
            data = json.load(f)
    else:
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
            # Transient flood warning identifier (i.e. might not resolve in the future on API)
            d['warning_uri'] = w['@id']
            # Normalise string elements
            d['label'] = None if not w.get('description') else normalise_string(w['description'])
            d['message'] = None if not w.get('message') else normalise_string(w['message'])
            # The severity of the warning as a text label: 'Flood Alert', 'Flood Warning', 'Severe Flood Warning' or 'Warning no Longer in Force'
            d['severity'] = None if not w.get('severity') else w['severity']
            # URI for the flood alert or flood warning area affected
            d['area_uri'] = None if not w.get('floodArea') else w['floodArea'].get('@id')
            # Dates and times the warning was last reviewed
            d['timeRaised'] = None if not w.get('timeRaised') else w['timeRaised']
            d['timeMsgChanged'] = None if not w.get('timeMessageChanged') else w['timeMessageChanged']
            d['timeSevChanged'] = None if not w.get('timeSeverityChanged') else w['timeSeverityChanged']
            
            # Strip potential whitespace
            d = {k:v.strip() for k, v in d.items() if v}

            # Add time of last change
            last_altered = [d['timeRaised'], d['timeMsgChanged'], d['timeSevChanged']]
            last_altered = [dt.strptime(last, TIME_FORMAT) for last in last_altered if last]
            d['last_altered'] = max(last_altered)
            
            # Add to list of warnings to instantiate
            to_instantiate.append(d)
    
    return to_instantiate


def retrieve_flood_area_data(area_uri: str) -> dict:
    """
    Retrieve flood area data from the Environment Agency API

    Arguments:
        area_uri (str): Stable (and resolvable) long term flood area reference, e.g.
                        'http://environment.data.gov.uk/flood-monitoring/id/floodAreas/065FAG013'

    Returns:
        area (dict): Dictionary with relevant flood area data
    """

    # Retrieve dictionary of flood area data
    # (API: The flood areas API provide information on the geographic regions to which a given 
    # flood alert or warning may apply. These comprise Flood Alert Areas and Flood Warning Areas)
    data = retrieve_json_from_api(area_uri)
    data = data.get('items')

    area = {}
    if data:
        # Extract relevant information
        # Stable long term flood warning identifier (i.e. shall resolve in the future on API)
        area['area_uri'] = area_uri
        # Name of the county intersecting the flood area, as entered by the Flood Incident Management Team
        area['county'] = None if not data.get('county') else data['county']
        descr1 = None if not data.get('label') else normalise_string(data['label'])
        descr2 = None if not data.get('description') else normalise_string(data['description'])
        area['label']  = descr1 + ': ' + descr2 if descr1 and descr2 else descr1 if descr1 else descr2
        # Identifying code for the corresponding Target Area in Flood Warnings direct
        # (used to link between various external datasets)
        area['area_identifier'] = None if not data.get('fwdCode') else data['fwdCode']
        # URI for the polygon of the flood area, i.e. boundary of the area encoded as a geoJSON polygon
        area['polygon_uri'] = None if not data.get('polygon') else data['polygon']
        # Name of the river or sea area linked to the flood area (optional)
        area['water_body_label'] = None if not data.get('riverOrSea') else data['riverOrSea']
        area['water_body_type'] = assess_waterbody_type(area['water_body_label'])

        # Strip potential whitespace
        area = {k:v.strip() for k, v in area.items() if v}
        
        # Extract type information of flood area, i.e. each area is 
        #   1) a rt:FloodArea and either
        #   2) a rt:FloodAlertArea or a rt:FloodWarningArea
        area['area_types'] = None if not data.get('type') else data['type']
        area['area_types'] = [t.strip() for t in area['area_types'] if t]

    return area


def retrieve_flood_area_polygon(polygon_uri: str) -> dict:
    """
    Retrieve flood area polygon GeoJSON from the Environment Agency API

    Arguments:
        polygon_uri (str): Stable (and resolvable) long term flood polygon reference, e.g.
                          'http://environment.data.gov.uk/flood-monitoring/id/floodAreas/065FAG013/polygon'

    Returns:
        GeoJSON with relevant flood polygon data (in WGS84 coordinates)
    """

    # Retrieve boundary of the flood area encoded as a geoJSON polygon
    # API: Specification of the polygon for each area (as a geoJSON feature in WGS84 coordinates)
    poly = retrieve_json_from_api(polygon_uri)
    
    # Remove irrelevant crs information (and suppress error in case not provided)
    try:
        poly.pop('crs')
    except KeyError:
        pass
    
    #NOTE: This assumes that the polygon is a single feature
    props = poly.get('features')[0].get('properties')
    if props:
        # Extract relevant information
        props_new = {}
        # Try to retrieve most detailed name first
        props_new['name'] = None if not props.get('TA_NAME') else normalise_string(props['TA_NAME'])
        if not props_new.get('name'):
            props_new['name'] = None if not props.get('AREA') else normalise_string(props['AREA'])
        props_new['description'] = None if not props.get('DESCRIP') else normalise_string(props['DESCRIP'])
        # Assign updated properties to polygon
        poly['features'][0]['properties'] = props_new

    return poly


def retrieve_json_from_api(url, max_attempts=3) -> dict:
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


def assess_waterbody_type(waterbody: str) -> str:
    """
    Check if specific water body type can be retrieved from 'riverOrSea' description
    If no specific type can be identified, return 'waterbody'

    Parameters:
        waterbody (str): Description of attached water body as returned by 'riverOrSea'

    Returns:
        waterbody_type (str): Type of the water body, i.e. 
                              river, lake, canal, sea, waterbody
    """

    def _contains_word(word, string):
        # Ensure comparison in same case
        w = word.lower()
        s = string.lower()
        # Check for substring as standalone word
        patterns = [f' {w} ', f'^{w} ', f' {w}$']
        for pattern in patterns:
            if re.search(pattern, s):
                return True
        return False

    if waterbody:
        # Initialise list of all matching water bodies
        matches = []    
        for wb in list(WATERBODIES_API.keys()):
            if _contains_word(wb, waterbody):
                matches.append(WATERBODIES_API[wb])
        # Get unique matches
        matches = set(matches)

        # If description allows specific water body type to be identified, return it ...
        if len(matches) == 1:
            waterbody_type = matches.pop()

            return waterbody_type

    # ... otherwise, return general 'waterbody'
    # (also applies if no water body is provided)
    return 'waterbody'


def normalise_string(input_string):
    # Normalise Unicode characters and remove non-ASCII characters
    normalised_string = unicodedata.normalize('NFKD', input_string).encode('ascii', 'ignore').decode('utf-8')
    # Replace line breaks with a single space
    normalised_string = normalised_string.replace('\n', ' ').replace('\r', ' ')
    # Replace characters which could cause issues in SPARQL
    # 1) replace { with ( and } with )
    normalised_string = normalised_string.replace('{', '(').replace('}', ')')
    # 2) remove potential double quotes
    normalised_string = normalised_string.replace('"', '')

    # Replace double spaces with a single space
    normalised_string = ' '.join(normalised_string.split())

    return normalised_string
