###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 01 Apr 2022                           #
###############################################

# The purpose of this module is to provide functions to retrieve 
# station data from the KG

import os
import re
import json
import pathlib
import pandas as pd

#import agentlogging
from metoffice.kgutils.kgclient import KGClient
from metoffice.kgutils.querytemplates import *
from metoffice.utils.properties import QUERY_ENDPOINT, UPDATE_ENDPOINT
from metoffice.errorhandling.exceptions import InvalidInput
from metoffice.utils.output_formatting import create_geojson_output

# Initialise logger
#logger = agentlogging.get_logger("dev")


def get_all_metoffice_station_ids(query_endpoint: str = QUERY_ENDPOINT,
                                  update_endpoint: str = UPDATE_ENDPOINT):
    """
        Returns list of Met Office IDs of all instantiated stations
    """
    
    # Construct KG client with correct query
    query_string = all_metoffice_station_ids()
    kg_client = KGClient(query_endpoint, update_endpoint)
    # Execute query
    results = kg_client.performQuery(query=query_string)
    # Extract results
    res = [r['id'] for r in results] if results else []
    
    return res


def get_all_metoffice_stations(query_endpoint: str = QUERY_ENDPOINT,
                               update_endpoint: str = UPDATE_ENDPOINT,
                               circle_center: str = None,
                               circle_radius: str = None):
    """
        Returns dictionary with Met Office IDs as key and station IRI as value

        Arguments:
            circle_center - center for Blazegraph's geo:search "inCircle" mode
                            in WGS84 coordinates as 'latitude#longitude'
            circle_radius - radius for geo:search in km
    """
    
    # Validate input
    if circle_center and not circle_radius or \
       circle_radius and not circle_center:
        #logger.error("Circle center or radius is missing for geo:search.")
        raise InvalidInput("Circle center or radius is missing for geo:search.")
    if circle_center:
        if not re.findall(r'[\w\-\.]*#[\w\-\.]*', circle_center):
            #logger.error("Circle center coordinates shall be provided as " \
            #              +"\"latitude#longitude\" in WGS84 coordinates.")
            raise InvalidInput("Circle center coordinates shall be provided as " \
                               +"\"latitude#longitude\" in WGS84 coordinates.")

    # Construct KG client with correct query
    query_string = instantiated_metoffice_stations(circle_center=circle_center,
                                          circle_radius=circle_radius)
    kg_client = KGClient(query_endpoint, update_endpoint)
    # Execute query
    results = kg_client.performQuery(query=query_string)
    # Extract results in required format
    res = [(r['id'], r['station']) for r in results]
    res = dict(res)
    
    return res


def get_all_stations_with_details(query_endpoint: str = QUERY_ENDPOINT,
                                  update_endpoint: str = UPDATE_ENDPOINT,
                                  circle_center: str = None,
                                  circle_radius: str = None):
    """
        Returns DataFrame with all instantiated Met Office station details
        (['stationID', 'station', 'comment', 'latlon', 'elevation', 'dataIRI'])

        Arguments:
            circle_center - center for Blazegraph's geo:search "inCircle" mode
                            in WGS84 coordinates as 'latitude#longitude'
            circle_radius - radius for geo:search in km
    """

    # Validate input
    if circle_center and not circle_radius or \
       circle_radius and not circle_center:
        #logger.error("Circle center or radius is missing for geo:search.")
        raise InvalidInput("Circle center or radius is missing for geo:search.")
    if circle_center:
        if not re.findall(r'[\w\-\.]*#[\w\-\.]*', circle_center):
            #logger.error("Circle center coordinates shall be provided as " \
            #              +"\"latitude#longitude\" in WGS84 coordinates.")
            raise InvalidInput("Circle center coordinates shall be provided as " \
                               +"\"latitude#longitude\" in WGS84 coordinates.")

    # Construct KG client with correct query
    query_string = instantiated_metoffice_stations_with_details(circle_center=circle_center,
                                                                circle_radius=circle_radius)
    kg_client = KGClient(query_endpoint, update_endpoint)
    # Execute query
    results = kg_client.performQuery(query=query_string)
    # Parse results into DataFrame
    df = pd.DataFrame(columns=['stationID', 'station', 'comment', 'latlon', 'elevation', 'dataIRI'])
    df = df.append(results)

    return df


def create_json_output_files(outdir: str, query_endpoint: str = QUERY_ENDPOINT,
                             update_endpoint: str = UPDATE_ENDPOINT,
                             circle_center: str = None,
                             circle_radius: str = None):
    """
        Creates output files required by Digital Twin Visualisation Framework,
        i.e. geojson file with station locations, json file with metadata about
        stations, and json file with time series data

        Arguments:
            outdir - absolute path to output directory for (geo)json files
            circle_center - center for Blazegraph's geo:search "inCircle" mode
                            in WGS84 coordinates as 'latitude#longitude'
            circle_radius - radius for geo:search in km
    """

    # Validate input
    if not pathlib.Path.exists(pathlib.Path(outdir)):
        #logger.error('Provided output directory does not exist.')
        raise InvalidInput('Provided output directory does not exist.')
    else:
        fp_geojson = os.path.join(pathlib.Path(outdir), 'metoffice_stations.geojson')

    # Get details for instantiated stations
    station_details = get_all_stations_with_details(query_endpoint, update_endpoint,
                                                    circle_center, circle_radius)
    
    # Assign ids to stations (required for DTVF)
    stations = list(station_details['station'].unique())
    dtvf_ids =dict(zip(stations, range(len(stations))))
    station_details['dtvf_id'] = station_details['station'].map(dtvf_ids)

    # Create GeoJSON file for ReportingStations
    geojson = create_geojson_output(station_details)

    # Write output files
    with open(fp_geojson, 'w') as f:
        json.dump(geojson, indent=4, fp=f)

    print('')


if __name__ == '__main__':

    #get_all_metoffice_station_ids()

    #get_all_stations_with_details(circle_center='57.5#-3.5', circle_radius='1000')

    create_json_output_files('C:\TheWorldAvatar-git\Agents\MetOfficeAgent\output')
