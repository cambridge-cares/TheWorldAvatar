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
from metoffice.kgutils.timeseries import TSClient
from metoffice.kgutils.querytemplates import *
from metoffice.utils.properties import QUERY_ENDPOINT, UPDATE_ENDPOINT
from metoffice.errorhandling.exceptions import InvalidInput
from metoffice.dataretrieval.readings import get_time_series_data
from metoffice.utils.output_formatting import create_geojson_output, create_metadata_output

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
    df = pd.DataFrame(columns=['stationID', 'station', 'comment', 'latlon', 
                               'elevation', 'dataIRI_obs', 'dataIRI_fc'])
    df = df.append(results)
    # Consolidate dataIRI columns
    df['dataIRI'] = df[['dataIRI_obs', 'dataIRI_fc']].values.tolist()
    df = df.drop(columns=['dataIRI_obs', 'dataIRI_fc'])
    df = df.explode('dataIRI').reset_index(drop=True)
    df = df.dropna(subset=['dataIRI'])
    df = df.drop_duplicates()

    return df


def create_json_output_files(outdir: str, query_endpoint: str = QUERY_ENDPOINT,
                             update_endpoint: str = UPDATE_ENDPOINT,
                             circle_center: str = None,
                             circle_radius: str = None,
                             observation_types: list = None,
                             tmin: str = None, tmax: str = None,):
    """
        Creates output files required by Digital Twin Visualisation Framework,
        i.e. geojson file with station locations, json file with metadata about
        stations, and json file with time series data

        Arguments:
            outdir - absolute path to output directory for (geo)json files
            circle_center - center for Blazegraph's geo:search "inCircle" mode
                            in WGS84 coordinates as 'latitude#longitude'
            circle_radius - radius for geo:search in km
            observation_types - list of observation types (e.g., AirTemperature)
                                for which to retrieve data (all if None)
            tmin - oldest time step for which to retrieve data
            tmax - latest time step for which to retrieve data
    """

    # Validate input
    if not pathlib.Path.exists(pathlib.Path(outdir)):
        #logger.error('Provided output directory does not exist.')
        raise InvalidInput('Provided output directory does not exist.')
    else:
        fp_geojson = os.path.join(pathlib.Path(outdir), 'metoffice_stations2.geojson')
        fp_metadata = os.path.join(pathlib.Path(outdir), 'metoffice_stations2-meta.json')
        fp_timeseries = os.path.join(pathlib.Path(outdir), 'metoffice_stations2-timeseries.json')

    # Retrieve KG data
    # 1) Get details for instantiated stations
    station_details = get_all_stations_with_details(query_endpoint, update_endpoint,
                                                    circle_center, circle_radius)
    # 2) Get time series data
    station_iris = list(station_details['station'].unique())
    ts_data, ts_names, ts_units = get_time_series_data(station_iris, observation_types, 
                                                       tmin, tmax, query_endpoint, 
                                                       update_endpoint)

    # Assign ids to stations (required for DTVF)
    dtvf_ids =dict(zip(station_iris, range(len(station_iris))))
    station_details['dtvf_id'] = station_details['station'].map(dtvf_ids)

    # 1) Create GeoJSON file for ReportingStations
    geojson = create_geojson_output(station_details)

    # 2) Create JSON file for ReportingStations metadata
    metadata = create_metadata_output(station_details)

    # 3) Create Time series output    
    ts_client = TSClient.tsclient_with_default_settings()
    # Get List of corresponding dtvf ids for list of time series
    # (to assign time series output to correct station in DTVF)
    dataIRIs = [ts.getDataIRIs()[0] for ts in ts_data]
    id_list = [int(station_details.loc[station_details['dataIRI'] == i, 'dtvf_id'].values) for i in dataIRIs]
    timeseries = ts_client.convertToJSON(ts_data, id_list, ts_units, ts_names)
     # Make JSON file readable in Python
    timeseries = json.loads(timeseries.toString())

    # Write output files
    with open(fp_geojson, 'w') as f:
        json.dump(geojson, indent=4, fp=f)
    with open(fp_metadata, 'w') as f:
        json.dump(metadata, indent=4, fp=f)
    with open(fp_timeseries, 'w') as f:
        json.dump(timeseries, indent=4, fp=f)


if __name__ == '__main__':

    #get_all_metoffice_station_ids()

    #get_all_stations_with_details(circle_center='57.5#-3.5', circle_radius='1000')

    # create_json_output_files('C:\TheWorldAvatar-git\Agents\MetOfficeAgent\output',
    #                          circle_center='52.75#0.4', circle_radius='100')

    create_json_output_files('C:\TheWorldAvatar-git\Agents\MetOfficeAgent\output',
                             circle_center='52.75#0.4', circle_radius='100',
                             observation_types=['AirTemperature'])
