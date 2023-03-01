################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 28 Apr 2022                            #
################################################

# The purpose of this module is to provide functions to retrieve 
# station data from the KG and create output files for visualisation

import os
import re
import json
import time
import pathlib
import pandas as pd
from agent.kgutils import kgclient

#import agentlogging
from agent.kgutils.kgclient import KGClient
from agent.kgutils.timeseries import TSClient
from agent.kgutils.querytemplates import *
from agent.utils.stack_configs import DB_PASSWORD, DB_URL, DB_USER, QUERY_ENDPOINT, UPDATE_ENDPOINT
from agent.errorhandling.exceptions import InvalidInput
from agent.dataretrieval.readings import get_time_series_data
from agent.utils.output_formatting import create_geojson_output, create_metadata_output

# Initialise logger
#logger = agentlogging.get_logger("prod")


def get_all_airquality_station_ids(query_endpoint: str = QUERY_ENDPOINT,
                                   update_endpoint: str = UPDATE_ENDPOINT):
    """
        Returns list of UK AIR station IDs of all instantiated stations
        Note that UK AIR does not provide unique station IDs natively; hence,
        a combination of station name + location is used as unique ID, i.e.
        "<station name>_<latitude>#<longitude>" where <latitude> & <longitude>
        are rounded to 6 digits
    """
    
    # Construct KG client with correct query
    query_string = all_airquality_station_ids()
    kg_client = KGClient(query_endpoint, update_endpoint)
    # Execute query
    results = kg_client.performQuery(query=query_string)
    # Extract results
    res = [r['id'] for r in results] if results else []
    
    return res


def get_all_airquality_stations(query_endpoint: str = QUERY_ENDPOINT,
                                update_endpoint: str = UPDATE_ENDPOINT,
                                circle_center: str = None,
                                circle_radius: str = None):
    """
        Returns dictionary with station IDs as key and station IRI as value

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
    query_string = instantiated_airquality_stations(circle_center=circle_center,
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
        Returns DataFrame with all instantiated UK AIR station details
        (['stationID', 'station', 'label', 'latlon', 'elevation', 'dataIRI'])
        Some cell entries can potentially contain NaNs (e.g. elevation, dataIRI)

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
    query_string = instantiated_airquality_stations_with_details(circle_center=circle_center,
                                                                 circle_radius=circle_radius)
    kg_client = KGClient(query_endpoint, update_endpoint)
    # Execute query
    results = kg_client.performQuery(query=query_string)
    # Parse results into DataFrame
    df = pd.DataFrame(columns=['stationID', 'station', 'label', 'latlon', 
                               'elevation', 'dataIRI'])
    df = pd.concat([df, pd.DataFrame.from_dict(results)], axis=0)
    df = df.drop_duplicates()

    return df


def create_json_output_files(outdir: str, observation_types: list = None, 
                             circle_center: str = None, circle_radius: str = None, 
                             tmin: str = None, tmax: str = None, 
                             query_endpoint: str = QUERY_ENDPOINT,
                             update_endpoint: str = UPDATE_ENDPOINT):
    """
        Creates output files required by Digital Twin Visualisation Framework,
        i.e. geojson file with station locations, json file with metadata about
        stations, and json file with time series data

        Arguments:
            outdir - absolute path to output directory for (geo)json files
            observation_types - list of observation types (e.g., PM10Concentration)
                                for which to retrieve data (all if None)
            circle_center - center for Blazegraph's geo:search "inCircle" mode
                            in WGS84 coordinates as 'latitude#longitude'
            circle_radius - radius for geo:search in km            
            tmin - oldest time step for which to retrieve data
            tmax - latest time step for which to retrieve data
    """

    # Validate input
    if not pathlib.Path.exists(pathlib.Path(outdir)):
        #logger.error('Provided output directory does not exist.')
        raise InvalidInput('Provided output directory does not exist.')
    else:
        # Initialise lists of output files
        fp_geojson = [os.path.join(pathlib.Path(outdir), 'airquality_stations.geojson'),
                      os.path.join(pathlib.Path(outdir), 'airquality_stations_woTS.geojson')]
        fp_metadata = [os.path.join(pathlib.Path(outdir), 'airquality_stations-meta.json'),
                       os.path.join(pathlib.Path(outdir), 'airquality_stations_woTS-meta.json')]
        fp_timeseries = os.path.join(pathlib.Path(outdir), 'airquality_stations-timeseries.json')
        color = '#C0392B'
        opacity = 0.66
    
    # Initialise output/collection lists
    geojson, metadata = [], []

    #
    ###---  Retrieve KG data  ---###
    #
    # 1) Get details for instantiated stations
    print('Retrieving instantiated stations from KG ...')
    #logger.info('Retrieving instantiated stations from KG ...')
    t1 = time.time()
    station_details = get_all_stations_with_details(query_endpoint, update_endpoint,
                                                    circle_center, circle_radius)
    t2 = time.time()
    diff = t2-t1
    print(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    #logger.info('Stations successfully retrieved.')
    
    # Extract station IRIs of interest
    station_iris = list(station_details['station'].unique())
    # Assign ids to stations (required for DTVF)
    dtvf_ids =dict(zip(station_iris, range(len(station_iris))))
    station_details['dtvf_id'] = station_details['station'].map(dtvf_ids)
   
    # 2) Get time series data
    print('Retrieving time series data from KG ...')
    #logger.info('Retrieving time series data from KG ...')
    t1 = time.time()    
    ts_data, ts_names, ts_units = get_time_series_data(station_iris, observation_types,
                                                       tmin, tmax, query_endpoint,
                                                       update_endpoint)
    t2 = time.time()
    diff = t2-t1
    print(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    #logger.info('Time series successfully retrieved.')

    #
    ###---  Create output files  ---###
    #
    # Initialise time series client   
    # ts_client = TSClient.tsclient_with_default_settings()
    ts_client = TSClient(kg_client=kgclient, rdb_url=DB_URL, rdb_user=DB_USER, 
                         rdb_password=DB_PASSWORD)
    # Create output files for each set of retrieved time series data

    print('Creating output files (geojson, metadata, timeseries) ...')
    #logger.info('Creating output files (geojson, metadata, timeseries) ...')
    t1 = time.time()

    # 1) Create GeoJSON file for ReportingStations
    stations = station_details[~station_details['dataIRI'].isna()]
    geojson.append(create_geojson_output(stations, color, opacity))

    # 2) Create JSON file for ReportingStations metadata
    metadata.append(create_metadata_output(stations))

    # 3) Create Time series output    
    # Get List of corresponding dtvf ids for list of time series
    # (to assign time series output to correct station in DTVF)
    dataIRIs = [t.getDataIRIs()[0] for t in ts_data]
    id_list = [int(stations.loc[stations['dataIRI'] == i, 'dtvf_id'].values) for i in dataIRIs]
    tsjson = ts_client.convertToJSON(ts_data, id_list, ts_units, ts_names)
    # Make JSON file readable in Python
    timeseries = json.loads(tsjson.toString())
    t2 = time.time()
    diff = t2-t1
    print(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    #logger.info('Output files successfully created.')

    # Create output files for stations without any time series data
    stations = station_details[station_details['dataIRI'].isna()]
    geojson.append(create_geojson_output(stations, color, opacity))
    metadata.append(create_metadata_output(stations))

    #
    ###---  Write output files  ---###
    #
    print('Writing output files ...')
    #logger.info('Writing output files ...')
    for i in range(len(fp_geojson)):
        with open(fp_geojson[i], 'w') as f:
            json.dump(geojson[i], indent=4, fp=f)
        with open(fp_metadata[i], 'w') as f:
            json.dump(metadata[i], indent=4, fp=f)
    # No time series data for stations w/o time series
    with open(fp_timeseries, 'w') as f:
        json.dump(timeseries, indent=4, fp=f)
    print('Finished!\n')


if __name__ == '__main__':

    # Create station and time series output files
    create_json_output_files('C:\TheWorldAvatar-git\Agents\AirQualityAgent\output')

    # create_json_output_files('C:\TheWorldAvatar-git\Agents\AirQualityAgent\output',
    #                          observation_types=['PM10Concentration'])

    # Data retrieval with geospatial search require a geospatially-enabled
    # Blazegraph namespace to work successfully
    # create_json_output_files('C:\TheWorldAvatar-git\Agents\AirQualityAgent\output',
    #                          circle_center='52.75#0.4', circle_radius='100')

    # create_json_output_files('C:\TheWorldAvatar-git\Agents\AirQualityAgent\output',
    #                          circle_center='52.75#0.4', circle_radius='100',
    #                          observation_types=['PM10Concentration'])
