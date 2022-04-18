###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 01 Apr 2022                           #
###############################################

# The purpose of this module is to provide functions to retrieve 
# station data from the KG

import os
import re
import json
import time
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
        (['stationID', 'station', 'comment', 'latlon', 'elevation', 
          'obs_station', 'fcs_station', 'dataIRI'])

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
    # Add station classification (one hot encoded)
    df['obs_station'] = df['dataIRI_obs'].isna().apply(lambda x: 0 if x else 1)
    df['fcs_station'] = df['dataIRI_fc'].isna().apply(lambda x: 0 if x else 1)
    # Consolidate dataIRI columns
    df['dataIRI'] = df[['dataIRI_obs', 'dataIRI_fc']].values.tolist()
    df = df.drop(columns=['dataIRI_obs', 'dataIRI_fc'])
    df = df.explode('dataIRI').reset_index(drop=True)
    df = df.dropna(subset=['dataIRI'])
    df = df.drop_duplicates()

    return df


def create_json_output_files(outdir: str, observation_types: list = None, 
                             split_obs_fcs: bool = True, circle_center: str = None,
                             circle_radius: str = None, tmin: str = None, 
                             tmax: str = None, query_endpoint: str = QUERY_ENDPOINT,
                             update_endpoint: str = UPDATE_ENDPOINT):
    """
        Creates output files required by Digital Twin Visualisation Framework,
        i.e. geojson file with station locations, json file with metadata about
        stations, and json file with time series data

        Arguments:
            outdir - absolute path to output directory for (geo)json files
            observation_types - list of observation types (e.g., AirTemperature)
                                for which to retrieve data (all if None)
            split_obs_fcs - boolean flag whether to create joint output files for
                            observations and forecasts or 2 separate sets 
            circle_center - center for Blazegraph's geo:search "inCircle" mode
                            in WGS84 coordinates as 'latitude#longitude'
            circle_radius - radius for geo:search in km            
            tmin - oldest time step for which to retrieve data
            tmax - latest time step for which to retrieve data
            split_obs_fcs - boolean flag
    """

    # Validate input
    if not pathlib.Path.exists(pathlib.Path(outdir)):
        #logger.error('Provided output directory does not exist.')
        raise InvalidInput('Provided output directory does not exist.')
    else:
        if not split_obs_fcs:
            fp_geojson = [os.path.join(pathlib.Path(outdir), 'metoffice_stations.geojson')]
            fp_metadata = [os.path.join(pathlib.Path(outdir), 'metoffice_stations-meta.json')]
            fp_timeseries = [os.path.join(pathlib.Path(outdir), 'metoffice_stations-timeseries.json')]
            colors = ['#C0392B']
            opacities = [0.66]
        else:
            fp_geojson = [os.path.join(pathlib.Path(outdir), 'metoffice_observation_stations.geojson'),
                          os.path.join(pathlib.Path(outdir), 'metoffice_forecast_stations.geojson')]
            fp_metadata = [os.path.join(pathlib.Path(outdir), 'metoffice_observation_stations-meta.json'),
                           os.path.join(pathlib.Path(outdir), 'metoffice_forecast_stations-meta.json')]
            fp_timeseries = [os.path.join(pathlib.Path(outdir), 'metoffice_observation_stations-timeseries.json'),
                             os.path.join(pathlib.Path(outdir), 'metoffice_forecast_stations-timeseries.json')]
            colors = ['#C0392B', '#2471A3']
            opacities = [0.5, 0.5]
    
    # Initialise output/collection lists
    stat_details = []
    ts_data, ts_names, ts_units = [], [], []
    geojson, metadata, timeseries = [], [], []

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
    # Potentially split stations into forecast and observation station sets
    if split_obs_fcs:
        # Observations data (1st list element)
        stat_details.append(station_details[station_details['obs_station'] == 1])
        obs = get_time_series_data(station_iris, observation_types, True, False,
                                   tmin, tmax, query_endpoint, update_endpoint)
        ts_data.append(obs[0]); ts_names.append(obs[1]); ts_units.append(obs[2])
        # Forecast data (2nd list element)
        stat_details.append(station_details[station_details['fcs_station'] == 1])
        fcs = get_time_series_data(station_iris, observation_types, False, True,
                                   tmin, tmax, query_endpoint, update_endpoint)
        ts_data.append(fcs[0]); ts_names.append(fcs[1]); ts_units.append(fcs[2])
    else:
        stat_details.append(station_details)
        obs_fcs = get_time_series_data(station_iris, observation_types, True, True,
                                       tmin, tmax, query_endpoint, update_endpoint)
        ts_data.append(obs_fcs[0]); ts_names.append(obs_fcs[1]); ts_units.append(obs_fcs[2])
    t2 = time.time()
    diff = t2-t1
    print(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    #logger.info('Time series successfully retrieved.')

    #
    ###---  Create output files  ---###
    #
    # Initialise time series client   
    ts_client = TSClient.tsclient_with_default_settings()
    # Create output file for each set of retrieved time series data
    for i in range(len(ts_data)):
        # Get output data
        ts = ts_data[i]
        units = ts_units[i]
        names = ts_names[i]    
        stations = stat_details[i]
        # Get plotting properties
        co = colors[i]
        op = opacities[i]

        # Convert unintelligible ontology of units of measure symbols for DTVF
        for u in units:
            u.update((k, v.replace('&#x00B0;','°')) for k, v in u.items())

        print('Creating output files (geojson, metadata, timeseries) ...')
        #logger.info('Creating output files (geojson, metadata, timeseries) ...')
        t1 = time.time()

        # 1) Create GeoJSON file for ReportingStations
        geojson.append(create_geojson_output(stations, co, op))

        # 2) Create JSON file for ReportingStations metadata
        metadata.append(create_metadata_output(stations))

        # 3) Create Time series output    
        # Get List of corresponding dtvf ids for list of time series
        # (to assign time series output to correct station in DTVF)
        dataIRIs = [t.getDataIRIs()[0] for t in ts]
        id_list = [int(stations.loc[stations['dataIRI'] == i, 'dtvf_id'].values) for i in dataIRIs]
        tsjson = ts_client.convertToJSON(ts, id_list, units, names)
        # Make JSON file readable in Python
        timeseries.append(json.loads(tsjson.toString()))
        t2 = time.time()
        diff = t2-t1
        print(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
        #logger.info('Output files successfully created.')

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
        with open(fp_timeseries[i], 'w') as f:
            json.dump(timeseries[i], indent=4, fp=f)
    print('Finished!\n')


if __name__ == '__main__':

    # Create 1 joint time series output file
    # create_json_output_files('C:\TheWorldAvatar-git\Agents\MetOfficeAgent\output',
    #                          split_obs_fcs=False)

    # Create 2 separate time series output file
    create_json_output_files('C:\TheWorldAvatar-git\Agents\MetOfficeAgent\output')

    # create_json_output_files('C:\TheWorldAvatar-git\Agents\MetOfficeAgent\output',
    #                          circle_center='52.75#0.4', circle_radius='100')

    # create_json_output_files('C:\TheWorldAvatar-git\Agents\MetOfficeAgent\output',
    #                          circle_center='52.75#0.4', circle_radius='100',
    #                          observation_types=['AirTemperature'])
