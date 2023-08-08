###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 01 Apr 2022                           #
###############################################

# The purpose of this module is to provide functions to retrieve 
# station data from the KG

import json
import os
import pathlib
import re
import time
import pandas as pd

from py4jps import agentlogging
from agent.dataretrieval.readings import get_time_series_data
from agent.errorhandling.exceptions import InvalidInput
from agent.kgutils.kgclient import KGClient
from agent.kgutils.querytemplates import *
from agent.kgutils.stackclients import OntopClient
from agent.kgutils.tsclient import TSClient
from agent.utils.output_formatting import (create_geojson_output,
                                           create_metadata_output)
from agent.utils.stack_configs import (DB_PASSWORD, DB_URL, DB_USER,
                                       QUERY_ENDPOINT, UPDATE_ENDPOINT,
                                       ONTOP_URL)

# Initialise logger
logger = agentlogging.get_logger("prod")


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
            circle_center - center for geospatial 'inCircle' search (via PostGIS)
                            coordinates as 'latitude#longitude' in EPSG:4326
            circle_radius - radius in km
    """
    
    # Validate input
    if circle_center and not circle_radius or \
       circle_radius and not circle_center:
        logger.error("Circle center or radius is missing for geospatial search.")
        raise InvalidInput("Circle center or radius is missing for geospatial search.")
    if circle_center:
        if not re.findall(r'[\w\-\.]*#[\w\-\.]*', circle_center):
            logger.error("Circle center coordinates shall be provided as " \
                          +"\"latitude#longitude\" in EPSG:4326 coordinates.")
            raise InvalidInput("Circle center coordinates shall be provided as " \
                               +"\"latitude#longitude\" in EPSG:4326 coordinates.")

    # Construct KG client with correct query
    query_string = instantiated_metoffice_stations(circle_center=circle_center,
                                                   circle_radius=circle_radius)
    kg_client = KGClient(query_endpoint, update_endpoint)
    # Execute query
    results = kg_client.performQuery(query=query_string)
    # Extract results in required format
    res = [(r['stationID'], r['station']) for r in results]
    res = dict(res)
    
    return res


def get_all_stations_with_details(query_endpoint: str = QUERY_ENDPOINT,
                                  update_endpoint: str = UPDATE_ENDPOINT,
                                  circle_center: str = None,
                                  circle_radius: str = None):
    """
        Returns DataFrame with all instantiated Met Office station details
        (['stationID', 'station', 'label', 'latlon', 'elevation', 
          'obs_station', 'fcs_station', 'dataIRI'])

        Arguments:
            circle_center - center for geospatial 'inCircle' search (via PostGIS)
                            coordinates as 'latitude#longitude' in EPSG:4326
            circle_radius - radius in km
    """

    # Validate input
    if circle_center and not circle_radius or \
       circle_radius and not circle_center:
        logger.error("Circle center or radius is missing for geospatial search.")
        raise InvalidInput("Circle center or radius is missing for geospatial search.")
    if circle_center:
        if not re.findall(r'[\w\-\.]*#[\w\-\.]*', circle_center):
            logger.error("Circle center coordinates shall be provided as " \
                          +"\"latitude#longitude\" in EPSG:4326 coordinates.")
            raise InvalidInput("Circle center coordinates shall be provided as " \
                               +"\"latitude#longitude\" in EPSG:4326 coordinates.")

    # Construct KG client, set query and execute
    kg_query = instantiated_metoffice_stations_with_details(circle_center=circle_center,
                                                            circle_radius=circle_radius)                                            
    kg_client = KGClient(query_endpoint, update_endpoint)
    results = kg_client.performQuery(query=kg_query)

    # Extract all (unique) station IRIs (neglect potential Nones)
    station_iris = list(set([r.get('station') for r in results]))
    station_iris = [s for s in station_iris if s is not None]
    
    # NOTE: OntopClient seems to have issues with large queries; hence query in batches
    # Furthermore, OntopClient has issues with frequent connections; hence query via Blazegraph (for now)
    # ontop_client = OntopClient()
    n = 500     # batch size
    latlon_all = {}
    station_iris = [station_iris[i:i + n] for i in range(0, len(station_iris), n)]

    for iris in station_iris:
        # Set query and execute
        # NOTE This query tends to fail if 1) Ontop has been running for a while
        #      and/or 2) recurring requests have been made to it 
        #      --> Query Ontop via Blazegraph (for now)
        ontop_query = geospatial_station_info(iris, ONTOP_URL)
        res = kg_client.performQuery(ontop_query)
        # PostGIS documentation: For geodetic coordinates, X is longitude and Y is latitude
        lonlat = {r['station']: r['wkt'][r['wkt'].rfind('(')+1:-1].split(' ') for r in res}
        latlon = {k: '#'.join(v[::-1]) for k,v in lonlat.items()}
        # Append to overall dictionary
        latlon_all.update(latlon)

    # Parse results into DataFrame and map geospatial information to station IRIs
    df = pd.DataFrame(columns=['stationID', 'station', 'label', 'latlon', 
                               'elevation', 'dataIRI_obs', 'dataIRI_fc'])
    df = df.append(results)
    df['latlon'] = df['station'].map(latlon_all)
    # Drop stations without geo
    df = df.dropna(subset=['latlon'])
    # Add station classification (one hot encoded)
    df['obs_station'] = df['dataIRI_obs'].isna().apply(lambda x: 0 if x else 1)
    df['fcs_station'] = df['dataIRI_fc'].isna().apply(lambda x: 0 if x else 1)
    # Consolidate dataIRI columns
    df['dataIRI'] = df[['dataIRI_obs', 'dataIRI_fc']].values.tolist()
    df = df.drop(columns=['dataIRI_obs', 'dataIRI_fc'])
    df = df.explode('dataIRI').reset_index(drop=True)
    # Overwrite NaNs in dataIRI column for stations without any attached dataIRIs
    mask=(df['obs_station'] + df['fcs_station'] == 0)
    df.loc[mask, 'dataIRI'] = ''
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
            circle_center - center for geospatial 'inCircle' search (via PostGIS)
                            coordinates as 'latitude#longitude' in EPSG:4326
            circle_radius - radius in km         
            tmin - oldest time step for which to retrieve data
            tmax - latest time step for which to retrieve data
            split_obs_fcs - boolean flag
    """

    # Validate input
    if not pathlib.Path.exists(pathlib.Path(outdir)):
        logger.error('Provided output directory does not exist.')
        raise InvalidInput('Provided output directory does not exist.')
    else:
        if not split_obs_fcs:
            fp_geojson = [os.path.join(pathlib.Path(outdir), 'metoffice_stations.geojson'),
                          os.path.join(pathlib.Path(outdir), 'metoffice_stations_woTS.geojson')]
            fp_metadata = [os.path.join(pathlib.Path(outdir), 'metoffice_stations-meta.json'),
                           os.path.join(pathlib.Path(outdir), 'metoffice_stations_woTS-meta.json')]
            fp_timeseries = [os.path.join(pathlib.Path(outdir), 'metoffice_stations-timeseries.json'),
                             os.path.join(pathlib.Path(outdir), 'metoffice_stations_woTS-timeseries.json')]
            colors = ['#C0392B']
            opacities = [0.66]
        else:
            fp_geojson = [os.path.join(pathlib.Path(outdir), 'metoffice_observation_stations.geojson'),
                          os.path.join(pathlib.Path(outdir), 'metoffice_forecast_stations.geojson'),
                          os.path.join(pathlib.Path(outdir), 'metoffice_stations_woTS.geojson')]
            fp_metadata = [os.path.join(pathlib.Path(outdir), 'metoffice_observation_stations-meta.json'),
                           os.path.join(pathlib.Path(outdir), 'metoffice_forecast_stations-meta.json'),
                           os.path.join(pathlib.Path(outdir), 'metoffice_stations_woTS-meta.json')]
            fp_timeseries = [os.path.join(pathlib.Path(outdir), 'metoffice_observation_stations-timeseries.json'),
                             os.path.join(pathlib.Path(outdir), 'metoffice_forecast_stations-timeseries.json'),
                             os.path.join(pathlib.Path(outdir), 'metoffice_stations_woTS-timeseries.json')]
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
    #print('Retrieving instantiated stations from KG ...')
    logger.info('Retrieving instantiated stations from KG ...')
    t1 = time.time()
    station_details = get_all_stations_with_details(query_endpoint, update_endpoint,
                                                    circle_center, circle_radius)
    t2 = time.time()
    diff = t2-t1
    #print(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    logger.info(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    logger.info('Stations successfully retrieved.')
    
    # Extract station IRIs of interest
    station_iris = list(station_details['station'].unique())
    # Assign ids to stations (required for DTVF)
    dtvf_ids = dict(zip(station_iris, range(len(station_iris))))
    station_details['dtvf_id'] = station_details['station'].map(dtvf_ids)
   
    # 2) Get time series data
    #print('Retrieving time series data from KG ...')
    logger.info('Retrieving time series data from KG ...')
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
        stat_details.append(station_details[(station_details['obs_station'] == 1) |
                                            (station_details['fcs_station'] == 1)])
        obs_fcs = get_time_series_data(station_iris, observation_types, True, True,
                                       tmin, tmax, query_endpoint, update_endpoint)
        ts_data.append(obs_fcs[0]); ts_names.append(obs_fcs[1]); ts_units.append(obs_fcs[2])
    t2 = time.time()
    diff = t2-t1
    #print(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    logger.info(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    logger.info('Time series successfully retrieved.')

    #
    ###---  Create output files  ---###
    #
    # Initialise KG and TimeSeries Clients
    kg_client = KGClient(query_endpoint, update_endpoint)
    ts_client = TSClient(kg_client=kg_client, rdb_url=DB_URL, rdb_user=DB_USER, 
                         rdb_password=DB_PASSWORD)

    #print('Creating output files (geojson, metadata, timeseries) ...')
    logger.info('Creating output files (geojson, metadata, timeseries) ...')
    t1 = time.time()

    # Create output files for each set of retrieved time series data
    for i in range(len(ts_data)):
        # Get output data
        ts = ts_data[i]
        units = ts_units[i]
        names = ts_names[i]    
        stations = stat_details[i]
        # Get plotting properties
        co = colors[i]
        op = opacities[i]

        if stations.empty:
            geojson.append(create_geojson_output(stations, co, op))
            metadata.append(create_metadata_output(stations))
            timeseries.append([])
        else:
            # Convert unintelligible ontology of units of measure symbols for DTVF
            for u in units:
                u.update((k, v.replace('&#x00B0;','°')) for k, v in u.items())

            # 1) Create GeoJSON file for ReportingStations
            geojson.append(create_geojson_output(stations, co, op))

            # 2) Create JSON file for ReportingStations metadata
            metadata.append(create_metadata_output(stations))

            # 3) Create Time series output    
            # Get List of corresponding dtvf ids for list of time series
            # (to assign time series output to correct station in DTVF)
            
            dataIRIs = [t.getDataIRIs()[0] for t in ts]
            id_list = [int(stations.loc[stations['dataIRI'] == i, 'dtvf_id'].values) for i in dataIRIs]
            tsjson = ts_client.tsclient.convertToJSON(ts, id_list, units, names)
            # Make JSON file readable in Python
            timeseries.append(json.loads(tsjson.toString()))

    t2 = time.time()
    diff = t2-t1
    #print(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    logger.info(f'Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    logger.info('Output files successfully created.')

    # Create output files for stations without any time series data
    stations = station_details[(station_details['obs_station'] == 0) & 
                               (station_details['fcs_station'] == 0)]
    geojson.append(create_geojson_output(stations, '#C0392B', 0.66))
    metadata.append(create_metadata_output(stations))
    timeseries.append([])

    #
    ###---  Write output files  ---###
    #
    #print('Writing output files ...')
    logger.info('Writing output files ...')
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
