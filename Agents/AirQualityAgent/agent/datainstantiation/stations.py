################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 27 Apr 2022                            #
################################################

# The purpose of this module is to provide functions to retrieve 
# station data from the UK Air API and instantiate it in the KG

# The UK Air API does not require any registration or API token

import math
import uuid
import re
import requests
import pandas as pd

from agent.kgutils.querytemplates import *
from agent.kgutils.kgclient import KGClient
from agent.kgutils.stackclients import (GdalClient, GeoserverClient,
                                        OntopClient, PostGISClient,
                                        create_geojson_for_postgis)
from agent.dataretrieval.stations import get_all_airquality_station_ids
from agent.errorhandling.exceptions import APIException, InvalidInput
from agent.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT


# Initialise logger
from py4jps import agentlogging
logger = agentlogging.get_logger("prod")


def instantiate_stations(station_data: list,
                         query_endpoint: str = QUERY_ENDPOINT,
                         update_endpoint: str = UPDATE_ENDPOINT) -> None:
    """
        Instantiates a list of measurement stations in a single SPARQL update
        
        Arguments:
            station_data - list of dictionaries with station parameters as 
                           provided by UK AIR API (already cleaned)
    """
    
    # Initialise relevant Stack Clients and parameters
    postgis_client = PostGISClient()
    gdal_client = GdalClient()
    geoserver_client = GeoserverClient()
    station_type = 'UK-AIR Reporting station'

    # Initialise update query
    query_string = f"""
        INSERT DATA {{
    """

    # Add station details
    for data in station_data:
        station_IRI = KB + 'ReportingStation_' + str(uuid.uuid4())
        # Extract station information from API result
        to_instantiate = _condition_airquality_data(data)
        to_instantiate['station_iri'] = station_IRI
        # Remove location and subtype info to be handled separately
        lat, lon = to_instantiate.pop('location').split('#')

        # Create SPARQL update for Blazegraph
        query_string += add_station_data(**to_instantiate)

        # Create GeoJSON file for upload to PostGIS
        lat = float(lat)
        lon = float(lon)
        station_name = station_type + f' at {lat},{lon}' if not \
                       to_instantiate.get('label') else to_instantiate.get('label')        
        geojson = create_geojson_for_postgis(station_iri=station_IRI, station_name=station_name,
                                             station_type=station_type, lat=lat, long=lon, 
                                             kg_endpoint=query_endpoint)
        
        # Upload OBDA mapping and create Geoserver layer when first geospatial
        # data is uploaded to PostGIS
        if not postgis_client.check_table_exists():
            logger.info('Uploading OBDA mapping ...')
            OntopClient.upload_ontop_mapping()
            # Initial data upload required to create postGIS table and Geoserver layer            
            logger.info('Uploading GeoJSON to PostGIS ...')
            gdal_client.uploadGeoJSON(geojson)
            logger.info('Creating layer in Geoserver ...')
            geoserver_client.create_workspace()
            geoserver_client.create_postgis_layer()
        else:        
            # Upload new geospatial information
            if not postgis_client.check_point_feature_exists(lat, lon, station_type):
                logger.info('Uploading GeoJSON to PostGIS ...')
                gdal_client.uploadGeoJSON(geojson)

    # Close query
    query_string += f"}}"

    # Execute query
    kg_client = KGClient(query_endpoint, update_endpoint)
    kg_client.performUpdate(query_string)


def retrieve_station_data_from_api(crs: str = 'EPSG:4326') -> list:
    """
        Retrieve and condition station data from UK Air API

        Arguments:
            crs - coordinate reference system in which to return station 
                  locations (as EPSG code, e.g. 'EPSG:4326'). EPSG:4326 coordinates
                  are provided as [lat, long] and specifying any other CRS can 
                  potentially result in switched values for lat and lon
        Returns:
            List of dicts with station data as returned by API
    """

    # Construct API call to get basic information for all stations
    if crs and re.match(r"EPSG:\d+", crs):
        url = f'https://uk-air.defra.gov.uk/sos-ukair/api/v1/stations?{crs}'
        if crs != 'EPSG:4326':
            logger.info('Provided CRS is different from "EPSG:4326". Extraction of ' \
                      + 'latitude and longitude can be erroneous.')
    else:
        raise InvalidInput ("Provided CRS does not match expected 'EPSG:' format.")

    try:
        logger.info('Retrieving station data from API ...')
        stations_raw = requests.get(url=url).json()
        logger.info('Station data successfully retrieved.')
    except Exception as ex:
        logger.error(f"Error while retrieving station data from API: {ex}")
        raise APIException("Error while retrieving station data from API.") from ex

    # Create DataFrame from json response and condition data
    # StationIDs are no unique identifiers for stations (e.g. a station with several
    # measurement features has different IDs). Hence, the  station name will also 
    # serve as unique identifier
    stations = [{'station': None if not s.get('properties') else
                            s.get('properties').get('label').split('-')[0],
                 'latitude': None if not s.get('geometry') else 
                             s.get('geometry').get('coordinates')[0],
                 'longitude': None if not s.get('geometry') else 
                              s.get('geometry').get('coordinates')[1],
                 'elevation': None if not s.get('geometry') else 
                              s.get('geometry').get('coordinates')[2],
                } for s in stations_raw ]
    df = pd.DataFrame(stations)
    
    # Clean and condition returned API data
    df = clean_api_data(df)
    df.set_index('stationID', inplace=True)

    # Create return list of dicts
    stations = [{k: v} for k,v in df.to_dict('index').items()]

    return stations


def instantiate_all_stations(query_endpoint: str = QUERY_ENDPOINT,
                             update_endpoint: str = UPDATE_ENDPOINT) -> int:
    """
        Instantiate all air quality stations available via UK Air API

        Returns:
            Number of instantiated stations
    """

    # Get all available stations from API
    available = retrieve_station_data_from_api('EPSG:4326')
    available_ids = [list(d.keys())[0] for d in available]
    #TODO: remove testing limit
    available_ids = available_ids[:5]

    # Get already instantiated stations
    instantiated_ids = get_all_airquality_station_ids(query_endpoint=query_endpoint)

    # Derive not yet instantiated stations
    missing_ids = [s for s in available_ids if not s in instantiated_ids]
    to_instantiate = [d for d in available if list(d.keys())[0] in missing_ids]

    # Instantiate missing stations
    print('Instantiate/update stations in KG ...')
    instantiate_stations(station_data=to_instantiate,
                         query_endpoint=query_endpoint,
                         update_endpoint=update_endpoint)
    print('Stations successfully instantiated/updated.')
    
    return len(missing_ids)


def instantiate_mocked_kingslynn_stations(query_endpoint: str = QUERY_ENDPOINT,
                                          update_endpoint: str = UPDATE_ENDPOINT):
    """
    Instantiate a mocked air quality station within King's Lynn, which simply 
    displays the air quality measurements from the mocked station
    
    NOTE: The UK-AIR sensor observations currently do not cover East Anglia, i.e.
          King's Lynn; hence, we place a virtual station in King's Lynn and simply
          connect it to readings of the specified station
    """
    
    # Define properties of mocked station(s)
    mocked_stations = [
        {"label": "King's Lynn (virtual)",
         "lat": "52.7448",
         "lon": "0.4016",
         "elevation": 5.5,
         "station_to_mock": "Aberdeen Erroll Park",
         },
    ]

    # Initialise KG client
    kg_client = KGClient(query_endpoint, update_endpoint)

    for s in mocked_stations:
        # Create station data list as required by "instantiate_stations"
        data = {}
        data['station'] = s.get('label')
        data['latitude'] = s.get('lat')
        data['longitude'] = s.get('lon')
        data['elevation'] = s.get('elevation')
        station_id = s.get('label') + '_' + s.get('lat') + '#' + s.get('lon')
        station_data = [{station_id: data}]

        # 1) Instantiate mocked station(s) if not yet instantiated
        query = all_airquality_station_ids()
        res = kg_client.performQuery(query)
        if station_id not in [r.get('id') for r in res]:
            instantiate_stations(station_data)

        # 2) Connect mocked station(s) to real station measurements
        # Retrieve identifiers of station to mock and the one used for mocking
        virtual_station = s.get('label')
        mock_station = s.get('station_to_mock')
        # Execute SPARQL update
        query_string = connect_mocked_station(virtual_station=virtual_station,
                                              station_to_mock=mock_station)
        kg_client.performUpdate(query_string)

    return len(mocked_stations)



def _condition_airquality_data(station_data: dict) -> dict:
    """
        Condition retrieved UK AIR data as required for query template
        
        Arguments:
            station_data - station data dict as returned by API
    """

    # Data format as required for "add_station_data" query template
    conditioned = {'dataSource': "UK-AIR Sensor Observation Service",
                   'id': None,
                   'label': None,
                   'location': None,
                   'elevation': None
    }
    # Extract relevant data
    try:
        id = str(list(station_data.keys())[0])
    except:
        logger.warning("No station data to condition for query template.")
        return conditioned

    conditioned['id'] = id
    if 'station' in station_data[id].keys(): 
        conditioned['label'] = str(station_data[id]['station'])
    if 'elevation' in station_data[id].keys():
        if math.isnan(station_data[id]['elevation']):
            conditioned['elevation'] = None
        else:
            conditioned['elevation'] = str(station_data[id]['elevation'])
    if ('latitude' in station_data[id].keys()) and ('longitude' in station_data[id].keys()):
        conditioned['location'] = str(station_data[id]['latitude']) + '#' + str(station_data[id]['longitude'])
    else:
        logger.warning(f"Station {station_data['id']} does not have location data.")
    
    return conditioned


def clean_api_data(dataframe: pd.DataFrame):
    """
        Clean the DataFrame with the raw station data (as returned from API)
        (required columns: ['station', 'latitude', 'longitude', 'elevation'])
    """
    # Create local copy of DataFrame to manipulate
    data = dataframe.copy()

    # Remove meaningless stations / stations with missing data
    stations_to_remove = ['http://environment.data.gov.uk/air', 
                          'GB_SamplingFeature_missingFOI']
    data = data[~data['station'].isin(stations_to_remove)]

    # Remove stations with missing location information
    data = data.dropna(subset=['latitude', 'longitude'])

    # Potentially switch returned coordinates, as some of the returned station
    # locations are far outside the UK and lat/lon are most likely mixed up
    data['switch'] = data.apply(lambda x: check_coordinates(x['latitude'], x['longitude']), axis=1)
    data['latitude_old'] = data['latitude']
    data['longitude_old'] = data['longitude']
    data.loc[data['switch'] == True, 'latitude'] = data['longitude_old']
    data.loc[data['switch'] == True, 'longitude'] = data['latitude_old']

    # Remove duplicates (incl. those which might have formed by switching lat/long)  
    data.drop_duplicates(inplace=True)

    # Round elevation data to 2 decimals and convert 'NaN' to float nan
    data['elevation'] = data['elevation'].astype(float)
    data['elevation'] = data['elevation'].round(2)

    # Round coordinates to 6 digits
    data['latitude'] = data['latitude'].round(6)
    data['longitude'] = data['longitude'].round(6)

    # Construct "arbitrary" but unique station ID, as there is none from the API
    # (e.g. a station with several measurement features has different IDs).
    data['stationID'] = data.apply(lambda x: x['station'] + '_' + str(x['latitude'])
                                              + '#' + str(x['longitude']), axis=1)

    # Drop helper columns
    data.drop(columns=['switch', 'latitude_old', 'longitude_old'], inplace=True)

    return data


def check_coordinates(lat: float, lon:float):
    """
        Check whether a set of lat/long coordinates is outside of the UK
    """

    # Very rough geospatial extend of the UK (in EPSG:4326)
    latitude = (48.0, 61.0)
    longitude = (-7.0, 2.5)

    if (lat < latitude[0] or lat > latitude[1]) and \
       (lon < longitude[0] or lon > longitude[1]):
       return True
    else:
        return False


if __name__ == '__main__':

    response = instantiate_all_stations()
    print(f"Number of instantiated stations: {response}")
