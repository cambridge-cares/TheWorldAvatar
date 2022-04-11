###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 11 Apr 2022                           #
###############################################

# The purpose of this module is to provide functionality to structure
# output information retrieved from the KG in DTVF suitable formats

import os
import copy

#import agentlogging
from configobj import ConfigObj
from pathlib import Path

# Initialise logger
#logger = agentlogging.get_logger("dev")


# Define default properties for station features in geojson 
GEOJSON_STATION_PROPERTIES = {
    'displayName': '',
    'circle-color': '',
    'circle-stroke-width': 1,
    'circle-stroke-color': '#000000',
    'circle-stroke-opacity': 0.75,
    'circle-opacity': 0.66
}


def geojson_initialise_dict():
    # Start GeoJSON FeatureCollection
    geojson = {'type': 'FeatureCollection',
               'features': []
               }
    return geojson


def geojson_add_point_feature(feature_id: int, properties: dict, 
                              longitude: float, latitude: float,):
    # Define new GeoJSON point feature
    # GeoJSON requires coordinates as [longitude, latitude]
    feature = {'type': 'Feature',
               'id': feature_id,
               'properties': properties,
               'geometry': {'type': 'Point',
                            'coordinates': [
                                longitude,
                                latitude
                            ]
                        }
               }
    return feature


def create_geojson_output(station_data, color: str = '#ff0000'):
    """
        Create GeoJSON file with geospatial data for Met Office stations

        Arguments
            station_data - DataFrame with details about MetOffice stations
                           (['stationID', 'station', 'comment', 'latlon', 'elevation'])
            color - hex color code for station circles
    """

    # Initialise geojson dict
    output = geojson_initialise_dict()

    # Set geojson plotting properties
    geojson_props = GEOJSON_STATION_PROPERTIES.copy()
    geojson_props['circle-color'] = color

    # Get unique stations
    data = station_data.drop_duplicates(subset='station')

    # Append GeoJSON features
    for index, row in data.iterrows():
        # Retrieve data from DataFrame
        dtvf_id = int(row['dtvf_id'])
        lat, lon = row['latlon'].split('#')
        lat = float(lat)
        lon = float(lon)
        props = geojson_props.copy()
        props['displayName'] = row['comment']
        # Append data to GeoJSON
        # Blazegraph stores coordinates as lat#lon; however GeoJSON requires
        # reversed order, i.e. longitude first
        output['features'].append(geojson_add_point_feature(dtvf_id, props, lon, lat))
    
    return output







# def json_add_metadata(feature_id, lon, lat, elec, water, gas):
#     # Define metadata dictionary
#     metadata = { 'id': feature_id,
#                  'Data provider': "UK Met Office",
#                  'Station name': lat,
#                  'Met Office Station ID':
#                  'Latitude': elec,
#                  'Longitude': water,
#                  'Elevation': 'm',
#                  }
#     return metadata

