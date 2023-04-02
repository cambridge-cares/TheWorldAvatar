################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 28 Apr 2022                            #
################################################

# The purpose of this module is to provide functionality to structure
# output information retrieved from the KG in DTVF suitable formats
# (i.e. this is mainly deprecated due to DTVF visualisation mainly served via
# Postgis and Geoserver; however, this functionality is kept as "backup")

import math
from math import nan


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
                              longitude: float, latitude: float):
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


def create_geojson_output(station_data, color: str = '#C0392B',
                          opacity: float = 0.66):
    """
        Create GeoJSON file with geospatial data for UK AIR stations

        Arguments
            station_data - DataFrame with details about UK AIR stations
                           (['stationID', 'station', 'label', 'latlon', 'elevation', 'dtvf_id'])
            color - hex color code for station circles
    """

    # Initialise geojson dict
    output = geojson_initialise_dict()

    # Set geojson plotting properties
    geojson_props = GEOJSON_STATION_PROPERTIES.copy()
    geojson_props['circle-color'] = color
    geojson_props['circle-opacity'] = opacity

    # Get unique stations
    data = station_data.drop_duplicates(subset='station')

    # Append GeoJSON features
    for index, row in data.iterrows():
        if row['latlon'] and row['latlon'] is not nan:
            # Retrieve data from DataFrame
            dtvf_id = int(row['dtvf_id'])
            lat, lon = row['latlon'].split('#')
            lat = float(lat)
            lon = float(lon)
            props = geojson_props.copy()
            props['displayName'] = row['label'] if row['label'] and \
                                   row['label'] is not nan else 'n/a'
            # Append data to GeoJSON
            # Blazegraph stores coordinates as lat#lon; however GeoJSON requires
            # reversed order, i.e. longitude first
            output['features'].append(geojson_add_point_feature(dtvf_id, props, lon, lat))
    
    return output


def json_add_metadata(feature_id: int, name: str, longitude: float, 
                      latitude: float, elevation: float):
    # Define metadata dictionary
    metadata = {'id': feature_id,
                'Station name': name if name and name is not nan else 'n/a',
                'Data provider': "UK-AIR Sensor Observation Service",                
                'Latitude': latitude,
                'Longitude': longitude,
                'Elevation': f'{elevation} m' if elevation and not math.isnan(elevation) \
                             else 'n/a',
                }
    return metadata


def create_metadata_output(station_data):
    """
        Create JSON file with metadata for UK AIR stations

        Arguments
            station_data - DataFrame with details about MetOffice stations
                           (['stationID', 'station', 'label', 'latlon', 'elevation'])
    """

    # Initialise geojson dict
    output = []

    # Get unique stations
    data = station_data.drop_duplicates(subset='station')

    # Append JSON features
    for index, row in data.iterrows():
        # Retrieve data from DataFrame
        dtvf_id = int(row['dtvf_id'])
        name = row['label']
        if '#' in row['latlon']:
            lat, lon = row['latlon'].split('#')
            lat = float(lat)
            lon = float(lon)
        else:
            lat = lon = 'n/a'
        elev = float(row['elevation'])        
        # Append data to metadata JSON
        output.append(json_add_metadata(dtvf_id, name, lon, lat, elev))
    
    return output
