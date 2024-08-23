###############################################
# Author: Markus Hofmeister (mh807@cam.ac.uk) #
# Date: 02 Mar 2023                           #
###############################################

""" 
This module helps to identify the most relevant flood areas around King' Lynn
and create mock API responses for those areas
1) Retrieves flood areas around King's Lynn from API
2) Extracts flood warnings for these areas from previously collected data
   (gives some idea about how mock API responses could look like)
"""

import json
import os
import pandas as pd
import random
import requests

from pathlib import Path


# Specify area of interest (King's Lynn center & radius)
# details: https://environment.data.gov.uk/flood-monitoring/doc/reference#flood-areas
lat = 52.752
lon = 0.395
r = 25

# Define relative path to txt file containing previously collected flood warnings
fp = '../../../../Data/06 Flood warnings/historical_flood_warnings.txt'

# Define identifiers for most relevant flood areas
# (leave empty list if not known yet, i.e. initial screening of geojson)
# most_relevant = ['052FWTKLNWL',     # west
#                  '052FWTKLNKL1',    # central 
#                  '052FWTKLNKL2'     # east
#                 ]
most_relevant = []

# Define default polygon plotting properties
fill_props = {'displayName': '',
              'description': '',
              'fill-color': '',
              'fill-outline-color': '#000000',
              'fill-opacity': 0.66
              }

# ===============================================================================
# Functions to Structure Retrieved Data for DTVF

def geojson_initialise_dict():
    # Start GeoJSON FeatureCollection
    geojson = {'type': 'FeatureCollection',
               'features': []
               }
    return geojson


def geojson_add_area(feature_id, properties, poly_geometry):
    # Define new GeoJSON feature
    feature = {'type': 'Feature',
               'id': int(feature_id),
               'properties': properties,
               'geometry': poly_geometry }
    return feature


def json_add_area_metadata(feature_id, lon, lat, area_id, description, area_type):
    # Define metadata dictionary
    metadata = { 'id': feature_id,
                 'Longitude': lon,
                 'Latitude': lat,
                 'fwdCode': area_id,
                 'description': description,
                 'riverOrSea': area_type
                 }
    return metadata

def json_add_warning_metadata(feature_id, severity, tidal, message, time):
    # Define metadata dictionary
    metadata = { 'id': feature_id,
                 'SeverityLevel': severity,
                 'isTidal': tidal,
                 'Message': message,
                 'TimeAltered': time
                 }
    return metadata

# ===============================================================================

if __name__ == '__main__':

    # Get all specified flood areas in vicinity of King's Lynn (from API)
    url = 'https://environment.data.gov.uk/flood-monitoring/id/floodAreas?lat={}&long={}&dist={}'.format(lat, lon, r)
    resp = requests.get(url).json()
    areas = pd.DataFrame(resp['items'])

    # Load previously gathered flood warnings
    fp = os.path.join(Path(__file__).parent, Path(fp))
    warnings = pd.read_csv(fp, delimiter='|')

    # Extract relevant flood warnings for areas of interest only (if given)
    if most_relevant:
        areas = areas[areas['fwdCode'].isin(most_relevant)]

    rel_warnings = warnings[warnings['FloodAreaID'].isin(areas['fwdCode'])]
    rel_warnings.drop_duplicates(inplace=True)
    # Groups entries for same areas into lists and sets areaID as index
    rel_warnings = rel_warnings.groupby('FloodAreaID').agg(list)

    # 1) Create plotting data for all relevant flood areas
    # Initialise feature_id and output variables
    flood_areas_geojson = geojson_initialise_dict()
    flood_areas_meta = []
    feature_id = 0
    for i in areas.index:
        feature_id += 1
        # Set properties
        props = fill_props.copy()
        props['displayName'] = areas['fwdCode'][i]
        props['description'] = areas['label'][i]
        props['fill-color'] = ["#"+''.join([random.choice('ABCDEF0123456789') for i in range(6)])][0]
        # Retrieve polygon
        poly = requests.get(areas.loc[i, 'polygon']).json()
        flood_areas_geojson['features'].append(geojson_add_area(feature_id, props, 
                                                                poly['features'][0]['geometry']))
        # Append meta data
        flood_areas_meta.append(json_add_area_metadata(feature_id, areas['long'][i], areas['lat'][i], 
                                areas['fwdCode'][i], areas['description'][i], areas['riverOrSea'][i]))

    # Write all output files
    # Write flood area dictionaries formatted to files
    file_name = os.path.join(Path(__file__).parent, 'flood-areas.geojson')
    with open(file_name, 'w') as f:
        json.dump(flood_areas_geojson, indent=4, fp=f)
    file_name = os.path.join(Path(__file__).parent, 'flood-areas-meta.json')
    with open(file_name, 'w') as f:
        json.dump(flood_areas_meta, indent=4, fp=f)

    # 2) Create plotting data for historic flood alerts
    # Initialise feature_id and output variables
    flood_warnings_geojson = geojson_initialise_dict()
    flood_warnings_meta = []
    feature_id = 0
    # flood areas has been set as index
    for area in rel_warnings.index:
        feature_id += 1
        # Set properties
        props = fill_props.copy()
        props['displayName'] = area
        props['fill-color'] = ["#"+''.join([random.choice('ABCDEF0123456789') for i in range(6)])][0]
        # Retrieve polygon
        poly = requests.get(areas[areas['fwdCode'] == area]['polygon'].values[0]).json()
        flood_warnings_geojson['features'].append(geojson_add_area(feature_id, props, 
                                                  poly['features'][0]['geometry']))
        # Append meta data
        flood_warnings_meta.append(json_add_warning_metadata(feature_id, rel_warnings['SeverityLevel'][area],
                                   rel_warnings['isTidal'][area], rel_warnings['Message'][area], 
                                   rel_warnings['TimeAltered'][area]))

    # Write all output files
    # Write flood area dictionaries formatted to files
    file_name = os.path.join(Path(__file__).parent, 'flood-warnings.geojson')
    with open(file_name, 'w') as f:
        json.dump(flood_warnings_geojson, indent=4, fp=f)
    file_name = os.path.join(Path(__file__).parent, 'flood-warnings-meta.json')
    with open(file_name, 'w') as f:
        json.dump(flood_warnings_meta, indent=4, fp=f)

