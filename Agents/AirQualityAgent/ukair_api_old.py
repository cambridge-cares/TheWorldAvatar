###############################################
# Author: Markus Hofmeister (mh807@cam.ac.uk) #
# Date: 10 Feb 2022                           #
###############################################

""" 
This module investigates the capabilities of the UK Air SOS service
https://uk-air.defra.gov.uk/data/about_sos

API documentation has been shared via mail and seems unavailable online
"""

import json
import datetime as dt
import pandas as pd
import requests


# Get stations (sensors): either all or stations in area of interest
# 1) basic information for all staions
#url = 'https://uk-air.defra.gov.uk/sos-ukair/api/v1/stations'
# Request station coordinates in particular CRS (default is WGS:84)
#url = 'https://uk-air.defra.gov.uk/sos-ukair/api/v1/stations?crs=EPSG:27700'
#url = 'https://uk-air.defra.gov.uk/sos-ukair/api/v1/stations?crs=EPSG:4326'
# 2) more detailed information for all stations
url = 'https://uk-air.defra.gov.uk/sos-ukair/api/v1/stations?expanded=true'
# 3) Only stations in area of interest - documentation proposes bounding box and circle,
#    does not work for me (issue also mentioned in documentation)
stations_raw = requests.get(url=url).json()

# Create DataFrame from json response
stations = [{'station': s['properties']['label'].split('-')[0],
              # StationID seems not to be unique identifier for a station, but for a feature; hence,
              # a station with several measurement features has different IDs
              #'station_id': s['properties']['id'],
              'longitude': s['geometry']['coordinates'][1],
              'latitude': s['geometry']['coordinates'][0],
              'timeseries_id': list(s['properties']['timeseries'].keys())
             } for s in stations_raw ]
df = pd.DataFrame(stations)
df = df.groupby(by=['station']).agg(lambda x: list(x))
df['longitude'] = df['longitude'].apply(lambda x: list(set(x)))
df['latitude'] = df['latitude'].apply(lambda x: list(set(x)))
df['timeseries_id'] = df['timeseries_id'].apply(lambda x: [item for sublist in x for item in sublist])

# Assess data quality
unique_stations = df[(df['latitude'].apply(lambda x: len(x)) == 1) & (df['longitude'].apply(lambda x: len(x)) == 1)]
print('Share of unique stations: {:%}'.format(len(unique_stations)/len(df)))

unique_stations = unique_stations[:2]

# Get data for individual time series
# Get time series IDs based on feature/site IDs
ts_ids = [item for sublist in list(unique_stations['timeseries_id']) for item in sublist]
for ts in ts_ids:
    # Get timeseries information & metadata
    url = 'https://uk-air.defra.gov.uk/sos-ukair/api/v1/timeseries/{}'.format(ts)
    ts_data = requests.get(url=url).json()
    # 1) unit and phenomenon
    # Links to EEA data dictionary used for the pollutant (phenomena)
    # http://dd.eionet.europa.eu/vocabulary/aq/pollutant/view
    unit = ts_data['uom']
    phenomenon = ts_data['parameters']['phenomenon']['label']
    procedure = ts_data['parameters']['procedure']['label']

    # 2) time series data
    # Returned timestamps are in ms and need to be converted to s first
    # Time filters for timeseries data require ISO8601 formatted period notation
    earliest = dt.datetime.utcfromtimestamp(ts_data['firstValue']['timestamp']/1000).isoformat()
    latest = dt.datetime.utcfromtimestamp(ts_data['lastValue']['timestamp']/1000).isoformat()

    # Get last 7 days
    url = 'https://uk-air.defra.gov.uk/sos-ukair/api/v1/timeseries/{}/getData'.format(ts)
    # Get period of 1 year starting at 00:00 hours, 1 January 2019
    # https://uk-air.defra.gov.uk/sos-ukair/api/v1/timeseries/{id}/getData?timespan=2019-01-01T00:00:00Z/P1Y
    # Get period of 1 year ending at 00:00 hours, 1 January 2019
    # https://uk-air.defra.gov.uk/sos-ukair//api/v1/timeseries/{id}/getData?timespan=P1Y/2019-01-01T00:00:00Z
    # Get period of 1 month starting at 00:00 hours, 1 January 2019
    # https://uk-air.defra.gov.uk/sos-ukair/api/v1/timeseries/{id}/getData?timespan=2019-01-01T00:00:00Z/P1M
    # Get period of 1 week starting at 00:00 hours, 1 January 2019
    # https://uk-air.defra.gov.uk/sos-ukair/api/v1/timeseries/{id}/getData?timespan=2019-01-01T00:00:00Z/P1W
    # Get period between 00:00 hours, 1 January 2019 & 1 January 2020
    # https://uk-air.defra.gov.uk/sos-ukair/api/v1/timeseries/{id}/getData?timespan=2019-01-01T00:00:00Z/2020-01-01T00:00:00Z
    # MAXIMUM RETRIEVAL DURATION IS 1 YEAR
    ts_data = requests.get(url=url).json()
    ts_data = ts_data['values']

# Get data for multiple time series at once
# url = 'https://uk-air.defra.gov.uk/sos-ukair/api/v1/timeseries/getData'
# header = {'Content-Type': 'application/json'}
# payload = { 'timespan': '2022-01-01T13:00:00Z/2022-01-10T13:00:00ZZ', 
#             'timeseries': ts_ids[:3] }

# r = requests.post(url=url, headers=header, data=payload)
