###############################################
# Author: Markus Hofmeister (mh807@cam.ac.uk) #
# Date: 14 Dec 2021                           #
###############################################
#slightly updated for previous python versions by Toby Latcham
"""This module retrieves MIDAS sensor data from CEDA Archive website."""

import pandas as pd
import requests

def get_sensor_coords():
    ''' Get the coordinates for each desired sensor from
    the MIDAS website.
    '''
    # extract sensor IDs
    df = pd.read_csv('./Data/MIDAS_wind_data/MIDASwind.csv')
    ids = list(df['SRC_ID'].unique())

    # define request url and correct column names
    root_url = 'https://archive.ceda.ac.uk/cgi-bin/midas_stations/station_details.cgi.py?idstring='
    column_names = ['src_id', 'Station name', 'Area', 'Area type',
                    'Station start date', 'Station end date', 'Latitude',
                    'Longitude', 'Postcode', 'Open datasets available']
    # initialise output Dataframe
    station_data = pd.DataFrame()

    # retrieve data for all IDs
    for id in ids:
        # construct full url
        url = root_url + str(id)
        # get html page content
        page = requests.get(url)
        try:
            # parses table in webpage to pd.DataFrame
            table_data = pd.read_html(page.text)
            # append table_data to overall DataFrame
            station_data = station_data.append(table_data[0], ignore_index=True)
        except:
            print('No table could be read for Sensor ID {}'.format(id))

    # add correct column names
    station_data.columns = column_names

    #Fix station names by removing ' locate' from the end
    n=0
    for broken_name in station_data['Station name']:
        station_data['Station name'][n] = broken_name[:-7]
        n+=1


    # write output data
    station_data.to_csv('./Data/MIDAS_wind_data/stationlocations2.csv', index=False)