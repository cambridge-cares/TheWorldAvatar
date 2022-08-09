###########################################
# Authors: Toby Latcham (tjl47@cam.ac.uk) #
#          Sophie Hall (sh2000@cam.ac.uk) #
# Date: 22 Mar 2022                       #
###########################################

import pandas as pd

from Utils.data_reader_module import *

def process_solar_data(solar_response, solar_data_file):
    '''Returns a dictionary containing all desired data for each
    extracted Newcastle Observatory sensor.
    '''
    solartext = solar_response.text

    with open(solar_data_file, 'w', newline='') as f:
        f.write(solartext)

    df = pd.read_csv(solar_data_file)

    solar_sensor_list = get_df_column_data(df, 'Sensor Name')

    solar_sensor_names = {}
    for sensor in solar_sensor_list:
        # getting coords
        rows = get_df_selected_rows(df, 'Sensor Name', sensor)
        lat0 = get_df_column_data(rows, 'Sensor Centroid Latitude')
        lat = str(lat0.iloc[1])
        lon0 = get_df_column_data(rows, 'Sensor Centroid Longitude')
        lon = str(lon0.iloc[1])
        # getting timeseries data
        solar_timestamps_formatted = []
        solar_timestamps = get_df_column_data(rows, 'Timestamp')
        solar_values_formatted = []
        solar_values = get_df_column_data(rows, 'Value')
        n=0
        for solar_timestamp in solar_timestamps:
            solar_timestamp = solar_timestamp.replace(' ', 'T') + "Z"
            solar_timestamps_formatted.append(solar_timestamp)
            solar_values_formatted.append(solar_values.iloc[n])
            n+=1
        solar_sensor_names[sensor] = [solar_timestamps_formatted,
                                      solar_values_formatted, lon, lat]

    return solar_sensor_names


def format_solar_sensors(solar_sensor_names):
    '''Returns a formatted dictionary of all sensor data.'''
    solar_sensors = []
    for i in solar_sensor_names.keys():
        sensor_dict = {}
        sensor_dict['sensor name'] = i + ' solar'
        sensor_dict['lat'] = solar_sensor_names[i][3]
        sensor_dict['lon'] = solar_sensor_names[i][2]
        sensor_dict['times'] = solar_sensor_names[i][0]
        sensor_dict['timeseries'] = {'Radiation': solar_sensor_names[i][1]}
        solar_sensors.append(sensor_dict)

        #print(sensor_dict)
    return solar_sensors

