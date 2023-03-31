###########################################
# Authors: Toby Latcham (tjl47@cam.ac.uk) #
#          Sophie Hall (sh2000@cam.ac.uk) #
# Date: 22 Mar 2022                       #
###########################################

import pandas as pd

from Utils.data_reader_module import *

def process_wind_speed_data(wind_speed_response, wind_data_file):
    '''Returns a dictionary containing all desired data for each
    extracted Newcastle Observatory sensor.'''
    windspeedtext = wind_speed_response.text

    with open(wind_data_file, 'w', newline='') as f:
        f.write(windspeedtext)

    df = pd.read_csv(wind_data_file)

    wind_sensor_list = get_df_column_data(df, 'Sensor Name')

    wind_sensor_names = {}
    for sensor in wind_sensor_list:
        # getting coords
        rows = get_df_selected_rows(df, 'Sensor Name', sensor)
        lat0 = get_df_column_data(rows, 'Sensor Centroid Latitude')
        lat = str(lat0.iloc[1])
        lon0 = get_df_column_data(rows, 'Sensor Centroid Longitude')
        lon = str(lon0.iloc[1])
        # getting timeseries data
        wind_timestamps_formatted = []
        wind_timestamps = get_df_column_data(rows, 'Timestamp')
        wind_values_formatted = []        
        wind_values = get_df_column_data(rows, 'Value')
        n = 0
        sensor_function_test = 0 # some sensors are broken, this tests for that
        for wind_timestamp in wind_timestamps:
            wind_timestamp = wind_timestamp.replace(' ', 'T') + "Z"
            wind_timestamps_formatted.append(wind_timestamp)
            wind_values_formatted.append(wind_values.iloc[n])
            if int(wind_values.iloc[n]) > 100: # highest ever windspeed was about 77ms in UK, higher than 100 is clearly an error
                sensor_function_test += 1
            n+=1
        if sensor_function_test == 0:
            wind_sensor_names[sensor] = [wind_timestamps_formatted, wind_values_formatted, lon, lat]

    return wind_sensor_names


def format_wind_sensors(wind_sensor_names):
    '''Returns a formatted dictionary of all sensor data.'''
    wind_sensors = []
    for i in wind_sensor_names.keys():
        sensor_dict = {}
        sensor_dict['sensor name'] = i
        sensor_dict['lat'] = wind_sensor_names[i][3]
        sensor_dict['lon'] = wind_sensor_names[i][2]
        sensor_dict['times'] = wind_sensor_names[i][0]
        sensor_dict['timeseries'] = {'WindSpeed': wind_sensor_names[i][1]}
        wind_sensors.append(sensor_dict)

        #print(sensor_dict)
    return wind_sensors