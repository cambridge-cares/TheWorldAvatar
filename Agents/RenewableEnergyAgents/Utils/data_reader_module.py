##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 25 Nov 2021                      #
##########################################

"""This module enables users to read data from a CSV file from any column identified by
the column header (aka column name)."""

import pandas as pd

# Declared string constants for the CSV files
data_file_wind = './data/newcastle_data_wind.csv'
data_file_solar = './data/newcastle_data_solar.csv'

# Declared string constants for the column headers
column_sensor_name = 'Sensor Name'
column_variable = 'Variable'
column_units = 'Units'
column_timestamp = 'Timestamp'
column_value = 'Value'
column_flagged_suspect = 'Flagged as Suspect Reading'
column_ground_height = 'Ground Height Above Sea Level'
column_sensor_height = 'Sensor Height Above Ground'
column_longitude = 'Sensor Centroid Longitude'
column_latitude = 'Sensor Centroid Latitude'
column_raw_id = 'Raw ID'

"""Given a CSV file it loads data to a global dataframe"""
def load_data(data_file):
    # df stands for dataframe
    global df
    df = pd.read_csv(data_file)

"""Given a column name it returns all values associated with the column"""
def get_column_data(column_name):
    return df[column_name]

"""Given a dataframe and column name it returns all values associated with the column"""
def get_df_column_data(df, column_name):
    return df[column_name]

"""Given a column name and a value that can appear in the column, it returns all rows containing the value"""
def get_selected_rows(column_name, value):
    return df.loc[df[column_name] == value]

"""Given a dataframe, column name and value that can appear in the column, it returns all rows containing the value"""
def get_df_selected_rows(df, column_name, value):
    return df.loc[df[column_name] == value]

"""It shows how to iterate over the rows of a column"""
def iterate_over(column_df):
    for row in column_df:
        print (row)

"""This function will execute if this Python module or file is run from a command prompt or an IDE"""
if __name__ == '__main__':
    load_data(data_file_wind)
    print(get_column_data(column_value))
    print(get_df_column_data(df, column_value))
    iterate_over(get_column_data(column_value))
    iterate_over(get_df_column_data(get_selected_rows(column_sensor_name, 'PER_EMLFLOOD_HEPPLE'), column_value))
    print(get_selected_rows(column_sensor_name, 'PER_EMLFLOOD_HEPPLE'))