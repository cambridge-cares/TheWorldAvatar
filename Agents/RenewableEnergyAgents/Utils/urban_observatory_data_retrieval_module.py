###########################################
# Authors: Toby Latcham (tjl47@cam.ac.uk) #
#          Sophie Hall (sh2000@cam.ac.uk) #
# Date: 11 Feb 2022                       #
###########################################




import requests
import pandas as pd
import io
# import matplotlib.patheffects as pe

# Data Reader module, make sure it's in the same directory
from Utils.data_reader_module import *


# ----------------------
# retrieve data via API
def retrieve_data(sensor_type, variable, start_time, end_time, frequency):
    '''Retrieve all data of a desired type from the Newcastle Observatory website.'''
    sensor_params = dict({'subset_shapes': 'NE1', 'sensor_type': sensor_type})
    r = requests.get('http://uoweb3.ncl.ac.uk/api/v1.1/sensors/csv/', sensor_params)
    print(r.status_code)
    if r.status_code == 502:
        raise Exception('ERROR: Bad Gateway')
    sensor_info = pd.read_csv(io.StringIO(r.text))
    sensor_info

    api_date_string_format = "%Y%m%d%H%M%S"

    data_params = dict({'data_variable': variable, 'agg_method': 'median', 'agg_period': frequency,
                        'starttime': start_time.strftime(api_date_string_format),
                        'endtime': end_time.strftime(api_date_string_format)})
    data_params.update(sensor_params)
    data_params

    data_response = requests.get('http://uoweb3.ncl.ac.uk/api/v1.1/sensors/data/csv', data_params)
    print(data_response.status_code)
    if data_response.status_code == 502:
        raise Exception('ERROR: Bad Gateway')

    return data_response

def retrieve_data_individual(variable, start_time, end_time, frequency):
    '''Retrieve all data for a desired sensor from the Newcastle Observatory website.'''
    sensor_params = dict({'subset_shapes': 'NE1'})
    r = requests.get('http://uoweb3.ncl.ac.uk/api/v1.1/sensors/csv/', sensor_params)
    print(r.status_code)
    if r.status_code == 502:
        raise Exception('ERROR: Bad Gateway')
    sensor_info = pd.read_csv(io.StringIO(r.text))
    sensor_info

    api_date_string_format = "%Y%m%d%H%M%S"

    data_params = dict({'data_variable': variable, 'agg_method': 'median', 'agg_period': frequency,
                        'starttime': start_time.strftime(api_date_string_format),
                        'endtime': end_time.strftime(api_date_string_format)})
    data_params.update(sensor_params)
    data_params

    data_response = requests.get('http://uoweb3.ncl.ac.uk/api/v1.1/sensors/data/csv', data_params)
    print(data_response.status_code)
    if data_response.status_code == 502:
        raise Exception('ERROR: Bad Gateway')

    return data_response