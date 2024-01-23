import argparse
import json
import pandas as pd
import os

import datetime
import pytz

epw_labels = ['year', 'month', 'day', 'hour', 'minute', 'datasource', 'drybulb_C', 'dewpoint_C', 'relhum_percent',
                  'atmos_Pa', 'exthorrad_Whm2', 'extdirrad_Whm2', 'horirsky_Whm2', 'glohorrad_Whm2',
                  'dirnorrad_Whm2', 'difhorrad_Whm2', 'glohorillum_lux', 'dirnorillum_lux', 'difhorillum_lux',
                  'zenlum_lux', 'winddir_deg', 'windspd_ms', 'totskycvr_tenths', 'opaqskycvr_tenths', 'visibility_km',
                  'ceiling_hgt_m', 'presweathobs', 'presweathcodes', 'precip_wtr_mm', 'aerosol_opt_thousandths',
                  'snowdepth_cm', 'days_last_snow', 'Albedo', 'liq_precip_depth_mm', 'liq_precip_rate_Hour']

# dictionary of OntoEMS concepts as keys and EPW labels as values
ontoems_concepts = {"AirTemperature": 'drybulb_C',
             "RelativeHumidity": 'relhum_percent',
             "DewPoint": 'dewpoint_C',
             "AtmosphericPressure": 'atmos_Pa',
             "Rainfall": 'liq_precip_depth_mm',
             "Snowfall": 'snowdepth_cm',
             "CloudCover": 'totskycvr_tenths',
             "DirectNormalIrradiance": 'dirnorrad_Whm2',
             "DiffuseHorizontalIrradiance": 'difhorrad_Whm2',
             "WindDirection": 'windspd_ms',
             "WindSpeed" : 'winddir_deg'}

# EPW allowed range
mi_ma = {'drybulb_C': [-70, 70],
         'dewpoint_C': [-70, 70],
         'relhum_percent': [0, 110],
         'atmos_Pa': [31000, 120000],
         'exthorrad_Whm2': [0, ""],
         'extdirrad_Whm2': [0, ""],
         'horirsky_Whm2': [0, ""],
         'glohorrad_Whm2': [0, ""],
         'dirnorrad_Whm2': [0, ""],
         'difhorrad_Whm2': [0, ""],
         'glohorillum_lux': [0, ""],
         'dirnorillum_lux': [0, ""],
         'difhorillum_lux': [0, ""],
         'zenlum_lux': [0, ""],
         'winddir_deg': [0, 360],
         'windspd_ms': [0, 40],
         'totskycvr_tenths': [0, 10],
         'opaqskycvr_tenths': [0, 10],
         'visibility_km': ["", ""],
         'ceiling_hgt_m': ["", ""],
         'presweathobs': ["", ""],
         'presweathcodes': ["", ""],
         'precip_wtr_mm': ["", ""],
         'aerosol_opt_thousandths': ["", ""],
         'snowdepth_cm': ["", ""],
         'days_last_snow': ["", ""],
         'Albedo': ["", ""],
         'liq_precip_depth_mm': ["", ""],
         'liq_precip_rate_Hour': ["", ""]
         }

# missing values for EPW parameters
epw_missing = {'year': 'None', 
               'month': 'None',
               'day': 'None',
               'hour': 'None',
               'minute': 'None',
               'datasource': 'None',
               'drybulb_C': 99.9,
               'dewpoint_C': 99.9,
               'relhum_percent': 999.0,
               'atmos_Pa': 999999.0,
               'exthorrad_Whm2': 9999.0,
               'extdirrad_Whm2':  9999.0,
               'horirsky_Whm2': 9999.0,
               'glohorrad_Whm2': 9999.0,
               'dirnorrad_Whm2': 9999.0,
               'difhorrad_Whm2': 9999.0,
               'glohorillum_lux': 999999.0,
               'dirnorillum_lux': 999999.0,
               'difhorillum_lux': 999999.0,
               'zenlum_lux': 9999.0,
               'winddir_deg': 999.0,
               'windspd_ms': 999.0,
               'totskycvr_tenths': 99.0,
               'opaqskycvr_tenths': 99.0,
               'visibility_km': 9999.0,
               'ceiling_hgt_m': 99999.0,
               'presweathobs': 9,
               'presweathcodes': 999999999,
               'precip_wtr_mm': 999.0,
               'aerosol_opt_thousandths': 0.999,
               'snowdepth_cm': 999,
               'days_last_snow': 99,
               'Albedo': 99,
               'liq_precip_depth_mm': 999,
               'liq_precip_rate_Hour': 99}

def create_epw(times, data, weather_file, latitude, longitude, elevation, offset, default_epw):
    """
    Creates an EPW file based on the retrieved historical weather data 

    Parameters
    ----------
    times : time stamps of the weather data as a list
    data : weather data as a dictionary with the OntoEMS concepts names as the keys
    
    Returns
    -------
    None.

    """
    
    df = pd.DataFrame()
    
    # parse weather data
    retrieved_weather = parse_weather(times, data, offset)
    
    # get default EPW used by CEA
    default_epw, default_others = read_epw(default_epw)
    
    # fill dataframe with weather values
    for k, v in epw_missing.items():
        if k in retrieved_weather:
            df[k] = retrieved_weather[k]

        else:
            df[k] = default_epw[k]

        # ensure that the weather values are within the allowed range for EPW parameters
        if k in mi_ma:
            df = fix(df, k)

        # replace any NaN values with the missing values defined by EPW
        df[k].fillna(value = v, inplace = True)
    
    # concat to get the whole EPW including other non-weather, miscellaneous information
    epw = pd.concat([default_others, df], ignore_index = True)
    
    epw.loc[0, 'drybulb_C'] = latitude
    epw.loc[0, 'dewpoint_C'] = longitude
    epw.loc[0, 'relhum_percent'] = offset
    epw.loc[0, 'atmos_Pa'] = elevation
    
    epw.to_csv(weather_file, header = False, index = False)
       
def fix(df, p):
    """
    Ensures that the weather values are within the allowed range of EPW by replacing values
    over the maxiumum or under the minimum by the allowed maximum or the allowed minimum 
    of the corresponding EPW parameter

    Parameters
    ----------
    df : pandas dataframe with the weather values

    Returns
    -------
    df : pandas dataframe with the weather values within the allowed range of EPW

    """

    mi = mi_ma[p][0]
    ma = mi_ma[p][1]

    if mi != "":
        df[p] = df[p].apply(lambda x : mi if x < mi else x)

    if ma != "":
        df[p] = df[p].apply(lambda x : ma if x > ma else x)
            
    return df
            
def parse_weather(times, data, offset):
    """
    Parses the retrieved weather data 

    Parameters
    ----------
    times : time stamps of the weather data as a list
    data : weather data as a dictionary with the OntoEMS concepts names as the keys
    offset : timezone offset

    Returns
    -------
    results : dictionary of the weather data including timestamps

    """
    
    results = {}

    year = []
    month = []
    day = []
    hour = []
    minute = []

    results['year'] = [t['year'] for t in times]
    results['month'] = [t['month'] for t in times]
    results['day'] = [t['day'] for t in times]
    results['hour'] = [t['hour'] for t in times]
    results['minute'] = [t['minute'] for t in times]
    
    for key, val in data.items():
        if key in ontoems_concepts:
            results[ontoems_concepts[key]] = val
    
    return results

def read_epw(file_path):
    """
    Reads an EPW file as two pands dataframe, one with the weather information, the other with the non-wetaher information

    Parameters
    ----------
    file_path : path to the EPW file

    Returns
    -------
    df : EPW file with the first 8 rows skipped as a pandas dataframe
    df1 : EPW file with only the first 8 rows as a pandas dataframe

    """
    df = pd.read_csv(file_path, skiprows = 8, index_col = False, header = None,  names = list(epw_missing.keys()))
    df1 = pd.read_csv(file_path, nrows = 8, index_col = False, header = None,  names = list(epw_missing.keys()))

    return df, df1

def main(argv):
    weather_file = argv.file_location + os.sep + argv.file_name
    weather_times_file = argv.weather_times
    weather_data_file = argv.weather_data

    with open(weather_times_file, "r") as f:
        dataString = f.readlines()[0]
    weather_times = json.loads(dataString)
    
    with open(weather_data_file, "r") as f:
        dataString = f.readlines()[0]
    weather_data = json.loads(dataString)

    try:
        create_epw(weather_times, weather_data, weather_file, argv.latitude, argv.longitude, argv.elevation, argv.offset, argv.default_epw)
    except IOError:
        print('Error while processing file: ' + argv.file_name)

if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    parser.add_argument("weather_times")
    parser.add_argument("weather_data")
    parser.add_argument("latitude")
    parser.add_argument("longitude")
    parser.add_argument("elevation")
    parser.add_argument("offset")
    parser.add_argument("file_location")
    parser.add_argument("file_name")
    parser.add_argument("default_epw")
    
    args = parser.parse_args()
    main(args)