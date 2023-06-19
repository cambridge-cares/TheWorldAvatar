"""
Reads in SWPS provided time series raw data (as .txt and .xlsx) and creates overarching (date-sorted) csv file

@author: Markus Hofmeister
"""

import os
import re
import pandas as pd
import datetime as dt
from pathlib import Path


####################     FUNCTIONS     ####################


def read_textfiles(directory):
    """
    Reads all time series .txt files in 'directory' and returns a sorted DataFrame
    Removes all entries (entire rows) with invalid date format or non-numerical entries
    """

    # get list of all .txt files in directory
    files = []
    for entry in os.scandir(directory):
        if entry.name.endswith('.txt'):
            files.append(os.path.join(directory, entry))

    # loop over all files
    first = True
    for file in files:
        # read .txt file (engine='python' to suppress warning, as default engine not capable of double white
        # space delimiter '\s\s+')
        data = pd.read_csv(file, sep='\s\s+', header=None, skiprows=2, engine='python')
        # exclude rows with invalid date format by boolean indexing (1 where date format matches expected format)
        # regular expression for date format: '^[0-9]{2}\.[0-9]{2}\.[0-9]{4} [0-9]{2}\:[0-9]{2}'
        data = data[
            data[0].map(lambda x: bool(re.match(r'^[0-9]{2}\.[0-9]{2}\.[0-9]{4} [0-9]{2}\:[0-9]{2}$', x)) == True)]
        # Create DateTime index
        data['Date'] = pd.to_datetime(data[0], format='%d.%m.%Y %H:%M')  # convert to DateTime from German format
        data.set_index('Date', inplace=True)  # set DateTime as index
        data.drop(columns=[0], inplace=True)  # drop prior date column
        # round potential minor inconsistencies in DateTime index to nearest hour
        data.index = data.index.round('1h')
        # loop over all data series
        for col in data.columns:
            # remove invalid non-numeric data
            data = data[data[col].map(lambda x: isinstance(x, (int, float)))]
        # Create overall dataframe with cleaned data
        if first == True:
            all_data = data.copy()  # create real copy and NO view
            first = False
        else:
            all_data = all_data.append(data)

    # sort overall data by ascending DateTimeIndex
    all_data.sort_index(inplace=True)
    # remove potential duplicates by appending data
    all_data = all_data[~all_data.index.duplicated(keep='last')]

    return all_data


def read_SWPS_excel(excel_filename, sheet):
    """
    Reads time series on sheet 'sheet' in 'excel_filename' and returns a sorted DataFrame
    Removes all non-numeric entries from time series
    """

    # read time series (15min interval data stretches over columns C:CV; skip top 4 header rows; set index to date in column C
    # engine xlrd required to read column headers consistently as datetime.time
    # (openpyxl reads '00:00' as datetime.datetime and rest as datetime.time)
    data = pd.read_excel(excel_filename, engine='xlrd', sheet_name=sheet, usecols='C:CU', skiprows=4, index_col=0)
    # drop all columns which do not denote times (hourly/15min interval headers were read in as datetime.time objects)
    drop_cols = [c for c in data.columns if not isinstance(c, dt.time)]
    data.drop(columns=drop_cols, inplace=True)
    # lag midnight data (00:00) by 1 day to ensure temporal consistency
    data[dt.time(0, 0)] = data[dt.time(0, 0)].shift(1)
    # create stacked index with hourly/15min interval column headers (called 'level_1') and give proper column name
    data = data.stack().reset_index(name=sheet)
    # create column 'Date' from stacked indices with correct Date and Time
    data['Date'] = data.apply(lambda row: dt.datetime.combine(row['Datum'], row['level_1']), 1)
    # set as DateTime index
    data.set_index(data['Date'], inplace=True)
    # drop unnecessary columns and
    data.drop(columns=['Datum', 'level_1', 'Date'], inplace=True)
    # remove potential non-numeric values
    data = data[data[sheet].map(lambda x: isinstance(x, (int, float)))]
    # sort data by ascending DateTimeIndex
    data.sort_index(inplace=True)
    # remove potential duplicates
    data = data[~data.index.duplicated(keep='last')]

    return data


####################     BODY     ####################

# set root directory (extract directory of current module) and relative path to consolidated raw data file
rootdir = Path(__file__).parent
file = '..\\data\\input\\processed\\data_consolidated_raw.csv'

# only read individual raw data if it has not already been consolidated
if os.path.exists(os.path.join(rootdir, file)):
    pass

else:
    #####   consolidate .txt files   #####

    # set path to raw .txt file repository
    txtdir = os.path.join(rootdir, '..\\data\\input\\raw\\txts\\')
    # define dictionary with column names for time series variables provided as .txt files
    headers = {'Aussentemperatur_HKW': ['Aussentemperatur (degC)'],
               'Waermeerzeugung_GT': ['GT Wirkleistung (kW)', 'GT Waermeleistung (MW)'],
               'Waermeerzeugung_Kessel': ['Waermeleistung Kessel4 (MW)', 'Waermeleistung Kessel5 (MW)',
                                          'Waermeleistung Kessel6 (MW)'],
               'FW_Lastgang': ['Waermemenge Innenstadt (MW)', 'Waermemenge MHKW (MW)'],
               'Waermespeicher': ['WSP Temp1 (degC)', 'WSP Temp2 (degC)', 'WSP Temp3 (degC)', 'WSP Temp4 (degC)',
                                  'WSP Temp5 (degC)', 'WSP Temp6 (degC)', 'WSP Temp7 (degC)', 'WSP Temp8 (degC)',
                                  'WSP Temp9 (degC)', 'WSP Temp10 (degC)', 'WSP Temp11 (degC)', 'WSP Temp12 (degC)'],
               'Netztemperaturen': ['Temp Vorlauf (degC)', 'Temp Ruecklauf (degC)'],
               'MHKW_Einspeisung': ['MHKW Temp Vorlauf (degC)', 'MHKW Temp Ruecklauf (degC)', 'MHKW Durchfluss (m3/h)',
                                    'MHKW Druck Vorlauf (bar)', 'MHKW Waermemenge (MW)']}
    # loop over all directories in root directory
    first = True
    for entry in os.scandir(txtdir):
        if os.path.isdir(os.path.join(txtdir, entry)):
            var_name = entry.name
            inp_dir = os.path.join(txtdir, entry)
            # get sorted DataFrame containing all time series data from individual .txt files for current variable
            ts_raw = read_textfiles(inp_dir)
            # attach correct column header from dictionary
            ts_raw.columns = headers[var_name]
            # create overarching DataFrame of raw data
            if first == True:
                raw_txt = ts_raw.copy()
                first = False
            else:
                raw_txt = pd.concat([raw_txt, ts_raw], axis=1, sort='True')

    #####   consolidate .xls files   #####

    # set path to raw .xlsx file repository
    xlsdir = os.path.join(rootdir, '..\\data\\input\\raw\\xls\\')

    # 1) 'Daten 2013-2020 HKW_mh807'
    filename = 'Daten 2013-2020 HKW_mh807.xlsx'
    # obtain list of all sheets in file
    path = os.path.join(xlsdir, filename)
    xlsx = pd.ExcelFile(path)
    sheets = xlsx.sheet_names[:-1]  # remove last sheet with annually constant oil prices
    # loop over all sheets to read in time series data
    first = True
    for sheet in sheets:
        ts_raw = read_SWPS_excel(path, sheet)
        # resample to hourly resolution (some Excel data in 15min, some data in 1h intervals)
        ts_raw = ts_raw.resample('1h').mean()
        if first == True:
            raw_xls = ts_raw.copy()
            first = False
        else:
            raw_xls = pd.concat([raw_xls, ts_raw], axis=1, sort='True')

    # 2) Electricity spot prices
    filename = 'Spotmarktpreise 2017-2021_mh807.xlsx'
    path = os.path.join(xlsdir, filename)
    ts_raw = pd.read_excel(path, sheet_name='consolidated', usecols='A:B', index_col=0, parse_dates=True)
    # round potential minor inconsistencies while parsing dates (only a few (micro)seconds) to nearest hour
    ts_raw.index = ts_raw.index.round('1h')
    # sort data by ascending DateTimeIndex
    ts_raw.sort_index(inplace=True)
    # remove potential duplicates
    ts_raw = ts_raw[~ts_raw.index.duplicated(keep='last')]
    # append to overall 'raw_xls' DataFrame
    raw_xls = pd.concat([raw_xls, ts_raw], axis=1, sort='True')

    # 3) CO2 emissions
    filename = 'Markt EUA_mh807.xlsx'
    path = os.path.join(xlsdir, filename)
    ts_raw = pd.read_excel(path, sheet_name='consolidated', usecols='A:B', index_col=0, parse_dates=True)
    # upsample CO2 price data (provided daily) to hourly data and forward fill all emerging NaNs
    ts_raw = ts_raw.resample('1h').mean()
    ts_raw.fillna(method='ffill', inplace=True)
    # sort data by ascending DateTimeIndex
    ts_raw.sort_index(inplace=True)
    # remove potential duplicates
    ts_raw = ts_raw[~ts_raw.index.duplicated(keep='last')]
    # append to overall 'raw_xls' DataFrame
    raw_xls = pd.concat([raw_xls, ts_raw], axis=1, sort='True')

    #####   create overarching DataFrame of ALL raw data   #####

    # align unit of GT electric power also to MW
    raw_txt['GT Wirkleistung (kW)'] = raw_txt['GT Wirkleistung (kW)']/1000.0
    raw_txt.rename(columns={'GT Wirkleistung (kW)': 'GT Wirkleistung (MW)'}, inplace=True)
    # rename two columns to ensure naming consistency
    raw_xls.rename(columns={'Gaspreis GT (EURMWh)': 'Gaspreis GT (EUR/MWh)',
                            'Gaspreis Kessel (EURMWh)': 'Gaspreis Kessel (EUR/MWh)'}, inplace=True)
    raw_data = pd.concat([raw_txt, raw_xls], axis=1, sort='True')

    # write overall DataFrames to csv
    raw_data.to_csv(os.path.join(rootdir, file), index_label='Date')
