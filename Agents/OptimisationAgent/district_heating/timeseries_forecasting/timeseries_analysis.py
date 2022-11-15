"""
Analyses time series data provided by SWPS in preparation of forecasting heat load and district heating flow
and return temperatures

Plots time series when called as main script

@author: Markus Hofmeister
"""

import os
import math
import pickle
import random
import numpy as np
import pandas as pd
from scipy import stats
from pathlib import Path

import seaborn as sn
import matplotlib.pyplot as plt
from sklearn.pipeline import make_pipeline
from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import PolynomialFeatures

import hampel


####################     FUNCTIONS     ####################

def read_timeseries(file):
    """
    Reads all relevant time series for heat load, flow and return temperature analysis and forecasting from pre-cleaned
    consolidated csv file

    :param str file: Path to pre-cleaned consolidated time series data
    :returns pd.DataFrame data: DataFrame containing pre-cleaned time series data
    """

    # read relevant time series data from pre-cleaned consolidated csv files with DateTime index
    data = pd.read_csv(file, index_col='Date', parse_dates=True, dayfirst=True, usecols=
                       ['Date',
                        # HKW data
                        'Aussentemperatur (degC)', 'Waermemenge Innenstadt (MW)', 'Waermemenge MHKW (MW)',
                        'Temp Vorlauf (degC)', 'Temp Ruecklauf (degC)',
                        # MHKW data
                        'MHKW Temp Vorlauf (degC)', 'MHKW Temp Ruecklauf (degC)', 'MHKW Durchfluss (m3/h)',
                        'MHKW Druck Vorlauf (bar)', 'MHKW Waermemenge (MW)'])

    data['Waermeeinspeisung (MW)'] = data['Waermemenge Innenstadt (MW)'] + data['Waermemenge MHKW (MW)']
    data.drop(columns=['Waermemenge Innenstadt (MW)', 'Waermemenge MHKW (MW)'], inplace=True)

    return data


def condition_hkw_timeseries(data):
    """
    Returns fully conditioned time series data for analyses and forecasting purposes
        1) removes further specific outliers in (already pre-cleaned) data
        2) ensures equal spacing in 1h intervals

    :param pd.DataFrame data: DataFrame with time series data per variable
    :returns pd.DataFrame: DataFrame with fully conditioned time series data per variable
    """

    # initialize DataFrame of fully-conditioned data
    data = data[['Aussentemperatur (degC)', 'Waermeeinspeisung (MW)', 'Temp Vorlauf (degC)',
                       'Temp Ruecklauf (degC)']].copy()
    data.dropna(inplace=True)
    conditioned = data.copy()

    ##########   CLEAN DATA   ##########

    ###   clean "Vorlauftemperatur"   ###
    # remove exceptionally low outliers (potentially due to revisions etc.)
    lower_bound = conditioned['Temp Vorlauf (degC)'].quantile(.005)
    conditioned['Temp Vorlauf (degC)'] = np.where(conditioned['Temp Vorlauf (degC)'] < lower_bound, np.nan,
                                                  conditioned['Temp Vorlauf (degC)'])
    conditioned['Temp Vorlauf (degC)'].fillna(method='ffill', inplace=True)
    # apply Hampel filter to replace outliers with the rolling median (median absolute deviation filter)
    conditioned['Temp Vorlauf (degC)'] = hampel.hampel(conditioned['Temp Vorlauf (degC)'], window_size=5, n=4,
                                                       imputation=True)

    ###   cle lan "Ruecklauftemperatur"   ###
    #     # remove exceptionallyow outliers (potentially due to revisions etc.)
    lower_bound = conditioned['Temp Ruecklauf (degC)'].quantile(.005)
    conditioned['Temp Ruecklauf (degC)']= np.where(conditioned['Temp Ruecklauf (degC)'] < lower_bound, np.nan,
                                                   conditioned['Temp Ruecklauf (degC)'])
    conditioned['Temp Ruecklauf (degC)'].fillna(method='ffill', inplace=True)
    # apply Hampel filter to replace outliers with the rolling median (median absolute deviation filter)
    conditioned['Temp Ruecklauf (degC)'] = hampel.hampel(conditioned['Temp Ruecklauf (degC)'], window_size=5, n=4,
                                                         imputation=True)

    ###   clean "Waermeeinspeisung"   ###
    lower_bound = conditioned['Waermeeinspeisung (MW)'].quantile(.003)
    conditioned['Waermeeinspeisung (MW)'] = np.where(conditioned['Waermeeinspeisung (MW)'] < lower_bound, np.nan,
                                                     conditioned['Waermeeinspeisung (MW)'])
    conditioned['Waermeeinspeisung (MW)'].fillna(method='ffill', inplace=True)

    # 2013
    conditioned.loc['2013-05-16', 'Waermeeinspeisung (MW)'] = conditioned.loc['2013-05-18',
                                                                              'Waermeeinspeisung (MW)'].values
    # 2014
    conditioned.loc['2014-03-27', 'Waermeeinspeisung (MW)'] = conditioned.loc['2014-03-26',
                                                                              'Waermeeinspeisung (MW)'].values
    conditioned.loc['2014-05-21 12:00:00', 'Waermeeinspeisung (MW)'] = conditioned.loc['2014-05-21 11:00:00',
                                                                                       'Waermeeinspeisung (MW)']
    # 2015
    conditioned.loc['2015-09-22 15:00':'2015-10-12 05:00', 'Waermeeinspeisung (MW)'] -= 11
    # 2016
    conditioned.loc['2016-07-23':'2016-07-29', 'Waermeeinspeisung (MW)'] = conditioned.loc[
                    '2016-08-15':'2016-08-21', 'Waermeeinspeisung (MW)'].values
    # 2017
    conditioned.loc['2017-11-08 07:00:00', 'Waermeeinspeisung (MW)'] = conditioned.loc[
                    '2017-11-08 06:00:00', 'Waermeeinspeisung (MW)']
    conditioned.loc['2017-11-09 11:00:00', 'Waermeeinspeisung (MW)'] = conditioned.loc[
                    '2017-11-09 10:00:00', 'Waermeeinspeisung (MW)']
    conditioned.loc['2017-12-05 01:00:00', 'Waermeeinspeisung (MW)'] = conditioned.loc[
                    '2017-12-05 00:00:00', 'Waermeeinspeisung (MW)']
    conditioned.loc['2017-12-18 09:00:00', 'Waermeeinspeisung (MW)'] = conditioned.loc[
                    '2017-12-18 08:00:00', 'Waermeeinspeisung (MW)']
    # 2018
    conditioned.loc['2018-01-04 09:00:00', 'Waermeeinspeisung (MW)'] = conditioned.loc[
                    '2018-01-04 08:00:00', 'Waermeeinspeisung (MW)']
    conditioned.loc['2018-01-10 09:00:00', 'Waermeeinspeisung (MW)'] = conditioned.loc[
                    '2018-01-10 08:00:00', 'Waermeeinspeisung (MW)']
    conditioned.loc['2018-01-19 09:00:00', 'Waermeeinspeisung (MW)'] = conditioned.loc[
                    '2018-01-19 08:00:00', 'Waermeeinspeisung (MW)']
    conditioned.loc['2018-02-13 11:00:00', 'Waermeeinspeisung (MW)'] = conditioned.loc[
                    '2018-02-13 10:00:00', 'Waermeeinspeisung (MW)']
    conditioned.loc['2018-02-13 12:00:00', 'Waermeeinspeisung (MW)'] = conditioned.loc[
                    '2018-02-13 13:00:00', 'Waermeeinspeisung (MW)']
    conditioned.loc['2018-12-17 12:00:00', 'Waermeeinspeisung (MW)'] = conditioned.loc[
                    '2018-12-17 13:00:00', 'Waermeeinspeisung (MW)']
    # apply Hampel filter to replace outliers with the rolling median (median absolute deviation filter)
    conditioned['Waermeeinspeisung (MW)'] = hampel.hampel(conditioned['Waermeeinspeisung (MW)'], window_size=5, n=3,
                                                          imputation=True)

    print('\n#####   HKW data   #####\n')
    print('\n#####   Percentages of adjusted data points   #####')
    for var in ['Temp Vorlauf (degC)', 'Temp Ruecklauf (degC)', 'Waermeeinspeisung (MW)']:
        d = pd.concat([data[var], conditioned[var]], axis=1)
        d.columns = ['orig', 'adj']
        d['diff'] = np.where(d['orig'] == d['adj'], 0 , 1)
        print(var + ': %.1f %%' % (d['diff'].sum()/len(d)*100))

    ##########   ENSURE CONSISTENT HOURLY INTERVAL SPACING   ##########

    conditioned = conditioned.resample('1H').mean()
    nans = conditioned[conditioned.isna().any(axis=1)]
    conditioned.fillna(method='ffill', inplace=True)

    print('\n#####   Percentages of up-sampled data points   #####')
    for var in ['Temp Vorlauf (degC)', 'Temp Ruecklauf (degC)', 'Waermeeinspeisung (MW)']:
        print(var + ': %.1f %%' % (nans[var].isna().sum()/len(conditioned)*100))
    print('\nRemaining NaNs in time series data: %i' % conditioned.isna().sum().sum())

    return conditioned


def condition_mhkw_timeseries(data):
    """
    Returns fully conditioned time series data for analyses and forecasting purposes
    (grid temperatures are FULLY conditioned and partially re-engineered; flow rate, pressure and heat load are NOT)
        1) removes further specific outliers in (already pre-cleaned) data
        2) ensures equal spacing in 1h intervals

    :param pd.DataFrame data: DataFrame with time series data per variable
    :returns pd.DataFrame: DataFrame with fully conditioned time series data per variable
    """

    # initialize DataFrame of fully-conditioned data
    data = data[['MHKW Temp Vorlauf (degC)', 'MHKW Temp Ruecklauf (degC)', 'MHKW Durchfluss (m3/h)',
                        'MHKW Druck Vorlauf (bar)', 'MHKW Waermemenge (MW)']]
    # keep only relevant 2018 - 2020 data
    data = data['2018':'2020'].copy()

    ##########   CLEAN & CONDITION DATA (primarily grid temperatures)   ##########

    # 1) replace all daylight saving time adjustment induced NaNs with previous value
    days = ['2018-03-25 02:00:00', '2018-10-28 02:00:00', '2019-03-31 02:00:00', '2019-10-27 02:00:00',
            '2020-03-29 02:00:00', '2020-10-25 02:00:00']
    for d in days:
        ind = data.index.get_loc(d)
        data.iloc[ind, :] = data.iloc[ind - 1, :]

    # 2) remove non-representative values with MHKW flow rate < 1m3/h
    q_min = 1.0  # flow rate < 1 m3/h indicates MHKW internal circulation (not-representative data)
    conditioned = data.copy()
    conditioned[conditioned['MHKW Durchfluss (m3/h)'] < q_min] = np.nan

    # 3) further remove non-representative values with too low MHKW pressure
    cutoff = conditioned['MHKW Druck Vorlauf (bar)'].quantile(0.005)
    ind = conditioned[conditioned['MHKW Druck Vorlauf (bar)'] < cutoff].index
    for i in ind:
        ix = conditioned.index.get_loc(i)
        conditioned.iloc[ix, :] = conditioned.iloc[ix - 1, :]

    # 4) apply hampel filter to smooth data and remove further outliers (apply detailed data conditioning only to
    # most relevant flow and return temperature time series)
    variables = ['MHKW Temp Vorlauf (degC)', 'MHKW Temp Ruecklauf (degC)']
    for col in variables:
        conditioned[col] = hampel.hampel(conditioned[col], window_size=10, n=3, imputation=True)

    # 5) fill non-summer periods of missing data
    # 2018
    conditioned.loc['2018-04-17 10:00:00': '2018-04-23 06:00:00'] = \
        conditioned.loc['2018-04-11 10:00:00': '2018-04-17 06:00:00'].values
    # 2019
    conditioned.loc['2019-09-23': '2019-09-24'] = \
        conditioned.loc['2019-09-27': '2019-09-28'].values
    conditioned.loc['2019-10-30 16:00:00': '2019-11-07 07:00:00'] = \
        conditioned.loc['2018-10-30 16:00:00': '2018-11-07 07:00:00'].values
    conditioned.loc['2019-11-07 08:00:00': '2019-11-13 07:00:00'] = \
        conditioned.loc['2020-11-07 08:00:00': '2020-11-13 07:00:00'].values
    # 2020
    conditioned.loc['2020-09-17 11:00:00': '2020-10-14 12:00:00'] = \
        conditioned.loc['2019-09-17 11:00:00': '2019-10-14 12:00:00'].values
    conditioned.loc['2020-10-07 11:00:00': '2020-10-14 12:00:00'] = \
        conditioned.loc['2020-11-07 11:00:00': '2020-11-14 12:00:00'].values

    # 6) replace variable specific outliers
    conditioned.loc['2019-12-17 14:00:00', 'MHKW Temp Vorlauf (degC)'] = \
        conditioned.loc['2019-12-17 13:00:00', 'MHKW Temp Vorlauf (degC)']
    conditioned.loc['2019-11-13', 'MHKW Temp Ruecklauf (degC)'] = \
        conditioned.loc['2019-11-12', 'MHKW Temp Ruecklauf (degC)'].values
    conditioned.loc['2019-11-13', 'MHKW Druck Vorlauf (bar)'] = \
        conditioned.loc['2019-11-12', 'MHKW Druck Vorlauf (bar)'].values

    # 7) fill longer summer periods of missing data   ###
    # 7.1) extract representative summer behaviour from 2019 data
    summer = conditioned.loc['2019-06-25 07:00:00': '2019-07-28'].copy()
    summer.loc['2019-07-23 10:00:00': '2019-07-25 07:00:00', :] = \
        summer.loc['2019-07-21 10:00:00': '2019-07-23 07:00:00', :].values
    summer.loc['2019-07-09', :] = summer.loc['2019-07-12', :].values
    summer.loc['2019-07-10', :] = summer.loc['2019-07-13', :].values
    summer.loc['2019-07-15', :] = summer.loc['2019-07-17', :].values
    summer.loc['2019-07-16', :] = summer.loc['2019-07-17', :].values
    summer.loc['2019-07-25', :] = summer.loc['2019-07-20', :].values
    summer.loc['2019-07-26', :] = summer.loc['2019-07-21', :].values

    # 7.2) use short period of available summer data from 2019 in random chunks of 'period' length to fill missing period
    period = 14 * 24  # length of continuous 2019 summer data in one individual chunk
    transition_length = 21 * 24  # length of transition period at beginning and end of summer period
    median_period = 14 * 24  # lenghtnof period to evaluate before and after summer period for median/std calculation

    # years to evaluate with extracted start and end dates for missing summer data
    years = ['2018', '2019', '2020']
    gap = {'2018': ['2018-05-27 13:00:00', '2018-10-01 03:00:00'],
           '2019': ['2019-06-01 12:00:00', '2019-09-17 10:00:00'],
           '2020': ['2020-06-18 14:00:00', '2020-09-17 10:00:00']}

    # fill summer periods with missing data
    for y in years:

        # ensure consistent random selection for both flow and return temperature
        start_idx = None

        for col in variables:

            # extract data for col as pd. Series
            series = conditioned[col].copy()
            # extract missing data
            missing = series.loc[gap[y][0]: gap[y][1]].copy()

            # randomly up-sample available 2019 summer data to fill current summer gap
            n = math.floor(len(missing) / period)
            if not start_idx:
                # set seed to ensure consistency between individual runs
                random.seed(0)
                start_idx = random.choices(range(len(summer) - period), k=n)
            for i in range(len(start_idx)):
                missing.iloc[i * period: (i + 1) * period] = summer[col].iloc[start_idx[i]: start_idx[i] + period]
            missing.iloc[n * period:] = summer[col].iloc[start_idx[-1]: start_idx[-1] + len(missing) - n * period]

            # extract median and std for summer data
            med = missing.median()
            std = missing.std()

            # shift level of replaced summer values to minimum of left and right median
            start = series.index.get_loc(gap[y][0])
            end = series.index.get_loc(gap[y][1])
            med_start = series.iloc[start - median_period: start].median()
            med_end = series.iloc[end + 1:  end + median_period].median()
            med_min = min(med_start, med, med_end)
            shift = med_min - med
            missing += shift

            # increase 'noise' towards ends
            std_start = series.iloc[start - median_period: start].std()
            std_end = series.iloc[end + 1:  end + median_period].std()
            trend_start = (std_start - std) / transition_length
            missing.iloc[:transition_length] += (missing.iloc[:transition_length] - med_min) * trend_start * \
                                                np.arange(start=transition_length, stop=0, step=-1)
            trend_end = (std_end - std) / transition_length
            missing.iloc[-transition_length:] += (missing.iloc[-transition_length:] - med_min) * trend_end * \
                                                 np.arange(start=0, stop=transition_length, step=1)

            # include trend / slope from summer low to medians on left and right side
            trend_start = (med_start - med_min) / transition_length
            missing.iloc[:transition_length] += trend_start * np.arange(start=transition_length, stop=0, step=-1)
            trend_end = (med_end - med_min) / transition_length
            missing.iloc[-transition_length:] += trend_end * np.arange(start=0, stop=transition_length, step=1)

            # re-assign engineered data to original pd.DataFrame
            conditioned.loc[gap[y][0]: gap[y][1], col] = missing.values

    # 8) re-set pressure, flow rate, and heat amount
    reset = [i for i in conditioned.columns if i not in variables]
    for col in reset:
        conditioned[col] = data[col]

    # 9) fill remaining shorter periods of missing data
    for col in conditioned.columns:
        # derive start and number of consecutive NaNs
        mask = conditioned[col].isna()
        nans = conditioned[col].index.to_series()[mask].groupby((~mask).cumsum()[mask]).agg(['first', 'size'])
        nans.rename(columns=dict(size='Count_of_NaNs', first='Start_Date')).reset_index(drop=True)
        # extract data for col as pd. Series
        series = conditioned[col].copy()
        for i, row in nans.iterrows():
            idx = series.index.get_loc(row['first'])
            # replace missing values with preceding values
            series.iloc[idx: idx + row['size']] = series.iloc[idx - row['size']: idx]
        # re-assign
        conditioned[col] = series

    # 10) apply final hampel filter to smooth data and remove further outliers
    for col in conditioned.columns:
        conditioned[col] = hampel.hampel(conditioned[col], window_size=10, n=3, imputation=True)

    print('\n#####   MHKW data   #####\n')
    print('\n#####   Percentages of adjusted data points   #####')
    for var in ['MHKW Temp Vorlauf (degC)',	'MHKW Temp Ruecklauf (degC)', 'MHKW Durchfluss (m3/h)',
                'MHKW Druck Vorlauf (bar)']:
        d = pd.concat([data[var], conditioned[var]], axis=1)
        d.columns = ['orig', 'adj']
        d['diff'] = np.where(d['orig'] == d['adj'], 0 , 1)
        print(var + ': %.1f %%' % (d['diff'].sum()/len(d)*100))

    ##########   ENSURE CONSISTENT HOURLY INTERVAL SPACING   ##########

    conditioned = conditioned.resample('1H').mean()
    nans = conditioned[conditioned.isna().any(axis=1)]
    conditioned.fillna(method='ffill', inplace=True)

    print('\n#####   Percentages of up-sampled data points   #####')
    for var in ['MHKW Temp Vorlauf (degC)',	'MHKW Temp Ruecklauf (degC)', 'MHKW Durchfluss (m3/h)',
                'MHKW Druck Vorlauf (bar)']:
        print(var + ': %.1f %%' % (nans[var].isna().sum()/len(conditioned)*100))
    print('\nRemaining NaNs in time series data: %i' % conditioned.isna().sum().sum())

    return conditioned


def hourly_averages(timeseries):
    """
    Returns average values for each hour per day for each day of the year

    :params pd.DataFrame timeseries: DataFrame with DateTimeIndex
    :returns pd.DataFrame: DataFrame with multiindex and average values per h
    """

    data = timeseries.copy()

    data['Hour'] = data.index.to_series()
    data['Hour'] = data['Hour'].dt.hour
    data['DayOfYear'] = data.index.to_series()
    data['DayOfYear'] = data['DayOfYear'].dt.dayofyear

    # Calculate average per h for each day of year and create multiindex [DayOfYear, Hour]
    data = data.groupby(['DayOfYear', 'Hour']).mean()

    return data


def plot_timeseries(timeseries, label):
    """
    Plots each time series with appropriate plotting parameters as
        1) entire time series history
        2) overlayed time series for each hour per day of the year

    :param pd.DataFrame timeseries: DataFrame with time series data per variable
    :param str label: label to describe the nature of the time series data ['pre-cleaned', 'fully-conditioned']
    """

    # derive years present in DataFrame
    years = timeseries.index.year.unique()

    # derive hourly averaged values for entire data history
    averages = hourly_averages(timeseries)
    # remove 366th day from average data (as only present in leap years and average likely to be distorted)
    averages = averages.loc[:365, :]
    # reindex hourly average data to (random) non-leap year DateTime index
    averages.index = pd.date_range(start='01-01-2019', end='31-12-2019 23:00', freq='1H')
    ave_reindexed = averages.reset_index(drop=True)

    for var in timeseries.columns:
        # set global plotting parameters
        plt.rcParams['font.size'] = 14

        if var == 'Aussentemperatur (degC)':

            # plot entire data series
            fig, ax = plt.subplots(figsize=(16, 9))
            plt.grid(axis='both')
            ax.plot(timeseries[var], color='gray', linestyle='--', linewidth=0.5)
            ax.set_xlabel('Datum')
            ax.set_ylabel('Aussentemperatur (°C)')
            ax.set_title(var + ' ' + label)
            plt.show()
            plt.close()

            # plot annual slices
            fig, ax = plt.subplots(figsize=(16, 9))
            plt.grid(axis='both')
            legend = []
            for year in years:
                legend.append(year)
                # condition data
                plot_data = timeseries.loc[str(year), var].copy()
                plot_data.resample('1H').ffill()
                plot_data.reset_index(drop=True, inplace=True)
                plot_data.dropna(inplace=True)
                ax.plot(plot_data, linestyle='--', linewidth=0.5)
            plt.plot(ave_reindexed[var], linestyle='-', color='black', linewidth=1.0)
            legend.append('Mittelwert')
            plt.legend(legend)
            ax.set_xlabel('Stunde im Jahresverlauf')
            ax.set_ylabel('Aussentemperatur (°C)')
            ax.set_title(var + ' ' + label)
            plt.show()
            plt.close()

        if var == 'Temp Vorlauf (degC)':

            # plot entire data series
            fig, ax = plt.subplots(figsize=(16, 9))
            plt.grid(axis='both')
            ax.plot(timeseries[var], color='gray', linestyle='--', linewidth=0.5)
            ax.set_xlabel('Datum')
            ax.set_ylabel('HKW Vorlauftemperatur (°C)')
            ax.set_title(var + ' ' + label)
            plt.show()
            plt.close()

            # plot annual slices
            fig, ax = plt.subplots(figsize=(16, 9))
            plt.grid(axis='both')
            legend = []
            for year in years:
                legend.append(year)
                # condition data
                plot_data = timeseries.loc[str(year), var].copy()
                plot_data.resample('1H').ffill()
                plot_data.reset_index(drop=True, inplace=True)
                plot_data.dropna(inplace=True)
                ax.plot(plot_data, linestyle='--', linewidth=0.5)
            plt.plot(ave_reindexed[var], linestyle='-', color='black', linewidth=1.0)
            legend.append('Mittelwert')
            plt.legend(legend)
            ax.set_xlabel('Stunde im Jahresverlauf')
            ax.set_ylabel('HKW Vorlauftemperatur (°C)')
            ax.set_title(var + ' ' + label)
            plt.show()
            plt.close()

        if var == 'Temp Ruecklauf (degC)':

            # plot entire data series
            fig, ax = plt.subplots(figsize=(16, 9))
            plt.grid(axis='both')
            ax.plot(timeseries[var], color='gray', linestyle='--', linewidth=0.5)
            ax.set_xlabel('Datum')
            ax.set_ylabel('HKW Ruecklauftemperatur (°C)')
            ax.set_title(var + ' ' + label)
            plt.show()
            plt.close()

            # plot annual slices
            fig, ax = plt.subplots(figsize=(16, 9))
            plt.grid(axis='both')
            legend = []
            for year in years:
                legend.append(year)
                # condition data
                plot_data = timeseries.loc[str(year), var].copy()
                plot_data.resample('1H').ffill()
                plot_data.reset_index(drop=True, inplace=True)
                plot_data.dropna(inplace=True)
                ax.plot(plot_data, linestyle='--', linewidth=0.5)
            plt.plot(ave_reindexed[var], linestyle='-', color='black', linewidth=1.0)
            legend.append('Mittelwert')
            plt.legend(legend)
            ax.set_xlabel('Stunde im Jahresverlauf')
            ax.set_ylabel('HKW Ruecklauftemperatur (°C)')
            ax.set_title(var + ' ' + label)
            plt.show()
            plt.close()

        if var == 'Waermeeinspeisung (MW)':

            # plot entire data series
            fig, ax = plt.subplots(figsize=(16, 9))
            plt.grid(axis='both')
            ax.plot(timeseries[var], color='gray', linestyle='--', linewidth=0.5)
            ax.set_xlabel('Datum')
            ax.set_ylabel('Waermeeinspeisung (MW)')
            ax.set_title(var + ' ' + label)
            plt.show()
            plt.close()

            # plot annual slices
            fig, ax = plt.subplots(figsize=(16, 9))
            plt.grid(axis='both')
            legend = []
            for year in years:
                legend.append(year)
                # condition data
                plot_data = timeseries.loc[str(year), var].copy()
                plot_data.resample('1H').ffill()
                plot_data.reset_index(drop=True, inplace=True)
                plot_data.dropna(inplace=True)
                ax.plot(plot_data, linestyle='--', linewidth=0.5)
            plt.plot(ave_reindexed[var], linestyle='-', color='black', linewidth=1.0)
            legend.append('Mittelwert')
            plt.legend(legend)
            ax.set_xlabel('Stunde im Jahresverlauf')
            ax.set_ylabel('Waermeeinspeisung (MW)')
            ax.set_title(var + ' ' + label)
            plt.show()
            plt.close()

        if var == 'MHKW Temp Vorlauf (degC)':

            # plot entire data series
            fig, ax = plt.subplots(figsize=(16, 9))
            plt.grid(axis='both')
            ax.plot(timeseries[var], color='gray', linestyle='--', linewidth=0.5)
            ax.set_xlabel('Datum')
            ax.set_ylabel('MHKW Vorlauftemperatur (°C)')
            ax.set_title(var + ' ' + label)
            plt.show()
            plt.close()

            # plot annual slices
            fig, ax = plt.subplots(figsize=(16, 9))
            plt.grid(axis='both')
            legend = []
            for year in years:
                legend.append(year)
                # condition data
                plot_data = timeseries.loc[str(year), var].copy()
                plot_data.resample('1H').ffill()
                plot_data.reset_index(drop=True, inplace=True)
                plot_data.dropna(inplace=True)
                ax.plot(plot_data, linestyle='--', linewidth=0.5)
            plt.plot(ave_reindexed[var], linestyle='-', color='black', linewidth=1.0)
            legend.append('Mittelwert')
            plt.legend(legend)
            ax.set_xlabel('Stunde im Jahresverlauf')
            ax.set_ylabel('MHKW Vorlauftemperatur (°C)')
            ax.set_title(var + ' ' + label)
            plt.show()
            plt.close()

        if var == 'MHKW Temp Ruecklauf (degC)':

            # plot entire data series
            fig, ax = plt.subplots(figsize=(16, 9))
            plt.grid(axis='both')
            ax.plot(timeseries[var], color='gray', linestyle='--', linewidth=0.5)
            ax.set_xlabel('Datum')
            ax.set_ylabel('MHKW Ruecklauftemperatur (°C)')
            ax.set_title(var + ' ' + label)
            plt.show()
            plt.close()

            # plot annual slices
            fig, ax = plt.subplots(figsize=(16, 9))
            plt.grid(axis='both')
            legend = []
            for year in years:
                legend.append(year)
                # condition data
                plot_data = timeseries.loc[str(year), var].copy()
                plot_data.resample('1H').ffill()
                plot_data.reset_index(drop=True, inplace=True)
                plot_data.dropna(inplace=True)
                ax.plot(plot_data, linestyle='--', linewidth=0.5)
            plt.plot(ave_reindexed[var], linestyle='-', color='black', linewidth=1.0)
            legend.append('Mittelwert')
            plt.legend(legend)
            ax.set_xlabel('Stunde im Jahresverlauf')
            ax.set_ylabel('MHKW Ruecklauftemperatur (°C)')
            ax.set_title(var + ' ' + label)
            plt.show()
            plt.close()


def create_correlation_matrix(timeseries, lags):
    """
    Investigate correlation between heat demand, flow and return temperature, and ambient temperature at current and
    lagged timesteps and plot correlation heatmap and scatter plots

    :param pd.DataFrame timeseries: DataFrame containing historical heat demand, flow and return temperatures,
                                    and ambient temperature data
    :param int lags: maximum number of time steps to shift variables (for correlation analysis with lagged variables)
    """

    # create local copy of DataFrame
    data = timeseries.copy()

    # adjust/shorten column names (for later plotting)
    names = {'Aussentemperatur (degC)': 'Temp_t',
             'Temp Vorlauf (degC)': 'Tvor_t',
             'Temp Ruecklauf (degC)': 'Trueck_t',
             'Waermeeinspeisung (MW)': 'FW_t'}
    data.rename(columns=names, inplace=True)

    # create lagged versions of variables
    for lag in range(lags):
        cols = [n + '-' + str(lag+1) for n in names.values()]
        new = dict(zip(names.values(), cols))
        ts = data[names.values()].shift(lag+1)
        ts.rename(columns=new, inplace=True)
        data = pd.concat([data, ts], axis=1)
    data.dropna(inplace=True)
    # sort columns alphabetically
    data = data.reindex(sorted(data.columns), axis=1)

    # pd.plotting needs to happen before seaborn plotting - seaborn seems to change plotting parameters so that
    # pd.plotting.scatter_matrix throws valueerror when called to be shown with plt.show()
    # 1) plot scatter matrix for non-lagged variables
    plt.figure()
    plt.rcParams.update({'font.size': 20})
    pd.plotting.scatter_matrix(data[names.values()], alpha=0.2, figsize=(15, 15), diagonal='hist', hist_kwds={'bins': 50})
    plt.suptitle('Scatter plot matrix')
    plt.show()

    # 2) plot scatter matrix incl. lagged variables
    plt.figure()
    plt.rcParams.update({'font.size': 12})
    pd.plotting.scatter_matrix(data, alpha=0.2, figsize=(15, 15), diagonal='hist', hist_kwds={'bins': 50})
    plt.suptitle('Scatter plot matrix including lagged variables')
    plt.show()

    # 3) plot correlation matrix for non-lagged variables
    corrm = data[names.values()].corr()
    sn.set(font_scale=1.5)
    f = plt.figure(figsize=(16, 12))
    f = sn.heatmap(corrm, center=0, cmap='RdBu', linewidths=0.5, annot=True)
    f.set_xticklabels(f.get_xticklabels(), rotation=45)
    f.set_yticklabels(f.get_yticklabels(), rotation=0)
    f.set_title('Correlation matrix\n')
    plt.show()

    # 4) plot correlation matrix including lagged variables
    corrm = data.corr()
    sn.set(font_scale=1.5)
    f = plt.figure(figsize=(16, 12))
    f = sn.heatmap(corrm, center=0, cmap='RdBu', linewidths=0.5, annot=True)
    f.set_xticklabels(f.get_xticklabels(), rotation=45)
    f.set_yticklabels(f.get_yticklabels(), rotation=0)
    f.set_title('Correlation matrix\n')
    plt.show()

    return None


def heat_demand_temperature_regression(timeseries):
    """
    Create and save univariate regression model for (median) heat demand as function of (median) ambient temperature

    :param pd.DataFrame timeseries: DataFrame containing historical heat demand and temperature data
    """

    # summarize historical data for same temperatures
    temp = timeseries.groupby(by=['Aussentemperatur (degC)']).median()

    # re-format data for sklearn
    X = np.array(temp.index)
    X = X.reshape(-1, 1)
    y = np.array(temp['Waermeeinspeisung (MW)'])

    # fit polynomial regression model
    # degree 6 was found to be the best fit for degrees [1, ..., 15]
    degree = 6
    polyreg = make_pipeline(PolynomialFeatures(degree), LinearRegression())
    polyreg.fit(X, y)

    # save the fitted model by serializing with Pickle
    root = Path(__file__).parent
    model = '..\\..\\data\\models\\timeseries\\heat_demand_regression.sav'
    f = os.path.join(root, model)
    pickle.dump(polyreg, open(f, 'wb'))

    return None


def flow_temperature_regression(timeseries):
    """
    Create and save multivariate regression model for heating grid flow temperature as function of
    ambient temperature and heat demand

    :param pd.DataFrame timeseries: DataFrame containing historical flow temperature, heat demand and temperature data
    """

    # extract data for sklearn
    X = timeseries[['Waermeeinspeisung (MW)', 'Aussentemperatur (degC)']].values
    y = np.array(timeseries['Temp Vorlauf (degC)'])

    # fit multivariate linear regression
    reg = LinearRegression()
    reg.fit(X, y)

    # save the fitted model by serializing with Pickle
    root = Path(__file__).parent
    model = '..\\..\\data\\models\\timeseries\\flow_temperature_regression.sav'
    f = os.path.join(root, model)
    pickle.dump(reg, open(f, 'wb'))

    return None


def compare_grid_temperatures(file):
        """
        Reads and plots historical grid temperature data as time series and distributions

        :param str file: Path to pre-cleaned consolidated time series data
        """

        # read grid temperature time series data from fully conditioned csv files with DateTime index
        data = pd.read_csv(file, index_col='Date', parse_dates=True, dayfirst=True,
                           usecols=['Date', 'Temp Vorlauf (degC)', 'Temp Ruecklauf (degC)'])
        data.dropna(inplace=True)

        # assess temperature spread between flow and return temp
        data['Temp Differenz (degC)'] = data['Temp Vorlauf (degC)'] - data['Temp Ruecklauf (degC)']

        # plot grid temperatures and temperature spread as time series
        plt.rcParams['font.size'] = 14
        f, ax = plt.subplots(2, 1, sharex=True, figsize=(16, 9))
        ax[0].grid('both')
        ax[0].plot(data['Temp Vorlauf (degC)'], color='tab:red')
        ax[0].plot(data['Temp Ruecklauf (degC)'], color='tab:blue')
        ax[0].legend(['Flow temperature', 'Return temperature'])
        ax[0].set_ylabel('Temperature (°C)')
        ax[1].grid('both')
        ax[1].plot(data['Temp Differenz (degC)'], color='tab:gray')
        ax[1].legend(['Temperature spread'])
        ax[1].set_xlabel('Date')
        ax[1].set_ylabel('Temperature (°C)')
        f.suptitle('Historical grid temperatures and temperature spread')
        plt.show(), plt.close()

        # plot histograms
        min_temp = math.floor(data[['Temp Vorlauf (degC)', 'Temp Ruecklauf (degC)']].min().min())
        max_temp = math.ceil(data[['Temp Vorlauf (degC)', 'Temp Ruecklauf (degC)']].max().max())
        bins = np.arange(min_temp, max_temp, step=0.5)
        plt.rcParams['font.size'] = 14
        f, ax = plt.subplots(2, 1, sharex=True, figsize=(16, 9))
        ax[0].grid('both')
        ax[0].hist(data['Temp Vorlauf (degC)'], color='tab:red', bins=bins)
        ax[0].axvline(data['Temp Vorlauf (degC)'].median(), ls='--', color='orange')
        ax[0].legend(['Historical data distribution',
                      'Median flow temperature: {:.1f} °C'.format(data['Temp Vorlauf (degC)'].median())])
        ax[0].set_ylabel('Frequency')
        ax[1].grid('both')
        ax[1].hist(data['Temp Ruecklauf (degC)'], color='tab:blue', bins=bins)
        ax[1].axvline(data['Temp Ruecklauf (degC)'].median(), ls='--', color='orange')
        ax[1].legend(['Historical data distribution',
                      'Median return temperature: {:.1f} °C'.format(data['Temp Ruecklauf (degC)'].median())])
        ax[1].set_ylabel('Frequency')
        ax[1].set_xlabel('Temperature (°C)')
        f.suptitle('Historical grid temperature distributions')
        plt.show(), plt.close()


def align_hkw_generation_and_supply(file, plot_data=False):
    """
    "Harmonises" historical heat provision (hourly average values) and heat generation (hourly instantaneous values)
     time series data to allow for relatively fair comparison of optimised heat generation mix and actual historical
     heat generation, especially with regards to estimating potential saving opportunity

    :param str file: Path to pre-cleaned consolidated time series data
    :param boolean plot_data: Boolean flag whether to create plots or not
    :returns pd.DataFrame: DataFrame with "harmonised" historical heat generation time series for GT and boilers
    """
    # read pre-cleaned heat generation history and grid supply time series
    orig = pd.read_csv(file, index_col=0, parse_dates=True, dayfirst=True)
    orig = orig[['Waermemenge Innenstadt (MW)', 'GT Waermeleistung (MW)',
                 'Waermeleistung Kessel4 (MW)', 'Waermeleistung Kessel5 (MW)', 'Waermeleistung Kessel6 (MW)']]
    orig.dropna(inplace=True)
    orig = orig.loc[:'2020']
    # resample to ensure hourly spacing and forward fill NaNs (e.g., daylight saving time changes)
    orig = orig.resample('1H').mean()
    orig.fillna(method='ffill', inplace=True)
    orig = orig.round(1)

    ### 1) evaluate original historical data  ###
    # calculate cumulative HKW production on HOURLY basis
    orig['Summe Erzeugung (MW)'] = orig[['GT Waermeleistung (MW)', 'Waermeleistung Kessel4 (MW)',
                                         'Waermeleistung Kessel5 (MW)', 'Waermeleistung Kessel6 (MW)']].sum(axis=1)
    # assess difference between HKW heat provision and HKW heat generation on HOURLY basis
    orig['Differenz (MW)'] = orig['Waermemenge Innenstadt (MW)'] - orig['Summe Erzeugung (MW)']
    # assess error on DAILY level
    orig_daily = orig.resample('1D').sum()
    orig_daily = orig_daily.round(1)
    if plot_data:
        plot_daily_generation_and_supply(orig_daily)

    ### 2) derive "harmonised"" DAILY data  ###
    adjusted = orig.copy()
    # replace zero HKW heat provision with ("arbitrary") minimum grid supply of 0.5 MWh
    adjusted['Waermemenge Innenstadt (MW)'][adjusted['Waermemenge Innenstadt (MW)'] == 0] = 0.5
    # set all gas turbine operations < 1 MWh/h to zero
    adjusted['GT Waermeleistung (MW)'] = np.where(adjusted['GT Waermeleistung (MW)'] < 1, 0,
                                                  adjusted['GT Waermeleistung (MW)'])
    # remove unrepresentatively short gas turbine operations
    min_duration = 3
    mask = adjusted['GT Waermeleistung (MW)'] > 0
    # derive all gas turbine operations with start interval and length of operation
    gt_active = adjusted['GT Waermeleistung (MW)'].index.to_series()[mask].\
                                                   groupby((~mask).cumsum()[mask]).agg(['first', 'size'])
    gt_deactivate = gt_active[gt_active['size'] < min_duration]
    gt_hist = adjusted['GT Waermeleistung (MW)'].copy()
    for i, row in gt_deactivate.iterrows():
        idx = gt_hist.index.get_loc(row['first'])
        # set gas turbine heat generation to zero
        gt_hist.iloc[idx: idx + row['size']] = 0
    # re-assign adjusted gas turbine history
    adjusted['GT Waermeleistung (MW)'] = gt_hist

    # re-assess cumulative HKW heat production and provision (on HOURLY basis)
    adjusted['Summe Erzeugung (MW)'] = adjusted[['GT Waermeleistung (MW)', 'Waermeleistung Kessel4 (MW)',
                                       'Waermeleistung Kessel5 (MW)', 'Waermeleistung Kessel6 (MW)']].sum(axis=1)
    adjusted['Differenz (MW)'] = adjusted['Waermemenge Innenstadt (MW)'] - adjusted['Summe Erzeugung (MW)']
    # re-assess error on DAILY level
    adjusted_daily = adjusted.resample('1D').sum()
    adjusted_daily = adjusted_daily.round(1)

    # initialise further columns
    adjusted_daily['Waermespeicher (MW)'] = 0           # How much heat is stored in heat storage (EOD)?
    adjusted_daily['Korrektur (MW)'] = np.nan           # How many MWh/d need to be generated additionally?

    ### 2.1) excess heat generation  ###
    # 1) excess heat generation without GT
    # ASSUMPTION: used for steam generation etc. and cannot be stored / supplied to the grid later (i.e., is 'wasted')
    mismatch = adjusted_daily[(adjusted_daily['Differenz (MW)'] < 0) & (adjusted_daily['GT Waermeleistung (MW)']
                                                                        == 0)]['Differenz (MW)']
    # iterate through all elements of mismatch (DataSeries) and derive necessary correction value
    for ix, val in mismatch.iteritems():
        adjusted_daily.loc[ix, 'Korrektur (MW)'] = val

    # 2) excess heat generation with GT
    # ASSUMPTION: used to load the heat storage and can be used to compensate heat production deficits later; 0% energy
    # losses assumed; storage discharge directly after charging
    storage = 0
    # iterate through all elements of adjusted_daily (DataFrame)
    for ix, row in adjusted_daily.iterrows():
        # charge heat storage
        if (row['Differenz (MW)'] < 0) & (row['GT Waermeleistung (MW)'] > 0):
            storage -= row['Differenz (MW)']
            adjusted_daily.loc[ix, 'Waermespeicher (MW)'] += storage
            adjusted_daily.loc[ix, 'Korrektur (MW)'] = 0
        # discharge heat storage
        if (row['Differenz (MW)'] > 0) & (storage > 0):
            if storage > row['Differenz (MW)']:
                adjusted_daily.loc[ix, 'Waermespeicher (MW)'] = storage - row['Differenz (MW)']
                adjusted_daily.loc[ix, 'Korrektur (MW)'] = 0
            else:
                adjusted_daily.loc[ix, 'Waermespeicher (MW)'] = 0
                adjusted_daily.loc[ix, 'Korrektur (MW)'] = row['Differenz (MW)'] - storage
            storage -= min(storage, row['Differenz (MW)'])

    ### 2.2) deficit heat generation  ###
    adjusted_daily['Korrektur (MW)'] = np.where(np.isnan(adjusted_daily['Korrektur (MW)']),
                                                adjusted_daily['Differenz (MW)'], adjusted_daily['Korrektur (MW)'])
    adjusted_daily['Summe Erzeugung (MW)'] += adjusted_daily['Korrektur (MW)']
    adjusted_daily['Differenz (MW)'] = adjusted_daily['Waermemenge Innenstadt (MW)'] - adjusted_daily['Summe Erzeugung (MW)']
    adjusted_daily = adjusted_daily.round(1)
    if plot_data:
        plot_daily_generation_and_supply(adjusted_daily)

    ### 3) derive "harmonised"" HOURLY data  ###
    # define maximum hourly capacity per generator
    capacity = {'Waermeleistung Kessel6 (MW)': 4.5,
                'Waermeleistung Kessel4 (MW)': 7.5,
                'Waermeleistung Kessel5 (MW)': 7.5}
    # measurement uncertainty wrt heat generation due to measuring orifice
    uncertainty = 0.6

    # iterate through all elements of adjusted_daily (DataFrame)
    for ix, row in adjusted_daily.iterrows():
        # if historical generation volume needs to be decreased
        if row['Korrektur (MW)'] < 0:
            corr = abs(row['Korrektur (MW)'])
            generation_day = row[capacity.keys()].sort_values()
            # deactivate aggregate for full day if correction volume is large enough
            while corr >= generation_day[0]:
                adjusted.loc[ix.strftime('%Y-%m-%d'), generation_day.index[0]] = 0
                corr -= generation_day[0]
                generation_day.drop(generation_day.index[0], inplace=True)
            while corr > 0:
                # reduce generated heat volume proportionally across all active hours
                share = corr / adjusted.loc[ix.strftime('%Y-%m-%d'), generation_day.index[0]].sum()
                # iterate through all hours of aggregate with lowest load at date 'ix' (DataSeries)
                for ix2, val in adjusted.loc[ix.strftime('%Y-%m-%d'), generation_day.index[0]].iteritems():
                    if (val > 0) & (corr > 0):
                        # subtract maximum of 0.1 MWh or proportional amount based on current hourly load
                        reduce_by = max(0.1, math.floor(share * val * 10) / 10)
                        if (val - reduce_by) >= 0:
                            adjusted.loc[ix2, generation_day.index[0]] -= reduce_by
                            corr -= reduce_by

        # if historical generation volume needs to be increased
        if row['Korrektur (MW)'] > 0:
            corr = abs(row['Korrektur (MW)'])
            generation_day = row[capacity.keys()].to_frame('used')
            generation_day['remaining'] = generation_day.index.map(capacity) * 24 - generation_day['used']
            # start by filling already active generators (in decreasing order wrt daily heat contribution)
            if generation_day['used'].sum() > 0:
                generation_day = generation_day.sort_values(by=['used', 'remaining'], ascending=False)
                # fill currently active aggregate(s) until full capacity
                while corr >= generation_day['remaining'][0]:
                    adjusted.loc[ix.strftime('%Y-%m-%d'), generation_day.index[0]] = capacity[generation_day.index[0]]
                    corr -= generation_day['remaining'][0]
                    generation_day.drop(generation_day.index[0], inplace=True)

                # fill currently active intervals (proportionally to already existing heat load)
                active = adjusted.loc[ix.strftime('%Y-%m-%d'), generation_day.index[0]][
                    (adjusted.loc[ix.strftime('%Y-%m-%d'), generation_day.index[0]] > 0) &
                    (adjusted.loc[ix.strftime('%Y-%m-%d'), generation_day.index[0]] < capacity[generation_day.index[0]])]
                rem_active = len(active) * capacity[generation_day.index[0]] - active.sum()
                while (corr > 0) & (rem_active > 0):
                    # increase generated heat volume proportionally across all active hours
                    share = corr / active.sum()
                    # iterate through all hours of aggregate with lowest load at date 'ix' (DataSeries)
                    for ix2, val in active.iteritems():
                        # assess remaining capacity
                        rem = math.floor((capacity[generation_day.index[0]] - val) * 10) / 10
                        if (rem > 0) & (corr > 0):
                            # increase minimum 0.1 MWh (if possible)
                            increase_by = max(0.1, math.floor(share * val * 10) / 10)
                            if (val + increase_by) > capacity[generation_day.index[0]]:
                                increase_by = capacity[generation_day.index[0]] - val
                            adjusted.loc[ix2, generation_day.index[0]] += increase_by
                            corr -= increase_by
                    # re-assess remaining heat volume
                    active = adjusted.loc[ix.strftime('%Y-%m-%d'), generation_day.index[0]][
                        (adjusted.loc[ix.strftime('%Y-%m-%d'), generation_day.index[0]] > 0) &
                        (adjusted.loc[ix.strftime('%Y-%m-%d'), generation_day.index[0]] < capacity[
                            generation_day.index[0]])]
                    rem_active = len(active) * capacity[generation_day.index[0]] - active.sum()
                    # round to avoid infinite loops for smaller discrepancies
                    corr = math.floor(corr * 10) / 10

                # fill currently inactive intervals (on active generators)
                active = adjusted.loc[ix.strftime('%Y-%m-%d'), generation_day.index[0]][
                    adjusted.loc[ix.strftime('%Y-%m-%d'), generation_day.index[0]] > 0]
                while corr > 0:
                    increase_by = max(uncertainty, math.floor(corr / (24 - len(active)) * 10) / 10)
                    for ix2, val in adjusted.loc[ix.strftime('%Y-%m-%d'), generation_day.index[0]].iteritems():
                        # assess remaining capacity
                        rem = math.floor((capacity[generation_day.index[0]] - val) * 10) / 10
                        if (rem > 0) & (corr > 0):
                            if increase_by < corr:
                                adjusted.loc[ix2, generation_day.index[0]] += increase_by
                                corr -= increase_by
                            else:
                                adjusted.loc[ix2, generation_day.index[0]] += corr
                                corr = 0

            # if no generator is already active, start with boiler 6
            else:
                # fill entire aggregate until capacity
                while corr >= generation_day['remaining'][0]:
                    adjusted.loc[ix.strftime('%Y-%m-%d'), generation_day.index[0]] = capacity[generation_day.index[0]]
                    corr -= generation_day['remaining'][0]
                    generation_day.drop(generation_day.index[0], inplace=True)

                # fill currently inactive intervals
                while corr > 0:
                    increase_by = max(uncertainty, math.floor(corr / 24 * 10) / 10)
                    for ix2, val in adjusted.loc[ix.strftime('%Y-%m-%d'), generation_day.index[0]].iteritems():
                        # assess remaining capacity
                        rem = math.floor((capacity[generation_day.index[0]] - val) * 10) / 10
                        if (rem > 0) & (corr > 0):
                            if increase_by < corr:
                                adjusted.loc[ix2, generation_day.index[0]] += increase_by
                                corr -= increase_by
                            else:
                                adjusted.loc[ix2, generation_day.index[0]] += corr
                                corr = 0

    # re-assess cumulative HKW heat production and provision (on HOURLY basis)
    adjusted['Summe Erzeugung (MW)'] = adjusted[['GT Waermeleistung (MW)', 'Waermeleistung Kessel4 (MW)',
                                       'Waermeleistung Kessel5 (MW)', 'Waermeleistung Kessel6 (MW)']].sum(axis=1)
    adjusted['Differenz (MW)'] = adjusted['Waermemenge Innenstadt (MW)'] - adjusted['Summe Erzeugung (MW)']
    # re-assess error on DAILY level
    adjusted_daily_new = adjusted.resample('1D').sum()
    adjusted_daily_new = adjusted_daily_new.round(1)

    if plot_data:

        plot_daily_generation_and_supply(adjusted_daily_new)

        # plot heat generation distribution for aggregates
        generators = {'Kessel 4': ['Waermeleistung Kessel4 (MW)', 7.5],
                      'Kessel 5': ['Waermeleistung Kessel5 (MW)', 7.5],
                      'Kessel 6': ['Waermeleistung Kessel6 (MW)', 4.5],
                      'Gasturbine': ['GT Waermeleistung (MW)', 12.5]}

        plt.rcParams['font.size'] = 14
        f, ax = plt.subplots(2, 2, figsize=(16, 9))
        f.suptitle('Waermeerzeugung je Aggregat (Verteilungen fuer Erzeugungswerte > 0.1 MWh)')
        p = 0
        for gen in generators:
            # extract values larger zero
            orig_values = orig[generators[gen][0]][orig[generators[gen][0]] >= 0.01]
            adj_values = adjusted[generators[gen][0]][adjusted[generators[gen][0]] >= 0.01]
            bins = np.arange(0.01, generators[gen][1]+0.1, 0.1)
            # plot histogram
            ax[p // 2][p % 2].grid('both')
            ax[p // 2][p % 2].set_title(gen + ' (Anteil Waermeerzeugung < 0.1 MWh/h: {:.0%})'.format(1-(len(orig_values) /
                                                                                                        len(orig))))
            ax[p // 2][p % 2].hist([orig_values, adj_values], bins=bins, density=True)
            # include kernel density estimations
            gkde = stats.gaussian_kde(orig_values)
            ax[p // 2][p % 2].plot(bins, gkde.evaluate(bins), color='tab:blue', label='Originaldaten')
            gkde = stats.gaussian_kde(adj_values)
            ax[p // 2][p % 2].plot(bins, gkde.evaluate(bins), color='tab:orange', label='Harmonisierte Daten')
            ax[p // 2][p % 2].legend()
            # add axis labels
            if p % 2 == 0:
                ax[p // 2][p % 2].set_ylabel('Haeufigkeitsdichte')
            if p // 2 == 1:
                ax[p // 2][p % 2].set_xlabel('Waermemenge (MWh/h)')
            p += 1
        plt.tight_layout()
        plt.show()

    return adjusted[['GT Waermeleistung (MW)', 'Waermeleistung Kessel4 (MW)', 'Waermeleistung Kessel5 (MW)',
                     'Waermeleistung Kessel6 (MW)']]


def plot_daily_generation_and_supply(generation_data):
    """
    Plots daily sums of heat generation and provision as scatter plots as well as cumulative values per year

    :param pd.DataFrame generation_data: Historical heat generation data with cumulative generation and DateTimeIndex
                                         with frequency
    """
    if generation_data.index.freq == 'D':
        daily_data = generation_data
    elif generation_data.index.freq == 'H':
        daily_data = generation_data.resample('1D').sum()

    # plot control figures
    plt.rcParams['font.size'] = 14

    f1, ax1 = plt.subplots(2, 3, figsize=(16, 9))
    f1.suptitle('HKW Waermeeinspeisung vs. HKW Waermeerzeugung (kumulierte Tageswerte)')
    daily_max = daily_data['Waermemenge Innenstadt (MW)'].max()

    f2, ax2 = plt.subplots(2, 3, figsize=(16, 9))
    f2.suptitle('Kumulierte HKW Waermeeinspeisung vs. kumulierte HKW Waermeerzeugung')

    p = 0
    for year in [str(y) for y in range(2015, 2021)]:
        # scatter plot
        ax1[p // 3][p % 3].grid('both')
        ax1[p // 3][p % 3].scatter(daily_data.loc[year, 'Waermemenge Innenstadt (MW)'],
                                   daily_data.loc[year, 'Summe Erzeugung (MW)'], alpha=0.5)
        ax1[p // 3][p % 3].plot([0, daily_max], [0, daily_max], '--', color='tab:orange')
        if (p // 3) == 1:
            ax1[p // 3][p % 3].set_xlabel('HKW Einspeisung (MWh/d)')
        if (p % 3) == 0:
            ax1[p // 3][p % 3].set_ylabel('HKW Erzeugung (MWh/d)')
        ax1[p // 3][p % 3].set_title(year)

        # cumulative plots
        supply = daily_data.loc[year, 'Waermemenge Innenstadt (MW)'].cumsum()/1000
        generation = daily_data.loc[year, 'Summe Erzeugung (MW)'].cumsum()/1000
        ax2[p // 3][p % 3].grid('both')
        ax2[p // 3][p % 3].plot(supply, generation)
        ax2[p // 3][p % 3].plot([0, supply.max()], [0, supply.max()], '--', color='tab:orange')
        ax2[p // 3][p % 3].text(0, 0.75*supply.max(), 'Erzeugung: {:>.2f} GWh\nEinspeisung: {:>.2f} GWh\n'
                                'Unterdeckung: {:.0f} MWh\nUnterdeckung: {:.1%}'.format(generation.max(), supply.max(),
                                ((supply.max() - generation.max()) * 1000), ((supply.max() - generation.max()) /
                                supply.max())), backgroundcolor='w')
        if (p // 3) == 1:
            ax2[p // 3][p % 3].set_xlabel('HKW Einspeisung (GWh)')
        if (p % 3) == 0:
            ax2[p // 3][p % 3].set_ylabel('HKW Erzeugung (GWh)')
        ax2[p // 3][p % 3].set_title(year)

        p += 1

    plt.tight_layout()
    plt.show()


####################     BODY     ####################

# path to fully conditioned time series data and temperature regression model
root = Path(__file__).parent
data_cleaned = '..\\..\\data\\input\\processed\\data_consolidated_clean.csv'
data_cleaned_gen_adjusted = '..\\..\\data\\input\\processed\\data_consolidated_clean_gen_adjusted.csv'
ts_data = '..\\..\\data\\input\\processed\\fully_conditioned_timeseries.csv'
mhkw_ts_data = '..\\..\\data\\input\\processed\\conditioned_mhkw_timeseries.csv'
heat_demand_model = '..\\..\\data\\models\\timeseries\\heat_demand_regression.sav'
flow_temp_model = '..\\..\\data\\models\\timeseries\\flow_temperature_regression.sav'

# create conditioned MHKW time series data only if not already exists
# MHKW grid temperatures will be fully conditioned and re-engineered for missing periods
# MHKW pressure, flow rate and heat load are NOT re-engineered for missing periods
if os.path.exists(os.path.join(root, mhkw_ts_data)):
    pass
else:
    # load pre-cleaned data
    data_raw = read_timeseries(os.path.join(root, data_cleaned))
    # condition data
    data_cond = condition_mhkw_timeseries(data_raw)
    # write fully conditioned time series data
    data_cond.to_csv(os.path.join(root, mhkw_ts_data))

# "harmonise" SWPS heat generation data only if not already exists
if os.path.exists(os.path.join(root, data_cleaned_gen_adjusted)):
    pass
else:
    # load pre-cleaned data
    precleaned = pd.read_csv(os.path.join(root, data_cleaned), index_col='Date', parse_dates=True, dayfirst=True)
    # harmonise generation data
    gen_adj = align_hkw_generation_and_supply(os.path.join(root, data_cleaned), plot_data=False)
    precleaned[gen_adj.columns] = gen_adj
    # write pre-cleaned time series data with adjusted generation values
    precleaned.to_csv(os.path.join(root, data_cleaned_gen_adjusted))

# create fully conditioned time series data (of all variables needed for forecasting) only if not already exists
if os.path.exists(os.path.join(root, ts_data)):
    pass
else:
    # load pre-cleaned data
    data_raw = read_timeseries(os.path.join(root, data_cleaned))
    # condition data
    data_cond1 = condition_hkw_timeseries(data_raw)
    data_cond2 = pd.read_csv(os.path.join(root, mhkw_ts_data), index_col='Date', parse_dates=True, dayfirst=True,
                            usecols=['Date', 'MHKW Temp Vorlauf (degC)', 'MHKW Temp Ruecklauf (degC)'])
    data_cond = pd.concat([data_cond1, data_cond2], axis=1)
    # write fully conditioned time series data
    data_cond.to_csv(os.path.join(root, ts_data))

# create temperature regression model only if not already exists
if os.path.exists(os.path.join(root, heat_demand_model)):
    pass
else:
    # load fully conditioned time series data
    data_cond = pd.read_csv(os.path.join(root, ts_data), index_col='Date', parse_dates=True, dayfirst=True)
    # create and save regression model
    heat_demand_temperature_regression(data_cond)

# create flow temperature regression model only if not already exists
if os.path.exists(os.path.join(root, flow_temp_model)):
    pass
else:
    # load fully conditioned time series data
    data_cond = pd.read_csv(os.path.join(root, ts_data), index_col='Date', parse_dates=True, dayfirst=True)
    # create and save regression model
    flow_temperature_regression(data_cond)

####################     MAIN     ####################

if __name__ == '__main__':

    # plot annual time series 'overlayed' by one another
    #plot_timeseries(data_cond, 'fully conditioned')

    # plot comparison of flow and return temperatures
    #compare_grid_temperatures(os.path.join(root, ts_data))

    # plot generation data "harmonisation"
    #align_hkw_generation_and_supply(os.path.join(root, data_cleaned), plot_data=True)

    # load fully conditioned data
    data_cond = pd.read_csv(os.path.join(root, ts_data), index_col='Date', parse_dates=True, dayfirst=True,
                            usecols=['Date', 'Aussentemperatur (degC)', 'Waermeeinspeisung (MW)', 'Temp Vorlauf (degC)',
                                     'Temp Ruecklauf (degC)', 'MHKW Temp Vorlauf (degC)', 'MHKW Temp Ruecklauf (degC)'])
    data_cond = data_cond[:'2020']
    print_precleaned_vs_fully_conditioned = False

    # plot annual comparison of pre-cleaned and fully conditioned time series data (to detect outlier etc.)
    if print_precleaned_vs_fully_conditioned:
        # load pre-cleaned data
        precleaned = read_timeseries(os.path.join(root, data_cleaned))
        precleaned = precleaned[:'2020']

        for var in data_cond.columns:
            for year in data_cond.index.year.unique():
                plt.rcParams['font.size'] = 14
                plt.figure(figsize=(16, 9))
                plt.grid('both')
                plt.plot(precleaned.loc[str(year), var])
                plt.plot(data_cond.loc[str(year), var])
                plt.legend(['Original data', 'Conditioned data'])
                plt.ylabel(var)
                plt.title(var + ' ' + str(year))
                plt.show()
            #plot detailed example period
            plt.rcParams['font.size'] = 14
            plt.figure(figsize=(16, 9))
            plt.grid('both')
            plt.plot(precleaned.loc['2018-01-01':'2018-01-10', var])
            plt.plot(data_cond.loc['2018-01-01':'2018-01-10', var])
            plt.legend(['Original data', 'Conditioned data'])
            plt.ylabel(var)
            plt.title(var + ' ' + str(year))
            plt.show()

    #load and evaluate saved temperature regression model
    polyreg = pickle.load(open(os.path.join(root, heat_demand_model), 'rb'))
    temp = data_cond.groupby(by=['Aussentemperatur (degC)']).median()
    # re-format data for sklearn
    X = np.array(temp.index)
    X = X.reshape(-1, 1)
    y = polyreg.predict(X)

    # print correlation
    c = np.corrcoef(temp.index, temp['Waermeeinspeisung (MW)'])[0, 1]
    print('\nPearson correlation coefficient Waermeeinspeisung-Temperatur: %.2f' % c)

    # plot results
    plt.rcParams['font.size'] = 14
    plt.figure(figsize=(16,9))
    plt.grid('both')
    plt.scatter(X, temp['Waermeeinspeisung (MW)'], marker='.')
    plt.plot(X, y, color="orange", lw=2.0)
    plt.xlabel('Temperature (°C)')
    plt.ylabel('Heat demand (MW)')
    plt.title('Heat demand depending on ambient temperature')
    plt.legend(['Regression model', 'Historical data'])
    plt.show()

    # create correlation matrix and scatter plots
    #create_correlation_matrix(data_cond, 2)

