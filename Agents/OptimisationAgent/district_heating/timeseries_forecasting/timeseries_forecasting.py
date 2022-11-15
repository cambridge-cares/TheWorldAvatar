"""
Collection of various forecasting methods for time series data provided by SWPS (esp. heat load, district heating flow
and return temperatures)

Evaluates forecast performance of various methods for full year 2020 when called as main script

@author: Markus Hofmeister
"""

import os
import math
import random
import pickle
import copy
import numpy as np
import pandas as pd
import datetime as dt
from pathlib import Path
import matplotlib.pyplot as plt
import matplotlib.dates as mdates

from sklearn import preprocessing

from statsmodels.tsa.arima.model import ARIMA
from sklearn.metrics import mean_squared_error
from pmdarima.preprocessing import FourierFeaturizer

# ensure existence of fully conditioned time series data and temperature regression model
from district_heating.timeseries_forecasting import timeseries_analysis
from district_heating.timeseries_forecasting import create_SARIMAX_model


####################     FUNCTIONS     ####################


def naive_forecast(ts_hist, forecast_length, start=False):
    """
    Creates naive/persistent forecast of length 'forecast_length'

    :param pd.DataFrame ts_hist: DataFrame with DateTimeIndex of historical time series data (hourly intervals)
    :param int forecast_length: length of forecast (in h)
    :param str start: string describing DateTime of start of forecast (start equals first time step to be forecasted;
                      if no start is specified, forecast starts at end of historical time series)
    :returns pd.DataFrame: DataFrame with DateTimeIndex of forecasted time series
    """

    if start:
        # extract index to start forecast
        if isinstance(ts_hist.index.get_loc(start), slice):
            # if start returns a slice, extract start index
            s = ts_hist.index.get_loc(start).start
        else:
            # if start returns single int index
            s = ts_hist.index.get_loc(start)
        fc = ts_hist.iloc[s-1 : s+forecast_length, :].copy()
        fc.iloc[1:, :] = np.nan
        fc.fillna(method='ffill', inplace=True)
        fc = fc.iloc[1:, :]

    else:
        # start forecast at end of historical time series
        start = ts_hist.index[-1]
        freq = ts_hist.index.inferred_freq
        ind = pd.date_range(start, periods=forecast_length+1, freq=freq)[-forecast_length:]
        fc = pd.DataFrame(index=ind, columns=ts_hist.columns)
        fc.iloc[0, :] = ts_hist.iloc[-1, 0]
        fc.fillna(method='ffill', inplace=True)

    return fc


def seasonal_naive_forecast(ts_hist, seasonal_periods, forecast_length, start=False, temperature=None):
    """
    Creates seasonal naive forecast of length 'forecast_length'

    :param pd.DataFrame ts_hist: DataFrame with DateTimeIndex of historical time series data (hourly intervals)
    :param int seasonal_periods: length of one seasonal period (in h)
    :param int forecast_length: length of forecast (in h)
    :param str start: string describing DateTime of start of forecast (start equals first time step to be forecasted;
                      if no start is specified, forecast starts at end of historical time series)
    :param pd.DataFrame temperature: DataFrame with DateTimeIndex of historical (& forecasted) temperature
    :returns pd.DataFrame: DataFrame with DateTimeIndex of forecasted time series
    """

    ###   create 'simple' seasonal naive forecast   ###

    if start:
        # extract index to start forecast
        if isinstance(ts_hist.index.get_loc(start), slice):
            # if start returns a slice, extract start index
            s = ts_hist.index.get_loc(start).start
        else:
            # if start returns single int index
            s = ts_hist.index.get_loc(start)
        ind = ts_hist.iloc[s:s + forecast_length, :].index
    else:
        # extend index beyond current data history
        s = len(ts_hist)
        start = ts_hist.index[-1]
        freq = ts_hist.index.inferred_freq
        ind = pd.date_range(start, periods=forecast_length+1, freq=freq)[-forecast_length:]

    # derive historical data from previous season
    train = ts_hist[:s]
    n = math.ceil(forecast_length/seasonal_periods)     # number of seasons to go back
    predictions = train[-n * seasonal_periods:]         # timeseries data for last n seasons
    predictions = predictions[:forecast_length]         # trim forecast to forecast_length

    # construct forecast DataFrame
    fc = pd.DataFrame(index=ind, data=predictions.values, columns=predictions.columns)

    ###   create temperature adjusted 'seasonal' naive forecast   ###

    # ONLY for heat load forecast + in-sample predictions, i.e. start not False to ensure temperature data is available
    if temperature is not None and 'Waermeeinspeisung (MW)' in fc.columns and bool(start):

        # load saved temperature regression model for heat demand
        root = Path(__file__).parent
        model = '..\\..\\data\\models\\timeseries\\heat_demand_regression.sav'
        polyreg = pickle.load(open(os.path.join(root, model), 'rb'))

        # derive temperature data associated with historical heat demand and forecast period
        temp_hist = temperature.loc[train.index]
        temp_forecast = temperature.loc[ind]
        # derive actual (old) temperatures for naive forecast
        temp_old = temp_hist[-n * seasonal_periods:]
        temp_old = temp_old[:forecast_length]
        # scale based on temperature regression model
        # 1) assess regression model heat demand based on historical temperature data
        X = np.array(temp_old).reshape(-1, 1)
        y1 = polyreg.predict(X)
        # 2) assess regression model heat demand for forecasted temperature data
        X = np.array(temp_forecast).reshape(-1, 1)
        y2 = polyreg.predict(X)
        # scaling factor for naive forecast
        scaler = y2/y1
        # add to prediction DataFrame
        fc['Waermeeinspeisung (MW)'] = fc['Waermeeinspeisung (MW)'] * scaler

    return fc.round(2)


def hourly_average_forecast(ts_hist, forecast_length, start=False, temperature=None):
    """
    Creates forecast of length 'forecast_length' based on historical average values of particular hour and day

    :param pd.DataFrame ts_hist: DataFrame with DateTimeIndex of historical time series data (hourly intervals)
    :param int forecast_length: length of forecast (in h)
    :param str start: string describing DateTime of start of forecast (start equals first time step to be forecasted;
                      if no start is specified, forecast starts at end of historical time series)
    :param pd.DataFrame temperature: DataFrame with DateTimeIndex of historical (& forecasted) temperature
    :returns pd.DataFrame: DataFrame with DateTimeIndex of forecasted time series
    """

    ###   create 'simple' average forecast   ###

    if start:
        # extract index to start forecast
        if isinstance(ts_hist.index.get_loc(start), slice):
            # if start returns a slice, extract start index
            s = ts_hist.index.get_loc(start).start
        else:
            # if start returns single int index
            s = ts_hist.index.get_loc(start)
        ind = ts_hist.iloc[s:s + forecast_length, :].index
    else:
        # extend index beyond current data history
        s = len(ts_hist)
        start = ts_hist.index[-1]
        freq = ts_hist.index.inferred_freq
        ind = pd.date_range(start, periods=forecast_length+1, freq=freq)[-forecast_length:]

    # extract available historical data
    train = ts_hist[:s]
    # derive average per hour per day based on 'available' data history
    hourly_ave = timeseries_analysis.hourly_averages(train)

    # create Hour per DayOfYear as multiindex for forecast period to allow later extraction from 'hourly_ave'
    period = pd.DataFrame()
    period['Hour'] = ind.to_series()
    period['Hour'] = period['Hour'].dt.hour
    period['DayOfYear'] = ind.to_series()
    period['DayOfYear'] = period['DayOfYear'].dt.dayofyear
    period['placeholder'] = 0       # placeholder data to enable groupby
    period = period.groupby(['DayOfYear', 'Hour']).mean()

    # construct forecast DataFrame
    fc = pd.DataFrame(index=ind, data=hourly_ave.loc[period.index].values, columns=hourly_ave.columns)

    ###   create temperature adjusted average forecast   ###

    # ONLY for heat load forecast + in-sample predictions, i.e. start not False to ensure temperature data is available
    if temperature is not None and 'Waermeeinspeisung (MW)' in fc.columns and bool(start):

        # load saved temperature regression model for heat demand
        root = Path(__file__).parent
        model = '..\\..\\data\\models\\timeseries\\heat_demand_regression.sav'
        polyreg = pickle.load(open(os.path.join(root, model), 'rb'))

        # derive actual and average temperature data associated with heat demand forecast period
        temp_forecast = temperature.loc[ind]
        temp_average = timeseries_analysis.hourly_averages(temperature)
        temp_average = temp_average.loc[period.index]
        # scale based on temperature regression model
        # 1) assess regression model heat demand based on average temperature data
        X = np.array(temp_average).reshape(-1, 1)
        y1 = polyreg.predict(X)
        # 2) assess regression model heat demand for forecasted temperature data
        X = np.array(temp_forecast).reshape(-1, 1)
        y2 = polyreg.predict(X)
        # scaling factor for naive forecast
        scaler = y2/y1
        # add to prediction DataFrame
        fc['Waermeeinspeisung (MW)'] = fc['Waermeeinspeisung (MW)'] * scaler

    return fc


def temperature_regression(temperature, forecast_length, start):
    """
    Creates forecast of length 'forecast_length' based on pure regression with ambient temperature

    :param pd.DataFrame temperature: DataFrame with DateTimeIndex of historical (& forecasted) temperature
    :param int forecast_length: length of forecast (in h)
    :param str start: string describing DateTime of start of forecast (start equals first time step to be forecasted)
    :returns pd.DataFrame: DataFrame with DateTimeIndex of forecasted time series
    """

    # extract index to start forecast
    if isinstance(temperature.index.get_loc(start), slice):
        # if start returns a slice, extract start index
        s = temperature.index.get_loc(start).start
    else:
        # if start returns single int index
            s = temperature.index.get_loc(start)
    # derive indices for forecast period
    ind = temperature.iloc[s:s + forecast_length, :].index

    # derive temperature data for forecast period
    temp_forecast = temperature.loc[ind]

    # load saved temperature regression model for heat demand
    root = Path(__file__).parent
    model = '..\\..\\data\\models\\timeseries\\heat_demand_regression.sav'
    polyreg = pickle.load(open(os.path.join(root, model), 'rb'))

    # assess regression model heat demand for forecasted temperature data
    X = np.array(temp_forecast).reshape(-1, 1)
    y = polyreg.predict(X)

    fc = pd.DataFrame(index=ind, data=y.round(2))

    return fc


def evaluate_forecast_errors(ts_hist, forecast):
    """
    Returns accuracy metrics for provided forecasted and historical time series

    :param pd.DataFrame ts_hist: single column DataFrame with DateTimeIndex of actual time series data
    :param pd.DataFrame forecast: single column DataFrame with DateTimeIndex of forecasted time series data
    :returns float rmse: root mean square error
    :returns float max_err: maximum error
    """

    # derive ONLY data for mutual/overlapping indices
    ind = forecast.index
    ind = ind.intersection(ts_hist.index)
    ts = ts_hist.loc[ind]
    fc = forecast.loc[ind]

    # calculate error metrics
    rmse = math.sqrt(mean_squared_error(ts, fc))    # root mean square error: mean performance measure
    max_err = max(abs(ts - fc))                     # maximum error: worst performance measure

    return rmse, max_err


####################     FUNCTIONS     ####################

if __name__ == '__main__':

    # load fully conditioned historical time series data
    root = Path(__file__).parent
    ts_data = '..\\..\\data\\input\\processed\\fully_conditioned_timeseries.csv'
    ts = pd.read_csv(os.path.join(root, ts_data), index_col='Date', parse_dates=True, dayfirst=True)
    ts.index.freq = ts.index.inferred_freq
    ts = ts[:'2020']

    # create artificial training/fit history for MHKW grid temperatures for 2017
    ts.loc['2017', ['MHKW Temp Vorlauf (degC)', 'MHKW Temp Ruecklauf (degC)']] = \
        ts.loc['2018', ['MHKW Temp Vorlauf (degC)', 'MHKW Temp Ruecklauf (degC)']].values

    # initialize parameters for single day forecasts
    start = '2018-01-01 00:00:00'       # start of (consecutive) forecasting
    forecast_length = 24                # forecast length per single forecast (in h))
    forecasts = 3*365                   # number of consecutive forecasts (to cover full year 2020)
    season_length = 24                  # 24h daily pattern
    var = 'Waermeeinspeisung (MW)'    # variable to forecast
    period = '2020-11'                  # period for which 'final' forecasts will be plotted


    # initialize SARIMA(X) / regression model and fitting parameters
    if var == 'Temp Ruecklauf (degC)':
        model_params = {'arima': {'order': (2, 1, 1), 'seasonal_order': (1, 0, 1, 24)}, 'fourier': (0, 0)}
        regr = []
        fitting_params = {'method': 'statespace', 'low_memory': True, 'cov_type': 'none',
                          'method_kwargs': {'minimize_kwargs': {'method': 'powell', 'options': {'maxiter': 250}}}}
        seasons_to_use = 21             # length (number of short-term seasons) of data history to use
        refit_interval = 90             # refit SARIMA(X) model every X forecasts
        fc_name = '..\\..\\data\\models\\timeseries\\SARIMAX\\ReturnTemp_forecasts.csv'
    elif var == 'MHKW Temp Ruecklauf (degC)':
        model_params = {'arima': {'order': (2, 1, 1), 'seasonal_order': (1, 1, 1, 24)}, 'fourier': (0, 0)}
        regr = []
        fitting_params = {'method': 'statespace', 'low_memory': True, 'cov_type': 'none',
                          'method_kwargs': {'minimize_kwargs': {'method': 'powell', 'options': {'maxiter': 250}}}}
        seasons_to_use = 14             # length (number of short-term seasons) of data history to use
        refit_interval = 60             # refit SARIMA(X) model every X forecasts
        fc_name = '..\\..\\data\\models\\timeseries\\SARIMAX\\MHKW_ReturnTemp_forecasts.csv'
    elif var == 'Waermeeinspeisung (MW)':
        model_params = {'arima': {'order': (1, 0, 2), 'seasonal_order': (1, 1, 1, 24)}, 'fourier': (8766, 1)}
        regr = ['Aussentemperatur (degC)']
        fitting_params = {'method': 'statespace', 'low_memory': True, 'cov_type': 'none',
                          'method_kwargs': {'minimize_kwargs': {'method': 'powell', 'options': {'maxiter': 250}}}}
        seasons_to_use = 365            # length (number of short-term seasons) of data history to use
        refit_interval = 365            # refit SARIMA(X) model every X forecasts
        fc_name = '..\\..\\data\\models\\timeseries\\SARIMAX\\Heatload_forecasts.csv'
    elif var == 'Temp Vorlauf (degC)':
        fc_name = '..\\..\\data\\models\\timeseries\\SARIMAX\\FlowTemp_forecasts.csv'
        regr = ['Waermeeinspeisung (MW)', 'Aussentemperatur (degC)']
        model_params = {'arima': {'order': (1, 0, 0), 'seasonal_order': (2, 0, 1, 24)}, 'fourier': (0, 0)}
        fitting_params = {'method': 'statespace', 'low_memory': True, 'cov_type': 'none',
                          'method_kwargs': {'minimize_kwargs': {'method': 'powell', 'options': {'maxiter': 250}}}}
        seasons_to_use = 21             # length (number of short-term seasons) of data history to use
        refit_interval = 60             # refit SARIMA(X) model every X forecasts
    elif var == 'MHKW Temp Vorlauf (degC)':
        model_params = {'arima': {'order': (1, 0, 1), 'seasonal_order': (1, 1, 1, 24)}, 'fourier': (0, 0)}
        regr = ['Waermeeinspeisung (MW)', 'Aussentemperatur (degC)']
        fitting_params = {'method': 'statespace', 'low_memory': True, 'cov_type': 'none',
                          'method_kwargs': {'minimize_kwargs': {'method': 'powell', 'options': {'maxiter': 250}}}}
        seasons_to_use = 14             # length (number of short-term seasons) of data history to use
        refit_interval = 60             # refit SARIMA(X) model every X forecasts
        fc_name = '..\\..\\data\\models\\timeseries\\SARIMAX\\MHKW_FlowTemp_forecasts.csv'

    # initialize time series data (history)
    timeseries = ts[[var]].copy()
    regr = ts[regr].copy()
    if 'Waermeeinspeisung (MW)' in regr.columns:
        # replace actual heat load values with SARIMA(X) forecast values
        heat_load_forecast = '..\\..\\data\\models\\timeseries\\SARIMAX\\Heatload_forecasts.csv'
        hl_fc = pd.read_csv(os.path.join(root, heat_load_forecast), index_col='Date', parse_dates=True, dayfirst=True,
                    usecols=['Date', 'SARIMAX'])
        regr.loc[hl_fc.index, ['Waermeeinspeisung (MW)']] = hl_fc['SARIMAX']

    # ONLY relevant for heat load forecast
    if var == 'Waermeeinspeisung (MW)':
        # add calender effects and Fourier terms to eXogeneous regressors
        if len(regr.columns) > 0:
            regr = create_SARIMAX_model.add_daycharacteristics(regr)
            if model_params['fourier'][0] > 0 and model_params['fourier'][1] > 0:
                _, exog_data = FourierFeaturizer(*model_params['fourier']).fit_transform(timeseries)
                exog_data.set_index(timeseries.index, inplace=True)
                regr = pd.concat([regr, exog_data], axis=1)
                # min_max_scaler = preprocessing.MinMaxScaler()
                # regr_vals = min_max_scaler.fit_transform(regr)
                # regr = pd.DataFrame(index=regr.index, data=regr_vals, columns=regr.columns)


    ####################   CREATE CONSECUTIVE FORECASTS   ####################

    # loop over all consecutive forecasts
    print('###   Run consecutive forecasts   ###\n')
    for fc in range(forecasts):
        print('Forecast: {}'.format(fc + 1))

        # initially create SARIMA(X) model, potentially refit, and append 'new' actual data
        if fc % refit_interval == 0:
            sarimax_start = dt.datetime.strptime(start, "%Y-%m-%d %H:%M:%S")
            sarimax_start = sarimax_start - dt.timedelta(hours=season_length * seasons_to_use)
            sarimax_start = dt.datetime.strftime(sarimax_start, "%Y-%m-%d %H:%M:%S")
            sarimax_end = dt.datetime.strptime(start, "%Y-%m-%d %H:%M:%S")
            sarimax_end = sarimax_end - dt.timedelta(hours=1)
            sarimax_end = dt.datetime.strftime(sarimax_end, "%Y-%m-%d %H:%M:%S")
            ts_hist_sarimax = timeseries.loc[sarimax_start: sarimax_end]
            if len(regr.columns) > 0:
                regr_hist_sarimax = regr.loc[ts_hist_sarimax.index]
            else:
                regr_hist_sarimax = None

            # initialize and fit SARIMA(X) model
            model = ARIMA(ts_hist_sarimax, exog=regr_hist_sarimax, **model_params['arima'])
            model = model.fit(**copy.deepcopy(fitting_params))

            # define boundaries for next SARIMA(X) forecast
            end = dt.datetime.strptime(start, "%Y-%m-%d %H:%M:%S")
            end = end + dt.timedelta(hours=forecast_length - 1)
            end = dt.datetime.strftime(end, "%Y-%m-%d %H:%M:%S")
        else:
            model = model.append(endog=ts_actual, exog=regr_fc)

        # extract actual and regressor values for current forecasting period
        ts_actual = timeseries.loc[start:end]
        if len(regr.columns) > 0:
            regr_fc = regr.loc[ts_actual.index]
        else:
            regr_fc = None

        # create forecasts
        if fc == 0:
            naive = naive_forecast(timeseries, forecast_length, start=start)
            s_naive = seasonal_naive_forecast(timeseries, season_length, forecast_length, start=start)
            temp_reg = temperature_regression(ts[['Aussentemperatur (degC)']], forecast_length, start=start)
            sarimax = model.forecast(forecast_length, exog=regr_fc)
        else:
            naive = pd.concat([naive, naive_forecast(timeseries, forecast_length, start=start)], axis=0)
            s_naive = pd.concat([s_naive, seasonal_naive_forecast(timeseries, season_length, forecast_length,
                                                                  start=start)], axis=0)
            temp_reg = pd.concat([temp_reg, temperature_regression(ts[['Aussentemperatur (degC)']], forecast_length,
                                                                   start=start)], axis=0)
            sarimax = pd.concat([sarimax, model.forecast(forecast_length, exog=regr_fc)], axis=0)

        # update start & end
        start = dt.datetime.strptime(start, "%Y-%m-%d %H:%M:%S")
        start = start + dt.timedelta(hours=forecast_length)
        start = dt.datetime.strftime(start, "%Y-%m-%d %H:%M:%S")
        end = dt.datetime.strptime(end, "%Y-%m-%d %H:%M:%S")
        end = end + dt.timedelta(hours=forecast_length)
        end = dt.datetime.strftime(end, "%Y-%m-%d %H:%M:%S")

    # attach predictions to overall DataFrame
    forecasts = timeseries.loc[naive.index]
    naive.columns = ['Naive']                           # pd.DataFrame
    s_naive.columns = ['Seasonal naive']                # pd.DataFrame
    temp_reg.columns = ['Temperature regression']       # pd.DataFrame
    sarimax.name = 'SARIMAX'                            # pd.Series
    forecasts = pd.concat([forecasts, naive, s_naive, temp_reg, sarimax], axis=1)
    forecasts = forecasts.round(2)
    # save forecasts
    forecasts.to_csv(os.path.join(root, fc_name))


    ####################   EVALUATE FORECASTS   ####################

    # evaluate forecast errors
    print('\n###   ' + var + ' forecasting errors   ###\n')
    errors = forecasts.copy()
    for c in errors.columns:
        if c != var:
            rmse, max_err = evaluate_forecast_errors(errors[var], errors[c])
            print('{} - RMSE: {:.2f}   MaxError: {:.2f}'.format(c, rmse, max_err))
            errors[c] = errors[c] - errors[var]

    ###  plot error histograms  ###
    bins = np.arange(math.floor(errors.iloc[:, 1:].min().min()), math.ceil(errors.iloc[:, 1:].max().max()), step=0.2)
    plt.rcParams['font.size'] = 12
    f, ax = plt.subplots(2, 2, figsize=(16, 9))
    ax[0, 0].grid('both')
    ax[0, 0].hist(errors.iloc[:, 1], bins=bins)
    ax[0, 0].axvline(errors.iloc[:, 1].mean(), color='orange')
    ax[0, 0].axvline(errors.iloc[:, 1].mean() - errors.iloc[:, 1].std(), ls='--', color='orange')
    ax[0, 0].axvline(errors.iloc[:, 1].mean() + errors.iloc[:, 1].std(), ls='--', color='orange', label='_nolegend_')
    ax[0, 0].set_xlabel('Forecast error')
    ax[0, 0].set_ylabel('Count of occurrences')
    #ax[0, 0].set_title(errors.iloc[:, 1].name + ' forecast')
    ax[0, 0].legend(['Mean', '+/- Standard deviation', errors.iloc[:, 1].name])
    ax[1, 0].grid('both')
    ax[1, 0].hist(errors.iloc[:, 2], bins=bins)
    ax[1, 0].axvline(errors.iloc[:, 2].mean(), color='orange')
    ax[1, 0].axvline(errors.iloc[:, 2].mean() - errors.iloc[:, 2].std(), ls='--', color='orange')
    ax[1, 0].axvline(errors.iloc[:, 2].mean() + errors.iloc[:, 2].std(), ls='--', color='orange', label='_nolegend_')
    ax[1, 0].set_xlabel('Forecast error')
    ax[1, 0].set_ylabel('Count of occurrences')
    #ax[1, 0].set_title(errors.iloc[:, 2].name + ' forecast')
    ax[1, 0].legend(['Mean', '+/- Standard deviation', errors.iloc[:, 2].name])
    ax[0, 1].grid('both')
    ax[0, 1].hist(errors.iloc[:, 3], bins=bins)
    ax[0, 1].axvline(errors.iloc[:, 3].mean(), color='orange')
    ax[0, 1].axvline(errors.iloc[:, 3].mean() - errors.iloc[:, 3].std(), ls='--', color='orange')
    ax[0, 1].axvline(errors.iloc[:, 3].mean() + errors.iloc[:, 3].std(), ls='--', color='orange', label='_nolegend_')
    ax[0, 1].set_xlabel('Forecast error')
    ax[0, 1].set_ylabel('Count of occurrences')
    #ax[0, 1].set_title(errors.iloc[:, 3].name + ' forecast')
    ax[0, 1].legend(['Mean', '+/- Standard deviation', errors.iloc[:, 3].name])
    ax[1, 1].grid('both')
    ax[1, 1].hist(errors.iloc[:, 4], bins=bins)
    ax[1, 1].axvline(errors.iloc[:, 4].mean(), color='orange')
    ax[1, 1].axvline(errors.iloc[:, 4].mean() - errors.iloc[:, 4].std(), ls='--', color='orange')
    ax[1, 1].axvline(errors.iloc[:, 4].mean() + errors.iloc[:, 4].std(), ls='--', color='orange', label='_nolegend_')
    ax[1, 1].set_xlabel('Forecast error')
    ax[1, 1].set_ylabel('Count of occurrences')
    #ax[1, 1].set_title(errors.iloc[:, 4].name + ' forecast')
    ax[1, 1].legend(['Mean', '+/- Standard deviation', errors.iloc[:, 4].name])
    f.suptitle(var + ' : forecast errors')
    plt.show()

    ###  plot time series forecasts vs. actual historical data  ###
    plt.rcParams['font.size'] = 14
    n = len(forecasts.columns)-1
    f, ax = plt.subplots(n, 1, sharex=True, figsize=(16, 9))
    for i in range(n):
        ax[i].grid('both')
        ax[i].plot(forecasts.loc[period].iloc[:, [0, i+1]])
        ax[i].legend(forecasts.loc[period].iloc[:, [0, i+1]].columns, loc='upper left')
    f.suptitle(var + ' : actual data vs. forecasts')
    plt.show()

    # plot detailed SARIMA(X) /linear regression forecast
    f, ax = plt.subplots(figsize=(16, 9))
    ax.grid('both')
    ax.plot(forecasts.loc[period].iloc[:, 0])
    if 'model_params' in locals():
            ax.plot(forecasts.loc[period, 'SARIMAX'])
    else:
        ax.plot(forecasts.loc[period, 'Regression'])
    ax.legend(['Actual data', 'Forecast'], loc='upper left')
    f.suptitle(var + ' : actual data vs. forecast')
    plt.show()

    # extract days with largest SARIMA(X) forecast errors
    daily_err = errors[['SARIMAX']].copy()
    daily_err = abs(daily_err)
    daily_err = daily_err.resample('1D').mean()
    worst = daily_err[daily_err.iloc[:, 0] > daily_err.iloc[:, 0].quantile(0.95)]
    print('\n###   Worst 5% of forecasted days   ###\n')
    print(worst)

    ###   plot time series with large errors in detail  ###
    width = 3       # window width for each plot (in days)
    # extract dates
    for day in worst.index:
        ind = forecasts.index.get_loc(day)
        d = forecasts.iloc[ind-24*width : ind+24*(width+1)][[var, 'SARIMAX']]
        d['Aussentemperatur (degC)'] = ts.loc[d.index, 'Aussentemperatur (degC)']
        # plot time series
        f, ax = plt.subplots(2, sharex=True, figsize=(16, 9))
        ax[0].grid('both')
        ax[0].plot(d['Aussentemperatur (degC)'])
        ax[0].set_ylabel('Temperature (°C)')
        ax[1].grid('both')
        ax[1].plot(d[[var, 'SARIMAX']])
        ax[1].set_ylabel(var)
        ax[1].legend(['Actual data', 'SARIMAX forecast'], loc='upper left')
        f.suptitle(var)
        # shade weekends
        xmin, xmax = ax[1].get_xlim()
        days = np.arange(np.floor(xmin), np.ceil(xmax) + 2)
        weekends = [(dt.weekday() >= 5) | (dt.weekday() == 0) for dt in mdates.num2date(days)]
        ax[1].fill_between(days, *ax[1].get_ylim(), where=weekends, facecolor='k', alpha=.05)
        ax[1].set_xlim(xmin, xmax)
        plt.show()