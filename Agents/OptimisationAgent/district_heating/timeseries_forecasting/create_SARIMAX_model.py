"""
Find best fitting SARIMA(X) model for time series data (heat load, return temperature)
    - includes eXogeneous regressor variables (if required)
    - includes Fourier terms to model long & multiple seasonalities (if required)
    - plots in-sample forecast of fitted model with best hyperparameter configuration

@author: Markus Hofmeister
"""

import os
import re
import math
import requests
import sobol_seq
import numpy as np
import pandas as pd
import datetime as dt
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from pathlib import Path
import copy
import time

from statsmodels.graphics import tsaplots
from statsmodels.tsa.arima.model import ARIMA
from sklearn.metrics import mean_squared_error
from pmdarima.preprocessing import FourierFeaturizer
from statsmodels.tsa.stattools import adfuller
from statsmodels.tsa.arima.model import ARIMAResults

import multiprocessing as mp
from warnings import catch_warnings
from warnings import filterwarnings

from district_heating.timeseries_forecasting import timeseries_forecasting


####################     FUNCTIONS     ####################


def plot_screening_days(temperature, heat_load, days):
    """
    Plots and ambient temperature and heat load for selected days +/- 3 days

    :param pd.Series temperature: Series with DateTimeIndex of historical temperature data (single column)
    :param pd.Series heat_load: Series with DateTimeIndex of historical heat load data (single column)
    :param list days: list of strings with dates of days to be plotted
    """

    # extract start and end dates, depending on window width w (in days)
    w = 3
    start = [dt.datetime.strftime(dt.datetime.strptime(d, '%Y-%m-%d') - dt.timedelta(days=w), '%Y-%m-%d') for d in days]
    end = [dt.datetime.strftime(dt.datetime.strptime(d, '%Y-%m-%d') + dt.timedelta(days=w), '%Y-%m-%d') for d in days]

    # loop over all days
    for i in range(len(days)):
        plt.rcParams['font.size'] = 14
        f, ax = plt.subplots(2, sharex=True, figsize=(16, 9))
        ax[0].grid('both')
        ax[0].plot(temperature[start[i]:end[i]])
        ax[0].set_ylabel('Ambient temperature (°C)')
        ax[1].grid('both')
        ax[1].plot(heat_load[start[i]:end[i]])
        ax[1].set_ylabel('Heat load (MW)')
        plt.show()

    # create and print summary statistics
    q_min, q_mean, q_max, t_min, t_mean, t_max, we = [], [], [], [], [], [], []
    for day in days:
        q_min.append(heat_load[day].min())
        q_mean.append(heat_load[day].mean())
        q_max.append(heat_load[day].max())
        t_min.append(temperature[day].min())
        t_mean.append(temperature[day].mean())
        t_max.append(temperature[day].max())
        # in pandas.index.weekday: 0 ... Monday, 6 ... Sunday
        we.append(1 if heat_load[day].index.weekday.unique()[0] in [5, 6] else 0)
    stats = pd.DataFrame(data=zip(we, q_min, q_mean, q_max, t_min, t_mean, t_max), index=days,
                         columns=['Weekend', 'Q_min', 'Q_mean', 'Q_max', 'T_min', 'T_mean', 'T_max'])
    stats = stats.round(1)
    print('Summary statistics of days used for coarse SARIMA(X) model screening: \n')
    print(stats)


def plot_series_acf_pacf(ts, max_diff=0, max_sdiff=0, season=24):
    """
    Plot data as time series, autocorrelation function and partial autocorrelation function
        - creates ACF and PACF plots for differenced time series up to 'max_diff' differences and
          'max_sdiff' seasonal differences

    :param pd.Series ts: Series with DateTimeIndex of historical time series data (single column)
    :param int max_diff: maximum number of differences
    :param int max_sdiff: maximum number of seasonal differences
    :param int season: time steps within one seasonal period
    """

    # create local copy of time series
    data = ts.copy()
    data.dropna(inplace=True)

    # plot time series
    plt.rcParams['font.size'] = 14
    f, ax = plt.subplots(figsize=(16,9))
    ax.grid('both')
    ax.plot(data, '.--', ms=1, lw=0.5)
    ax.set_title(data.name)
    plt.show(), plt.close()

    # extract only subset of time series data for Dickey-Fuller test (Dickey-Fuller test might 'miss' long annual
    # seasonality in hourly data and erroneously reject null hypothesis 'pretending' stationarity )
    if data.index.inferred_freq == 'H':
        # extract 1 year for hourly data
        d = data.iloc[-24 * 365:].copy()
    elif data.index.inferred_freq == 'D':
        # extract 3 years for daily data
        d = data.loc[-365 * 3:].copy()
    elif data.index.inferred_freq == 'M':
        # extract 5 years for monthly data
        d = data.loc[-12 * 5:].copy()

    # create ACF and PACF plots for original and differenced time series & print out Dickey-Fuller p-values
    l = min(100, len(data) // 2 - 1)       # number of 'allowed' lags limited internally by plot_acf function
    print('\n#####   Augmented Dickey-Fuller test   #####\n')

    # i ... number of seasonal differences, j ... number of regular differences
    # loop over all seasonal differences
    for i in range(max_sdiff+1):
        if i == 0:
            plot_data = data.copy()
            d_diff = d.copy()
        else:
            plot_data = data.diff(i*season)
            plot_data.dropna(inplace=True)
            d_diff = d.diff(i*season)
            d_diff.dropna(inplace=True)

        # loop over all regular differences
        for j in range(max_diff+1):
            if i + j > 2:
                # never use more than TWO total differences (seasonal and non-seasonal combined) according to
                # http://people.duke.edu/~rnau/seasarim.htm
                continue
            if j == 0:
                plot_data2 = plot_data.copy()
                d_diff2 = d_diff.copy()
            else:
                plot_data2 = plot_data2.diff(j)
                plot_data2.dropna(inplace=True)
                d_diff2 = d_diff2.diff(j)
                d_diff2.dropna(inplace=True)
            f, ax = plt.subplots(2, 1, figsize=(16,9))
            tsaplots.plot_acf(plot_data2, lags=l, ax=ax[0])
            tsaplots.plot_pacf(plot_data2, lags=l, ax=ax[1])
            ax[1].set_xlabel('Lag')
            f.suptitle('{}:  {} time(s) differenced, {} time(s) seasonally differenced'.format(plot_data2.name, j, i))
            plt.show(), plt.close()
            # run Dickey-Fuller test (null hypothesis: series is NOT stationary)
            pval = adfuller(d_diff2)[1]
            print('Differences: {}   Seasonal differences: {}   P-value: {:.6f}'.format(j, i, pval))


def select_days(start, end, number):
    """
    Creates an (approx.) equally spaced series of 'number' days between start and end
        - plots histogram of days wrt corresponding month and day of week to ensure equal distribution

    :params str start: start date in the format "%Y-%m-%d"
    :params str end: end date in the format "%Y-%m-%d"
    :param int number: number of days to create
    :returns list days: list of dates in the format "%Y-%m-%d"
    """

    # create series of days
    start = dt.datetime.strptime(start, "%Y-%m-%d")
    end = dt.datetime.strptime(end, "%Y-%m-%d")
    days = []
    for d in create_sobol_points(number, 1, (end-start).days):
        day = start + dt.timedelta(days=int(d[0]))
        days.append(dt.datetime.strftime(day, "%Y-%m-%d"))

    # check equal distribution across months and days of week
    check = pd.DataFrame(index=days)
    check.index = pd.to_datetime(check.index)
    check['dow'] = check.index.to_series()
    check['dow'] = check['dow'].dt.dayofweek
    check['month'] = check.index.to_series()
    check['month'] = check['month'].dt.month
    plt.hist(check['month'], bins=list(range(1, 14)))
    plt.xticks(list(range(1, 13)), ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', ' Aug', 'Sep', 'Oct', 'Nov', 'Dec'])
    plt.xlabel('Month'), plt.ylabel('Count of occurrences'), plt.show()
    plt.hist(check['dow'], bins=list(range(8)))
    plt.xticks(list(range(7)), ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'])
    plt.xlabel('Day of week'), plt.ylabel('Count of occurrences'), plt.show()
    plt.show(), plt.close()

    return days


def add_daycharacteristics(timeseries):
    """
    Incorporates "external" characteristics for each day in DateTime index of time series 'timeseries'

        Adds columns ['Monday', ... 'Sunday'] to DataFrame: 1 for days which match column header, 0 otherwise
        Adds column 'Holiday': 1 if day is a holiday in Rheinland-Pfalz, 0 otherwise (Sundays are holidays)
        Adds column 'Vacation': 1 if day lays within school vacations, otherwise 0

    :param pd.DataFrame timeseries: DataFrame with historical time series of regressor variable(s), e.g. temperature
    """

    #####   Add weekdays vs. weekend   #####
    # 0 - Monday ... 6 - Sunday
    timeseries['DayOfWeek'] = timeseries.index.weekday
    timeseries['Weekday'] = np.where(timeseries['DayOfWeek'] < 5, 1, 0)
    timeseries['Weekend'] = np.where(timeseries['Weekday'] == 0, 1, 0)
    timeseries['Monday'] = np.where(timeseries['DayOfWeek'] == 0, 1, 0)
    timeseries['Saturday'] = np.where(timeseries['DayOfWeek'] == 5, 1, 0)
    timeseries.drop(columns=['DayOfWeek'], inplace=True)

    #####   Add holidays (via API)   #####
    # extract all years in time series
    years = timeseries.index.year.unique()
    dates = list()
    for year in years:
        # adjust API url for each year
        url = 'https://feiertage-api.de/api/?jahr=' + str(year) + '&nur_land=RP'
        # get API data in JSON format
        temp_data = requests.get(url).json()
        # extract dates from API data
        entries = [entry['datum'] for entry in temp_data.values()]
        dates.extend(entries)
    # create helper column 'date' in timeseries DataFrame
    timeseries['date'] = timeseries.index.date.astype(str)
    # create column 'Holiday' with 1 if holiday and 0 otherwise
    timeseries['Holiday'] = np.where(timeseries['date'].isin(dates), 1, 0)

    #####   Add vacation (no API --> HTML web scraping)   #####
    dates = list()
    for year in years:
        # adjust website url for each year
        url = 'https://www.schulferien.org/Kalender_mit_Ferien/kalender_' + str(year) + '_ferien_Rheinland_Pfalz.html'
        # get website as HTML object
        page = requests.get(url)
        # vacation dates are always listed as '<br> Rheinland-Pfalz <br>dd.mm.yyyy - dd.mm.yyyy' in website source code
        # find '<br> Rheinland-Pfalz <br>' pattern
        entries = [i.end() for i in re.finditer('&lt;br&gt;Rheinland-Pfalz  &lt;br&gt;', page.text)]
        # extract vacation durations
        entries = [page.text[entry:entry+len('dd.mm.yyyy - dd.mm.yyyy')] for entry in entries]
        dates.extend(entries)
    # remove all duplicates
    dates = list(dict.fromkeys(dates))
    # separate start and end dates
    start = [entry.split(' - ')[0] for entry in dates]
    end = [entry.split(' - ')[1] for entry in dates]
    # convert to DateTime objects
    start = [dt.datetime.strptime(entry, '%d.%m.%Y') for entry in start]
    end = [dt.datetime.strptime(entry, '%d.%m.%Y') for entry in end]
    # create Datetime index encompassing all vacation periods
    idx = pd.DatetimeIndex([])
    for i in range(len(start)):
        idx = idx.union(pd.date_range(start=start[i], end=end[i]))
    # create column 'Vacation' with 1 if vacation and 0 otherwise
    timeseries['Vacation'] = np.where(timeseries['date'].isin(idx.astype(str)), 1, 0)


    # drop helper column 'date' and potential NaNs
    timeseries.drop(columns=['date'], inplace=True)
    timeseries = timeseries.dropna()

    return timeseries


def create_sobol_points(length, order, maxorder):
    """
    Returns set of Sobol points

    :param int length: number of Sobol points to create
    :param int order: number of elements per Sobol point
    :param tuple maxorder: maximum value per point element/range of values (p_max, d_max, q_max)
    """

    # initialize
    missing = length
    skip = 0

    if (np.array(maxorder)+1).prod() > length:
        # avoid creation of Sobol point duplicates if there are enough possibilities of different combinations
        while missing > 0:
            # generate 'missing' Sobol points
            sob = sobol_seq.i4_sobol_generate(order, missing, skip)
            # scale Sobol points of range [0,1] to maxorder and round to int
            sob *= maxorder
            # round and integer conversion
            sob = sob.round().astype(int)
            # append to overall Sobol points
            if missing == length:
                sobol = sob.copy()
            else:
                sobol = np.concatenate((sobol, sob), axis=0)
            # remove potential duplicate rows (due to rounding)
            sobol = np.unique(sobol, axis=0)
            skip += missing
            missing = length - len(sobol)
    else:
        # generate 'missing' Sobol points
        sob = sobol_seq.i4_sobol_generate(order, missing, skip)
        # scale Sobol points of range [0,1] to maxorder and round to int
        sob *= maxorder
        # round and integer conversion
        sobol = sob.round().astype(int)

    return sobol


def create_model_and_validate(timeseries, forecast_length, model_params, fit_params, exog_data=None,
                              return_model=False, save_model=False, model_name='model'):
    """
    Creates SARIMA(X) model and evaluates RMSE of in-sample prediction

    :param pd.DataFrame: DataFrame (one column) of time series to be forecasted
    :param int forecast_length: length of forecast / number of time steps
    :param dict model_params: dictionary describing SARIMAX model [fourier, order, seasonal_order]
    :param dict fit_params: dictionary providing fitting parameters passed to statsmodels ARIMA fit function
    :param pd.DataFrame exog_data: time series of potential eXogeneous regressor data
    :param bool return_model: boolean flag whether to return fitted model
    :param bool save_model: boolean flag whether to save fitted model
    :param str model_name: string describing how the saved model shall be named
    :returns pd.DataFrame: in-sample forecast of length 'forecast_length'
    :returns float: Akaike Information Criterion of fitted model
    :returns float: Root mean square error of in-sample forecast
    :returns float: maximum error of in-sample forecast
    :returns pd.Series: parameter overview of fitted SARIMAX model
    :returns ARIMAResults: fitted model as ARIMAResults object
    """

    # split timeseries (and exog data) into train and test set
    y_train, y_test = timeseries[:-forecast_length], timeseries[-forecast_length:]
    if exog_data is not None:
        exog_train, exog_test = exog_data[:-forecast_length], exog_data[-forecast_length:]

    # create and train SARIMA(X) model according to provided parameters
    if exog_data is not None:
        model = ARIMA(y_train, exog=exog_train, **model_params['arima'])
    else:
        model = ARIMA(y_train, **model_params['arima'])
    model_fitted = model.fit(**fit_params)

    # extract relevant parameters of fitted model
    aic = model_fitted.aic
    fitted_params = model_fitted.params

    # make predictions of length forecast_length
    if exog_data is not None:
        predictions = model_fitted.forecast(forecast_length, exog=exog_test)
    else:
        predictions = model_fitted.forecast(forecast_length)

    # construct prediction DataFrame
    predictions = pd.DataFrame(predictions.values, index=y_test.index, columns=['SARIMAX forecast'])

    # calculate error metrics of prediction
    rmse = math.sqrt(mean_squared_error(y_test, predictions))   # root mean square error: mean performance measure
    max_err = max(abs(y_test.values - predictions.values))[0]   # maximum error: worst performance measure

    if save_model:
        # save the fitted model by serializing with Pickle
        fstr = str(model_params['fourier']).replace(', ', '_')
        arstr = (str(model_params['arima']['order']) + 'x' + str(model_params['arima']['seasonal_order'])).replace(', ', '_')
        # extract last date considered in fitting the model
        last_date = model_fitted.data.dates[-1].strftime('%Y%m%d_%H%M')
        root = Path(__file__).parent
        model1 = '..\\..\\data\\models\\timeseries\\SARIMAX\\' + model_name + '_' + arstr + '_f_' + fstr + '_' + last_date +'_.sav'
        #model2 = '..\\..\\data\\models\\timeseries\\SARIMAX\\HeatLoad_' + arstr + '_f_' + fstr + '_' + last_date + 'small_.sav'
        model_fitted.save(os.path.join(root, model1))
        #model_fitted.save(os.path.join(root, model2), remove_data=True)

    if return_model:
        return predictions, aic, rmse, max_err, fitted_params, model_fitted
    else:
        return predictions, aic, rmse, max_err, fitted_params, None


def score_model(cfg):
    """
    Evaluates SARIMAX model with the configuration 'cfg' on multiple subsets of the time series

    Remaining parameters to call function 'create_model_and_validate' (timeseries, forecast_length, model_params,
    fit_params, exog_data) need to be specified as global variables

    :param np.array cfg: model configuration as [Fourier parameters (seasonal period, number of Fourier terms),
                                                 ARIMA order (p,d,q),
                                                 Seasonal ARIMA order (P,D,Q,m),
                                                 Data history parameters (length of individual subsets, # of subsets)]
    :returns tuple: (p, d, q)
    :returns tuple: (P, D, Q, m)
    :returns tuple: Fourier parameters
    :returns float: average RMSE
    :returns float: average AIC
    """

    # incorporate configuration provided in cfg into model parameters
    model_params = {'fourier': (cfg[0], cfg[1]),
                    'arima': {'order': (cfg[2], cfg[3], cfg[4])}
                    }
    # only include 'seasonal_order' if short term seasonality is provided, i.e. cfg[8] != 0
    if cfg[8] != 0:
        model_params['arima']['seasonal_order'] = (cfg[5], cfg[6], cfg[7], cfg[8])

    # derive indices of time series subsets to evaluate
    if days_glob is None:
        # extract 'random' subsets if no particular dates are specified in 'days_glob'
        # latest start of subset to still accommodate forecast length & specified data history length
        latest_start = len(timeseries_glob) - (cfg[8] + cfg[9])
        # extract several random starting points
        start = create_sobol_points(cfg[10], 1, latest_start)[:, 0]    # quasi-random Sobol sampling
        #start = np.random.randint(0, int(latest_start), int(cfg[10]))       # pseudo-random sampling
        # add end points for each starting point
        end = start + (cfg[8] + cfg[9])
    else:
        # use particularly specified 'days_glob' for random subsets
        start, end = [], []
        for day in days_glob:
            end.append(timeseries_glob.index.get_loc(day).stop)
            start.append(timeseries_glob.index.get_loc(day).start)
        start = [s - cfg[9] for s in start]

    # potentially featurize eXogeneous regressor data with further Fourier terms for longer-term seasonalities before
    # entering evaluation loop for individual subsets to ensure CONSISTENT FEATURIZATION across individual subsets
    # (only if long seasonality is relevant, i.e. model_params['fourier'][0] >0, and # Fourier terms > 0)
    if model_params['fourier'][0] > 0 and model_params['fourier'][1] > 0:
        _, exog_data = FourierFeaturizer(*model_params['fourier']).fit_transform(timeseries_glob)   # * to unpack tuple
        exog_data.set_index(timeseries_glob.index, inplace=True)
        # incorporate additional exogenous regressor data(if applicable)
        if exog_data_glob is not None:
            exog_data = pd.concat([exog_data_glob, exog_data], axis=1)
    else:
        exog_data = exog_data_glob

    # create 'local' fitting parameters - deepcopy required to avoid manipulation of global specification by ARIMA.fit()
    fitting_params = copy.deepcopy(fit_params_glob)

    # loop over number of specified history subsets to evaluate and average prediction errors --> fit and evaluate
    # model with configuration 'model_params' to X different historical subsets
    AIC = []
    RMSE = []
    max_err, MAX_ERR = -1, -1
    for run in range(cfg[10]):
        try:
            # never show warnings during hyperparameter screening
            with catch_warnings():
                filterwarnings('ignore')
                # call "create_model_and_validate" with global variables (except for "model_params")
                if exog_data is None:
                    _, aic, rmse, max_err, fitted_params, _ = create_model_and_validate(timeseries_glob[start[run]:end[run]],
                                                                        forecast_length_glob, model_params,
                                                                        fitting_params)
                else:
                    _, aic, rmse, max_err, fitted_params, _ = create_model_and_validate(timeseries_glob[start[run]:end[run]],
                                                                        forecast_length_glob, model_params, fitting_params,
                                                                        exog_data[start[run]:end[run]])
            # re-initialise fitting parameters for next run (potentially manipulated by ARIMA.fit)
            fitting_params = copy.deepcopy(fit_params_glob)
            # assign derived model parameters as starting parameters for next run/subset - NOT POSSIBLE for some
            # combinations of fitting parameters, e.g. method=innovations_mle
            fitting_params['start_params'] = fitted_params.values
        except:
            aic, rmse = np.nan, np.nan
            # re-initialise fitting parameters for next run (potentially manipulated by ARIMA.fit)
            fitting_params = copy.deepcopy(fit_params_glob)

        # add aic and rmse of individual forecast to overall AIC and RMSE for model configuration
        AIC.append(aic)
        RMSE.append(rmse)
        if max_err > MAX_ERR:
            MAX_ERR = max_err

    # calculate average AIC and RMSE (of all non-NaN elements)
    if np.isnan(AIC).all():
        AIC, RMSE = None, None
    else:
        AIC = round(np.nanmean(np.array(AIC)), 2)
        RMSE = round(np.nanmean(np.array(RMSE)), 2)

    # print each model validation (add empty 'seasonal_order' if not part of model_params)
    if 'seasonal_order' not in model_params['arima'].keys():
        model_params['arima']['seasonal_order'] = ()

    print('Order: {}     Seasonal: {}     Fourier: {}     RMSE: {:.2f}     MaxError: {:.2f}'.format(
          model_params['arima']['order'], model_params['arima']['seasonal_order'], model_params['fourier'],
          RMSE, MAX_ERR))

    return (model_params['arima']['order'], model_params['arima']['seasonal_order'], model_params['fourier'],
            AIC, RMSE, MAX_ERR)


def evaluate_models(timeseries, forecast_length, fit_params, cfg_list, fname, exog_data=None, days=None,
                    parallel=False, cpus=1):
    """
    Evaluates ALL SARIMAX models with configurations in 'cfg_list' and returns sorted list with ascending RMSE

    :param pd.DataFrame timeseries: time series for which SARIMAX model shall be derived
    :param int forecast_length: length of forecast / number of timesteps
    :param dict fit_params: dictionary providing fitting parameters passed to statsmodels ARIMA fit function
    :param np.array cfg_list: array of model configurations to evaluate [fourier, order, seasonal_order, data history]
    :param str fname: file name for consolidated results csv file to be saved
    :param pd.DataFrame exog_data: time series of potential eXogeneous regressor data
    :param list days: list of particular days, which shall be used for in-sample predictions while model screening
    :param bool parallel: boolean flag to indicate whether parallelizaton shall be used or not
    :paral int cpus: number of cpus to use for multiprocessing
    :returns scores: sorted list of all model configurations with ascending AIC
    """

    # execute hyperparameter estimation in parallel on specified number of cpus
    if parallel:
        # Step 1: Initialize multiprocessing.Pool() with number of cores to use and global static variables to increase
        # performance - initializing constant inputs as global variables reduces communication between parent and child
        # processes significantly
        with mp.Pool(cpus, initializer=init_pool, initargs=(timeseries, forecast_length, fit_params, exog_data, days)) as pool:
            # Step 2: Run jobs synchronously with `pool.map` and 'cfg' as "only" communicated variable
            scores = pool.map(score_model, cfg_list)
            # Step 3: Close pool to return results
            pool.close()    # tells the pool not to accept any new job
            pool.join()     # tells the pool to wait until all jobs finished then exit

    # execute hyperparameter estimation on single cpu
    else:
        # initialize global variables for "score model" function (only necessary, since function has been implemented
        # with cfg as only variable left to allow for more efficient parallel processing)
        init_pool(timeseries, forecast_length, fit_params, exog_data, days)
        scores = [score_model(cfg) for cfg in cfg_list]

    # save results to csv (incl. invalid models with AIC/RMSE: None, MaxError: -1)
    freq = timeseries.index.inferred_freq
    root = Path(__file__).parent
    file = '..\\..\\data\\models\\timeseries\\SARIMAX'
    file = os.path.join(root, file, fname + '_freq_1' + freq + '.csv')
    pd.DataFrame(scores, columns=['Order', 'Seasonal order', 'Fourier', 'AIC', 'RMSE', 'MaxError']).to_csv(file)

    # remove empty RMSE results (second last column)
    scores = [r for r in scores if r[-2] is not None]
    # sort configs in scores by ascending RMSE values
    scores.sort(key=lambda tup: tup[-2])
    return scores


def init_pool(ts, fc, fit_pars, exog, days):
    """
    Initialize multiprocessing pool by declaring all constant variables to "score_model" function as global, to
    minimize communication between parent and child processes and speed up performance
    """
    global timeseries_glob, forecast_length_glob, fit_params_glob, exog_data_glob, days_glob
    timeseries_glob = ts
    forecast_length_glob = fc
    fit_params_glob = fit_pars
    exog_data_glob = exog
    days_glob = days


####################     MAIN     ####################

if __name__ == '__main__':

    ####################   INITIALIZE PARAMETERS   ####################

    # specify which variable to forecast ['Waermeeinspeisung (MW)', 'Temp Vorlauf (degC)', 'Temp Ruecklauf (degC)',
    #                                     'MHKW Temp Vorlauf (degC)', 'MHKW Temp Ruecklauf (degC)']
    var = 'MHKW Temp Vorlauf (degC)'
    # specify potential eXogeneous regressor variable, e.g. ['Aussentemperatur (degC)']; [] for no external regressor
    regr = ['Waermeeinspeisung (MW)', 'Aussentemperatur (degC)']

    interval = '1H'             # specify time series frequency: '1H', '1D', '1M'
    duration = 24               # define number of periods to be forecasted
    season = [24, False]        # seasonality (number of periods within one seasonality cycle):
                                # [SHORT-TERM (model by arima model), LONG-TERM (modelled by Fourier terms)]
                                # 'False' if n/a, e.g. [24, False] if no long-term seasonality relevant
    seasons_to_use = 14         # length (number of short-term seasons) of data history to use
    subsets = 75                # number of subsets of historic data to evaluate per model (in case 'days' are provided
                                # and subsets < len(days): first 'subsets' entries in days are evaluated)

    # specify configurations to evaluate: ['Sobol', 'Predefined'] --> 'Sobol': evaluate random SARIMA configurations
    # with given specifications, 'Predefined': evaluate specific pre-defined SARIMA configurations from csv file
    configurations = 'Sobol'
    # number of models to evaluate (only relevant for Sobol screening)
    n = 100

    # define whether forecasts shall be evaluated for particular days or randomly selected days (if days=None)
    days = None
    #days = ['2020-01-10', '2020-02-10', '2020-03-10', '2020-04-10', '2020-05-10', '2020-06-10', '2020-07-10',
    #        '2020-08-10', '2020-09-10', '2020-10-10', '2020-11-10', '2020-12-10']
    days = select_days('2018-01-01', '2020-12-20', subsets)

    # define optional fitting parameter
    # fitting_params = {'method': 'innovations_mle',
    #                   'low_memory': True,      # do NOT return output from Kalman smoother/in-sample confidence intervals
    #                   'cov_type': 'none',      # do NOT return standard errors/t-stats/p-values of model parameters
    #                   'gls_kwargs': {'max_iter': 200},
    #                   'method_kwargs': {'minimize_kwargs': {
    #                        # determine which solver from `scipy.minimize` is used, e.g., 'nelder-mead', 'powell'
    #                        'method': 'powell',
    #                        'options': {'maxiter': 200}}}}
    fitting_params = {'method': 'statespace',
                      'low_memory': True,      # do NOT return output from Kalman smoother/in-sample confidence intervals
                      'cov_type': 'none',      # do NOT return standard errors/t-stats/p-values of model parameters
                      'method_kwargs': {'minimize_kwargs': {
                          # determine which solver from `scipy.minimize` is used, e.g., 'nelder-mead', 'powell'
                          'method': 'powell',
                          'options': {'maxiter': 200}}}}

    ####################   TIME SERIES LOADING, ANALYSIS, AND CONDITIONING   ####################

    # load fully conditioned historical time series data
    root = Path(__file__).parent
    ts_data = '..\\..\\data\\input\\processed\\fully_conditioned_timeseries.csv'
    ts = pd.read_csv(os.path.join(root, ts_data), index_col='Date', parse_dates=True, dayfirst=True)
    # order columns
    ts = ts[['Waermeeinspeisung (MW)', 'Temp Vorlauf (degC)', 'Temp Ruecklauf (degC)', 'MHKW Temp Vorlauf (degC)',
             'MHKW Temp Ruecklauf (degC)', 'Aussentemperatur (degC)']]
    ts.index.freq = ts.index.inferred_freq

    # plot heat load and ambient temperature data for selected days used for coarse Sobol model screening - to ensure
    # that selected days are a representative subset for model evaluations
    #plot_screening_days(ts['Aussentemperatur (degC)'], ts['Waermeeinspeisung (MW)'], days)

    # plot ACF and PACF plot (with various orders of differencing)
    #plot_series_acf_pacf(ts[var], max_diff=2, max_sdiff=1, season=season[0])

    # condition time series data and add covariates/eXogenous regressors
    if interval == '1M':
        # resample time series to monthly data
        ts = ts.resample('1M').mean()
        # split time series DataFrame into forecast variable and eXogenous regressors (if applicable)
        data = ts.loc[:, [var]]
        data.dropna(inlace=True)
        if len(regr) == 0:
            regr = None
        else:
            regr = ts.loc[:, regr]
            regr = regr.loc[data.index]
    elif interval == '1D':
        # resample time series to daily data
        ts = ts.resample('1D').mean()
        # split time series DataFrame into forecast variable and eXogenous regressors (if applicable)
        data = ts.loc[:, [var]]
        data.dropna(inlace=True)
        if len(regr) == 0:
            regr = None
        else:
            regr = ts.loc[:, regr]
            regr = regr.loc[data.index]
            # enrich eXogenous regressor with data about day of week, holidays, school vacation
            regr = add_daycharacteristics(regr)
    elif interval == '1H':
        # split time series DataFrame into forecast variable and eXogenous regressors (if applicable)
        data = ts.loc[:, [var]]
        data.dropna(inplace=True)
        if len(regr) == 0:
            regr = None
        else:
            regr = ts.loc[:, regr]
            regr = regr.loc[data.index]
            # enrich eXogenous regressor with data about day of week, holidays, school vacation
            #regr = add_daycharacteristics(regr)

    ####################   MODEL HYPERPARAMETER SCREENING   ####################

    # define ARIMA model configurations
    if configurations == 'Sobol':

        if var == 'Waermeeinspeisung (MW)':
            filename = 'Heatload_Sobol_hyperparameter_screening'
            modelname = 'Heatload'
        elif var == 'Temp Vorlauf (degC)':
            filename = 'FlowTemp_Sobol_hyperparameter_screening'
            modelname = 'FlowTemp'
        elif var == 'Temp Ruecklauf (degC)':
            filename = 'ReturnTemp_Sobol_hyperparameter_screening'
            modelname = 'ReturnTemp'
        elif var == 'MHKW Temp Vorlauf (degC)':
            filename = 'MHKW_FlowTemp_Sobol_hyperparameter_screening'
            modelname = 'MHKW_FlowTemp'
        elif var == 'MHKW Temp Ruecklauf (degC)':
            filename = 'MHKW_ReturnTemp_Sobol_hyperparameter_screening'
            modelname = 'MHKW_ReturnTemp'


        # define boundaries for hyperparameter fitting
        max_fourier = (0,)          # maximum number of Fourier terms/pairs
        max_pdq = (2, 1, 2)         # maximum orders of non-seasonal ARIMA parameters
        max_PDQ = (2, 1, 2)         # maximum orders of seasonal ARIMA parameters

        # create list of all configurations to be evaluated during hyperparameter fitting
        orders = create_sobol_points(n, len(max_pdq + max_PDQ), max_pdq + max_PDQ)      # (p, d, q, P, D, Q)
        fourier = create_sobol_points(n, len(max_fourier), max_fourier)
        # sort models by ascending total complexity (sum of ARIMA orders) - in order to compute 'easier' models first
        configs = np.array(sorted(orders, key=lambda row: sum(row)))
        # create consolidated model input configuration: (long_season, fourier_order, p, d, q, P, D, Q, m)
        short_seasonality = np.array([season[0] for i in range(n)]).reshape(n, 1)   # [False or value]
        long_seasonality = np.array([season[1] for i in range(n)]).reshape(n, 1)    # [False or value]
        configs = np.concatenate((long_seasonality, fourier, configs, short_seasonality), axis=1)
        # final layout: [Fourier seasonality, # Fourier terms, p, d, q, P, D, Q, m]

    elif configurations == 'Predefined':

        if var == 'Waermeeinspeisung (MW)':
            filename = 'Heatload_shortlisted_models_screening'
            modelname = 'Heatload'
            conf = '..\\..\\data\\models\\timeseries\\SARIMAX\\Heatload_Sobol_hyperparameter_screening_freq_1H_shortlist.csv'
        elif var == 'Temp Vorlauf (degC)':
            filename = 'FlowTemp_shortlisted_models_screening'
            modelname = 'FlowTemp'
            conf = '..\\..\\data\\models\\timeseries\\SARIMAX\\FlowTemp_Sobol_hyperparameter_screening_freq_1H_shortlist.csv'
        elif var == 'Temp Ruecklauf (degC)':
            filename = 'ReturnTemp_shortlisted_models_screening'
            modelname = 'ReturnTemp'
            conf = '..\\..\\data\\models\\timeseries\\SARIMAX\\ReturnTemp_Sobol_hyperparameter_screening_freq_1H_shortlist.csv'
        elif var == 'MHKW Temp Vorlauf (degC)':
            filename = 'MHKW_FlowTemp_shortlisted_models_screening'
            modelname = 'MHKW_FlowTemp'
            conf = '..\\..\\data\\models\\timeseries\\SARIMAX\\MHKW_FlowTemp_Sobol_hyperparameter_screening_freq_1H_shortlist.csv'
        elif var == 'MHKW Temp Ruecklauf (degC)':
            filename = 'MHKW_ReturnTemp_shortlisted_models_screening'
            modelname = 'MHKW_ReturnTemp'
            conf = '..\\..\\data\\models\\timeseries\\SARIMAX\\MHKW_ReturnTemp_Sobol_hyperparameter_screening_freq_1H_shortlist.csv'

        # load configurations from csv (e.g., from prior Sobol screening)
        conf = pd.read_csv(os.path.join(root, conf), index_col=None)
        n = len(conf)
        # extract order values from string representation in csv
        order = conf['Order'].apply(lambda row: [int(s) for s in re.findall(r'\d+', row)])
        order = [i for i in order.values]
        order = np.array(order).reshape(n, -1)
        seasonal_order = conf['Seasonal order'].apply(lambda row: [int(s) for s in re.findall(r'\d+', row)])
        seasonal_order = [i for i in seasonal_order.values]
        seasonal_order = np.array(seasonal_order).reshape(n, -1)
        fourier = conf['Fourier'].apply(lambda row: [int(s) for s in re.findall(r'\d+', row)])
        fourier = [i for i in fourier.values]
        fourier = np.array(fourier).reshape(n, -1)
        # consolidate configurations
        configs = np.concatenate((fourier, order, seasonal_order), axis=1)
        # final layout: [Fourier seasonality, # Fourier terms, p, d, q, P, D, Q, m]

    # add information about data history to use
    smallest_season = min(s for s in season if s > 0)    # avoid potential 'False' entries to be interpreted as 0
    history = np.array([[round(smallest_season * seasons_to_use), subsets] for i in range(n)]).reshape(n, 2)
    configs = np.concatenate((configs, history), axis=1).astype(int)

    print('\n ##########   Model screening   ########## \n')

    print('Start: ' + time.asctime())

    # evaluate all SARIMAX models defined in configs
    models = evaluate_models(data, duration, fitting_params, configs, filename, exog_data=regr, days=days,
                             parallel=True, cpus=4)

    print('End: ' + time.asctime())

    # extract model parameters for best fitting SARIMAX model
    model_params = {'arima': {'order': models[0][0], 'seasonal_order': models[0][1]}, 'fourier': models[0][2]}

    print('\n ##########   Best fitting model   ########## \n')

    print('SARIMAX model: {}x{}     Fourier terms: {}\n'.format(model_params['arima']['order'],
                                                               model_params['arima']['seasonal_order'],
                                                               model_params['fourier']))

    ####################   EVALUATE BEST FITTING MODEL   ####################

    # test one specific model configuration
    #model_params = {'arima': {'order': (2, 1, 1), 'seasonal_order': (1, 0, 1, 24)}, 'fourier': (0, 0)}

    # create copy of original time series for ARBITRARILY selected day to be forecasted
    fc = '2020-12-26'
    series = data.copy()
    end = series.index.get_loc(fc).stop
    start = end - (smallest_season * seasons_to_use + duration)
    series = series.iloc[start:end]

    # remove potentially empty seasonal_order (no seasonal order available, if no short-term seasonality specified)
    if model_params['arima']['seasonal_order'] == ():
        model_params['arima'].pop('seasonal_order')
    # add Fourier featurizer (in CONSISTENT manner as it was applied while screening the models)
    if model_params['fourier'][0] > 0 and model_params['fourier'][1] > 0:
        _, exog_data = FourierFeaturizer(*model_params['fourier']).fit_transform(data)   # * to unpack tuple
        exog_data.set_index(data.index, inplace=True)
        # incorporate additional exogenous regressor data(if applicable)
        if regr is not None:
            exog_data = pd.concat([regr, exog_data], axis=1)
    else:
        exog_data = regr
    # extract only relevant part of eXogeneous regressor data
    if exog_data is not None:
        exog_data = exog_data.loc[series.index]

    # fit SARIMA(X) model
    _, _, rmse, max_error, _, sarimax = create_model_and_validate(series, duration, model_params,
                                                                  fitting_params, exog_data=exog_data,
                                                                  return_model=True, save_model=True,
                                                                  model_name=modelname)

    # plot final model diagnostics / residual errors
    sarimax.plot_diagnostics(lags=min(50, sarimax.nobs), figsize=(16, 9))
    plt.show()

    # plot in-sample forecast including confidence intervals --> get_forecast method required to get confidence interval
    if exog_data is None:
        res = sarimax.get_forecast(duration)
    else:
        res = sarimax.get_forecast(duration, exog=exog_data.loc[series.index][-duration:])
    series = pd.concat([series, res.predicted_mean.rename('SARIMAX forecast')], axis=1)
    # retrieve (1-alpha)% confidence interval
    alpha = 0.1
    confint = res.conf_int(alpha=alpha)
    # plot in-sample forecast
    plt.rcParams['font.size'] = 14
    f, ax = plt.subplots(figsize=(16, 9))
    ax.grid('both')
    ax.plot(series[round(-2*duration):], '.--', lw=2.0)
    ax.set_ylabel('(Forecasted) value')
    #plt.xticks(rotation=45)
    ax.fill_between(confint.index, confint.iloc[:, 0], confint.iloc[:, 1], color='k', alpha=.15)
    ax.set_xlabel('Date')
    leg = list(series.columns.values)
    leg.append('%i%% confidence interval' % ((1-alpha)*100))
    ax.legend(leg, loc='upper left')
    f.suptitle(var)
    plt.show(), plt.close()

    # Print out model errors
    print('Best SARIMAX model test RMSE: %.3f' % rmse)
    print('Best SARIMAX model test MaxError: %.3f' % max_error)
    print('#####   end   #####')


