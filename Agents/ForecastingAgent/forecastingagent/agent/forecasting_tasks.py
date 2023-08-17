################################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk)   #
#          Markus Hofmeister (mh807@cam.ac.uk) # 
# Date: 30 Nov 2022                            #
################################################

# This module represents the main forecasting agent to forecast a time series using 
# a trained model or Prophet (as default). The forecast is then stored in the KG.

import pandas as pd

from darts import TimeSeries
from darts.models import Prophet, TFTModel
from darts.dataprocessing.transformers import Scaler
from darts.metrics import mape, mse, rmse, smape

from py4jps import agentlogging

from forecastingagent.fcmodels import FC_MODELS
from forecastingagent.datamodel.iris import *
from forecastingagent.utils.tools import *
from forecastingagent.utils.env_configs import SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT, \
                                               DB_USER, DB_PASSWORD
from forecastingagent.agent.forcasting_config import *
from forecastingagent.kgutils.kgclient import KGClient
from forecastingagent.kgutils.tsclient import TSClient


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


def forecast(config, db_url, time_format, kgClient=None, tsClient=None,
             query_endpoint=SPARQL_QUERY_ENDPOINT, update_endpoint=SPARQL_UPDATE_ENDPOINT,
             db_user=DB_USER, db_password=DB_PASSWORD):
    """
    Forecasts a time series using a pre-trained model or Prophet using darts.

    Arguments:
        #TODO: update
        kgClient {KGClient} - KG client to use
        time_format {str} - time format to use to initialise the forecast TimeSeries
        query_endpoint {str} - endpoint to query the KG
        update_endpoint {str} - endpoint to update the KG
        db_url {str} - URL to the RDB
        db_user {str} - username for the RDB
        db_password {str} - password for the RDB
    
    Returns:
        darts.TimeSeries object with forecasted time series
    """

    # Initialise the KG and TS clients
    if not kgClient:
        kgClient = KGClient(query_endpoint=query_endpoint, update_endpoint=update_endpoint)
    if not tsClient:
        tsClient = TSClient(kg_client=kgClient, rdb_url=db_url, rdb_user=db_user, 
                            rdb_password=db_password)

    # Update/condition forecasting configuration
    cfg = config.copy()
    
    # Calculate lower and upper bound for time series to speed up TSClient queries
    # (i.e., only query the time series data that is needed)
    lowerbound, upperbound = get_ts_lower_upper_bound(cfg, time_format=time_format)
    cfg['loaded_data_bounds'] = {
        'lowerbound': lowerbound, 
        'upperbound': upperbound
        }
    
    # Log some information before calculating forecast
    logger.info(f'Creating forecast for: {cfg["iri_to_forecast"]}')
    logger.info(f'Forecast start timestamp: {cfg["fc_start_timestamp"]}')
    logger.info(f'Loaded time bounds for forecast: {cfg["loaded_data_bounds"]}')
    
    # Load time series (and covariate) data
    series, covariates = load_ts_data(cfg, kgClient, tsClient)

    # Verify that forecast start time is in series ...
    if cfg['fc_start_timestamp'] in series.time_index:
        series, _ = series.split_before(cfg['fc_start_timestamp'])
        logger.info('Forecast start time is in series. Splitting series at forecast start time.')
    # ... or the next date (then keep whole series) ...
    elif cfg['fc_start_timestamp'] == series.time_index[-1] + series.freq:
        logger.info('Forecast start time immediately follows the last entry of the series. Keeping whole series.')
    # ... timestamp out of series range
    else:
        msg = 'Forecast start date is out of series range. '
        msg += f'fc_start_timestamp: {cfg["fc_start_timestamp"]} - out of range of series with start {series.start_time()} and end {series.end_time()}.'
        logger.error(msg)
        raise ValueError(msg)

    # Load the model
    # 1) Use default prophet model ...
    if cfg['fc_model'].get('name') == 'prophet':
        logger.info('Using default Prophet model to forecast.')
        model = Prophet()
        cfg['fc_model']['input_length'] = len(series) 

    else:
        # 2) ... or load pre-trained custom model
        mapped_model = FC_MODELS.get(cfg['fc_model'].get('name'))
        if mapped_model:
            logger.info('Loading (pre-trained) custom forecasting model ...')
            cfg, model = mapped_model[0](cfg, series)
            logger.info('Custom forecasting model loaded successfully.')
        else:
            msg = 'No model loading function has been provided for the specified forecasting model.'
            logger.error(msg)
            raise ValueError(msg)

    # Create forecast time series
    forecast = get_forecast(series, covariates, model, cfg)

    return forecast


def get_forecast(series, covariates, model, cfg):
    """
    It takes a series, covariates, model, and a model configuration as inputs, 
    and returns a forecast.

    Arguments:
        series: the time series data
        covariates: darts series  of covariates
        model: the model to use for forecasting
        cfg: a dictionary of forecast characteristics as created by
             'create_forecast_configuration'
    Returns:
        darts.TimeSeries object with forecasted time series
    """

    if cfg['fc_model'].get('scale_data'):
        # Neural methods perform better with scaled inputs
        msg = f'Scaling input data with data length of {len(series)} timesteps.'
        msg += 'Make sure that series is long enough (controlled with "data_length") to '
        msg += 'represent the whole data distribution, i.e., at least one full cycle.'
        logger.info(msg)
        scaler = Scaler()
        series = scaler.fit_transform(series)

    #model.__class__.__name__ =='Prophet'

    # Make forecast with covariates
    if covariates is not None:
        if cfg['fc_model'].get('train_again'):
            logger.info('Training model again with covariates.')
            # Train again with covariates
            model.fit(series, future_covariates=covariates)
        logger.info('Making forecast with covariates.')
        try:
            forecast = model.predict(
                n=cfg['horizon'], future_covariates=covariates, series=series)
        except RuntimeError as e:
            # Prediction sometimes fails due to wrong dtype -> convert to same dtype
            series = series.astype('float32')
            covariates = covariates.astype('float32')
            forecast = model.predict(
                n=cfg['horizon'], future_covariates=covariates, series=series)

    # Make prediction without covariates (e.g. default for Prophet)
    else:
        if cfg['fc_model'].get('train_again'):
            logger.info('Training model again without covariates.')
            model.fit(series)
        logger.info('Making forecast without covariates.')
        # NOTE: 
        forecast = model.predict(n=cfg['horizon'])

    if cfg['fc_model'].get('scale_data'):
        # Scale predicted data back
        logger.info('Scaling back predicted data, i.e., back-transform into actual values.')
        forecast = scaler.inverse_transform(forecast)

    return forecast


def load_ts_data(cfg, kgClient, tsClient):
    """
    Load time series and covariates data (if applicable) from RDB. 

    Arguments:
        cfg: dicti.onary with forecast configuration as returned by 'create_forecast_configuration'
            Relevant keys:
                'fc_model': configuration of forecasting model to use, i.e. relevant covariates
                'dataIRI': dataIRI of the time series to forecast (i.e., for which to retrieve data)
                'loaded_data_bounds': lowerbound and upperbound between which to load data
                'resample_data': string describing potential resampling frequency (for pandas resample function)
        kgClient: initialised KG client
        tsClient: initialised TS client
    
    Returns:
        darts.TimeSeries objects of timeseries (and covariates)
    """

    if cfg['fc_model'].get('covariate_iris'):
        # Load covariates data
        # TODO to be reworked for TFT & others
        logger.info('Loading covariates with function: ' + cfg['load_covariates_func'].__name__)
        covariates_iris, covariates = cfg['load_covariates_func'](
            kgClient, tsClient, cfg['loaded_data_bounds']['lowerbound'], cfg['loaded_data_bounds']['upperbound'])
        cfg['fc_model']['covariates_iris'] = covariates_iris

        # check if covariates are given for complete future horizon from fc_start_timestamp
        check_if_enough_covs_exist(cfg, covariates)
    else:
        logger.info('No covariates specified by forecasting model, no covariates are loaded.')
        covariates = None

    # Load timeseries to be forecasted as DataFrame
    df = get_df_of_ts(cfg['dataIRI'], tsClient, cfg['loaded_data_bounds']['lowerbound'],
                      cfg['loaded_data_bounds']['upperbound'], column_name="Series", 
                      index="Time")

    # Convert DataFrame to darts.TimeSeries
    # Potentially resample time series (especially relevant for irregularly spaced 
    # data to avoid issues with Darts being unable to retrieve frequency of ts)
    if cfg.get('resample_data'):
        df_resampled = df.set_index('Time').copy()
        df_resampled = df_resampled.resample(cfg.get('resample_data')).mean()
        df = df_resampled.reset_index()

    try:
        # Build a deterministic TimeSeries instance from DataFrame as is
        series = TimeSeries.from_dataframe(df, time_col='Time', value_cols="Series")
    except ValueError:
        # Fill missing times with NaN values if processing DataFrame as is fails;
        # requires possibility to infer the frequency from the provided timestamps
        series = TimeSeries.from_dataframe(df, time_col='Time', value_cols="Series",
                                           fill_missing_dates=True, 
                                           freq=None)

    # Remove nan values at beginning and end
    series = series.strip()

    logger.info('Time series (and covariate) data successfully loaded.')
    return series, covariates


def check_if_enough_covs_exist(cfg, covariates):
    if covariates is not None and (cfg['fc_start_timestamp'] + cfg['frequency'] * (cfg['horizon'] - 1) > covariates.end_time()):
        logger.error(
            f'Not enough covariates exist for the given fc_start_timestamp and horizon. The last covariate timestamp is {covariates.end_time()}')
        raise ValueError(
            f'Not enough covariates for complete future horizon. Covariates end at {covariates.end_time()} but forecast horizon ends at {cfg["fc_start_timestamp"] + cfg["frequency"] * (cfg["horizon"] - 1)}')
    return True


def get_ts_lower_upper_bound(cfg, time_format=TIME_FORMAT):
    """
    It takes the forecast start date, the frequency, the horizon, and the data length, 
    and returns the lower and upper bounds of the time series

    Arguments:
        cfg: a dictionary with the following keys:
            'fc_start_timestamp': the start of the forecast (pd.Timestamp)
            'frequency': the frequency of the time series data (dt.timedelta)
            'horizon': the length of the forecast, i.e. number of time steps (int)
            'data_length': the length of the time series data to retrieve before the
                        fc_start_timestamp, i.e. number of time steps (int)

    Returns:
        Lower and upper bounds of the time series as strings
    """

    # upper bound is fc_start_timestamp + forecast horizon
    upperbound = cfg['fc_start_timestamp'] + \
                 cfg['frequency'] * (cfg['horizon'] - 1)

    # lower bound is fc_start_timestamp - (historical) data_length
    lowerbound = cfg['fc_start_timestamp'] - \
        cfg['frequency'] * (cfg['data_length'])
    
    return lowerbound.strftime(time_format), upperbound.strftime(time_format)


def calculate_error(target, forecast):
    """
    Calculate error metrics between two darts TimeSeries objects and 
    returns a dictionary with error metrics.

    Arguments:
        target {TimeSeries} -- actual (historical) time series
        forecast {TimeSeries} -- forecasted time series
    Returns:
        dict: dictionary with error metrics
    """

    error = {}
    try:
        error['mape'] = mape(target, forecast)
    except ValueError:
        # MAPE calcualtion failed due to zero values
        pass
    error['smape'] = smape(target, forecast)
    error['mse'] = mse(target, forecast)
    error['rmse'] = rmse(target, forecast)
    error['max_error'] = max_error(target, forecast)
    return error
