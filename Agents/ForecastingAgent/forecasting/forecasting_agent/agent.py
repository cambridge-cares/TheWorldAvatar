################################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk)   #
# Date: 22 Nov 2022                            #
################################################

# The purpose of this module is to forecast a time series using a trained model or Prophet
from dateutil.parser import isoparse
import datetime as dt
import os
import time
import urllib
import uuid
from darts.metrics import mape, mase, mse, rmse, smape

import pandas as pd
from darts import TimeSeries
from darts.dataprocessing.transformers import Scaler
from darts.models import Prophet, TFTModel

from forecasting.datamodel.data_mapping import *
from forecasting.datamodel.iris import *
from forecasting.errorhandling.exceptions import KGException
from forecasting.kgutils.kgclient import KGClient
from forecasting.kgutils.tsclient import TSClient
from forecasting.utils.properties import *
from forecasting.utils.tools import *
from forecasting.utils.useful_queries import *

#import agentlogging
# Initialise logger
#logger = agentlogging.get_logger("prod")


def forecast(dataIRI, horizon, forecast_start_date=None, use_model_configuration=None, data_length=None):
    """
    Forecast a time series using a pre trained model or Prophet.
    returns a dictionary with the forecast and some metadata.
    
    :param dataIRI: The IRI of the time series you want to forecast
    :param horizon: The number of time steps to forecast
    :param forecast_start_date: The date from which you want to start forecasting
    :param use_model_configuration: If you want to force a specific model configuration, you can do so here
    :param data_length: The number of time steps which should be loaded from the DB
    :return: The forecast is being returned.
    """
    # initialise the  client
    kgClient = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)
    tsClient = TSClient(kg_client=kgClient)

    covariates = None

    # get the model configuration dictionary from the a specific use_model_configuration or use default
    if use_model_configuration is not None:
        model_configuration_name = use_model_configuration
    else:
        model_configuration_name = "DEFAULT"

    cfg = MODEL_MAPPING[model_configuration_name].copy()
    cfg['model_configuration_name'] = model_configuration_name
    cfg['dataIRI'] = dataIRI
    cfg['horizon'] = horizon
    if data_length is not None:
        cfg['data_length'] = data_length

    cfg['forecast_start_date'] = get_forecast_start_date(forecast_start_date, tsClient, cfg)

    # use model configuration function to get the correct dataIRI timeseries and its covariates
    series, covariates = load_ts_data(
        cfg, kgClient, tsClient)

    # split series at forecast_start_date
    # check if forecast_start_date is in series
    if cfg['forecast_start_date'] in series.time_index:
        series, backtest_series = series.split_before(
            cfg['forecast_start_date'])
    
    # or the next date 
    elif cfg['forecast_start_date'] == series.time_index[-1] + series.freq:
        series = series
        
    # Timestamp out of series range
    else:
        raise ValueError(
            f'Cannot split series at {cfg["forecast_start_date"]} - out of range of series start {series.start_time()} and end {series.end_time()}')

    # load the model
    # NOTE: If you have multiple different models, you need to edit here the loading function,
    # you can use the model_configuration_name to load the correct model or 
    # add a function to the model config like for loading the covariates
    if 'TFT_HEAT_SUPPLY' == cfg['model_configuration_name']:
        model = load_pretrained_model(
            cfg, TFTModel)
        # other models than TFT can have different key then 'input_chunk_length'
        cfg['fc_model']['input_length'] = model.model_params['input_chunk_length']

    elif 'DEFAULT' == cfg['model_configuration_name']:
        model = Prophet()
        cfg['fc_model']['input_length'] = len(series)
    else:
        raise ValueError(
            f'No model found for model_configuration_name {cfg["model_configuration_name"]}, use one of {MODEL_MAPPING.keys()}')
    
    forecast = get_forecast(series, covariates, model, cfg)
    
    # create metadata
    # input series range
    start_date = series.end_time() - series.freq * \
        (cfg['fc_model']['input_length'] - 1)
    end_date = series.end_time()

    cfg['model_input_interval'] = [start_date, end_date]
    cfg['model_output_interval'] = [forecast.start_time(), forecast.end_time()]
    cfg['created_at'] = pd.to_datetime('now', utc=True)

    # delete keys which you dont want in response, e.g. not json serializable objects
    keys_to_delete = ['load_covariates_func',
                      'time_delta', 'ts_data_type', 'frequency']
    for key in keys_to_delete:
        if key in cfg:
            del cfg[key]

    #instantiate_forecast(cfg = cfg, forecast=forecast, tsClient=tsClient, kgClient=kgClient)

    return cfg

def get_forecast_start_date(forecast_start_date, tsClient, cfg):
    if forecast_start_date is not None:
        return pd.Timestamp(
            isoparse(forecast_start_date)).tz_convert('UTC').tz_localize(None)
    else:
        # get the last value of ts and set next date as forecast start date
        latest = tsClient.tsclient.getLatestData(cfg['ts_iri'], tsClient.conn)
        return pd.Timestamp(isoparse(latest.getTimes(
        )[0].toString())).tz_convert('UTC').tz_localize(None) + cfg['frequency']


def get_forecast(series, covariates, model, cfg):
    """
    It takes a series, covariates, model, and a model configuration as inputs, and returns a forecast.
    
    :param series: the time series data
    :param covariates: darts series  of covariates
    :param model: the model to use for forecasting
    :param cfg: a dictionary containing the following keys:
        'fc_model': a dictionary containing the following keys:
            'input_length': the number of time steps to use as input for the model
    :return: The forecasted values
    """

    print(f"Forecasting with {cfg['fc_model']['name']} model")
    if cfg['fc_model']['scale_data']:
        # neural methods perform better with scaled inputs
        scaler = Scaler()
        series = scaler.fit_transform(series)

    # make forecast with covariates
    if covariates is not None:
        if cfg['fc_model']['train_again']:
            # TODO: add option to train model with covariates
            # train again with covariates
            model.fit(series, future_covariates=covariates)

        try:
            forecast = model.predict(
                n=cfg['horizon'], future_covariates=covariates, series=series)
        except RuntimeError as e:
            # prediction failed often, because of wrong dtype -> convert to same dtype
            series = series.astype('float32')
            covariates = covariates.astype('float32')
            forecast = model.predict(
                n=cfg['horizon'], future_covariates=covariates, series=series)

    # make prediction without covariates
    else:
        if cfg['fc_model']['train_again']:
            model.fit(series)

        forecast = model.predict(n=cfg['horizon'])

    if cfg['fc_model']['scale_data']:
        # scale back
        forecast = scaler.inverse_transform(forecast)

    return forecast


def calculate_error(target, forecast):
    """
    It takes two time series as input and returns a dictionary with error metrics.
    
    :param target: the actual values
    :param forecast: The forecasted values
    :return: A dictionary with error metrics
    """
    """Calculate error metrics between target and forecast

    Args:
        target (TimeSeries): target timeseries
        forecast (TimeSeries): forecast timeseries

    Returns:
        dict: dictionary with error metrics
    """
    error = {}
    try:
        error['mape'] = mape(target, forecast)
    except ValueError as e:
        # mape failed because of zero values
        pass
    error['smape'] = smape(target, forecast)
    error['mse'] = mse(target, forecast)
    error['rmse'] = rmse(target, forecast)
    return error


def load_ts_data(cfg, kgClient, tsClient):
    """
    Loads the time series data and covariates from the KG and TSDB. 
    Calculates the upper and lower bounds of the time series, based on the given data_length, forecast_start_date and horizon. 
    
    
    :param cfg: a dictionary with the model configuration with the following keys:
        'ts_data_type': the type of the time series data
        'frequency': the frequency of the time series data
        'data_length': the length of the time series data to retrieve before the forecast_start_date
        'forecast_start_date': the start date of the forecast, if None, the last date of the time series data is used
        'horizon': the length of the forecast
        'load_covariates_func': function to load covariates, if None, no covariates are loaded
    :param kgClient: a client for the knowledge graph
    :param tsClient: a client for the timeseries database
    :return: the timeseries and covariates.
    """

    #logger.info('Loading timeseries from KG')
    # get the data
    try:
        # try if ts hasValue where the actual ts is stored
        cfg['ts_iri'] = get_ts_value_iri(cfg['dataIRI'], kgClient)
    except IndexError as e:
        cfg['ts_iri'] = cfg['dataIRI']

    # calculate lower and upper bound for timeseries to speed up query
    lowerbound, upperbound = get_ts_lower_upper_bound(cfg)

    if 'load_covariates_func' in cfg:
        # load covariates
        covariates_iris, covariates = cfg['load_covariates_func'](
            kgClient, tsClient, lowerbound, upperbound)
        cfg['fc_model']['covariates_iris'] = covariates_iris

        # check if covariates are given for complete future horizon from forecast_start_date
        if covariates is not None and (cfg['forecast_start_date'] + cfg['frequency'] * (cfg['horizon'] - 1) > covariates.end_time()):
            # use default model
            print(f'\n\nNot enough covariates for complete future horizon.')
            
            raise ValueError(
                f'Not enough covariates for complete future horizon. Covariates end at {covariates.end_time()} but forecast horizon ends at {cfg["forecast_start_date"] + cfg["frequency"] * (cfg["horizon"] - 1)}')
    else:
        covariates = None

    # load timeseries which should be forecasted
    df = get_df_of_ts(cfg['ts_iri'], tsClient, lowerbound,
                      upperbound, column_name="Series", date_name="Date")

    # df to darts timeseries
    series = TimeSeries.from_dataframe(
        df, time_col='Date', value_cols="Series")  # , fill_missing_dates =True
    # remove nan values at beginning and end
    series = series.strip()

    cfg['loaded_data_bounds'] = {
        'lowerbound': lowerbound, 'upperbound': upperbound}

    print('Done with loading timeseries from KG and TSDB')
    return series, covariates


def get_ts_lower_upper_bound(cfg):
    """
    It takes the forecast start date, the frequency, the horizon, and the data length, and returns the
    lower and upper bounds of the time series
    
    :param cfg: a dictionary with the following keys:
        'forecast_start_date': the start date of the forecast
        'frequency': the frequency of the time series data
        'horizon': the length of the forecast
        'data_length': the length of the time series data to retrieve before the forecast_start_date
    :return: the lower and upper bounds of the time series
    """
    # upper bound is forecast_start_date + horizon
    upperbound = cfg['forecast_start_date'] + \
        cfg['frequency'] * (cfg['horizon'] - 1)

    # lower bound is forecast_start_date - input_length
    lowerbound = cfg['forecast_start_date'] - \
        cfg['frequency'] * (cfg['data_length'])
    return lowerbound.strftime(TIME_FORMAT), upperbound.strftime(TIME_FORMAT)


def load_pretrained_model(cfg, ModelClass, forece_download=False):
    """
    It downloads a model from a link, and then loads it into a Darts model
    
    :param cfg: a dictionary containing the configuration of the model
    :param ModelClass: the class of the model
    :param forece_download: If you want to download the model again if a folder already exists, set this to True, defaults to False
    (optional)
    :return: The model is being returned.
    """
    model_path_ckpt_link, model_path_pth_link = cfg['fc_model'][
        'model_path_ckpt_link'], cfg['fc_model']['model_path_pth_link']

    # try to load from checkpoint link
    path_ckpt = ""
    path_pth = ""
    path_to_store = Path(__file__).parent.absolute() / \
        'Models' / cfg['model_configuration_name'] / 'checkpoints'

    # TODO: until now we need to download both, checkpoint and model file
    # maybe you find a better way to just have one link

    if os.path.exists(path_to_store) and not forece_download:
        # model already exists
        path_ckpt = path_to_store / "best-model.ckpt"
        path_pth = ""
    else:
        # create folder
        os.makedirs(path_to_store)

        if model_path_ckpt_link.startswith("https://"):
            # download checkpoint model
            path_ckpt, cfg = urllib.request.urlretrieve(
                model_path_ckpt_link, path_to_store / "best-model.ckpt")
            print(
                f'Downloaded checkpoint model from {model_path_ckpt_link} to {path_ckpt}')

        if model_path_pth_link.startswith("https://"):
            # download model
            path_pth, cfg = urllib.request.urlretrieve(
                model_path_pth_link, path_to_store.parent.absolute() / "_model.pth.tar")
            print(f'Downloaded model from {model_path_pth_link} to {path_pth}')

    model = ModelClass.load_from_checkpoint(
        path_ckpt.parent.parent.__str__())
    cfg['fc_model']['name'] = model_path_ckpt_link.__str__()
    print(f'Loaded model from  {path_ckpt.parent.parent.__str__()}')

    # convert loaded model to device
    pl_trainer_kwargs = {"accelerator": 'cpu'}
    model.model_params['pl_trainer_kwargs'] = pl_trainer_kwargs
    model.trainer_params = pl_trainer_kwargs
    print(f'Moved model to device  {pl_trainer_kwargs["accelerator"]}')
    return model


def instantiate_forecast(cfg, forecast, tsClient, kgClient):
    """
    It takes a forecast object, a timeseries client and a knowledge graph client as input and
    instantiates the forecast in the knowledge graph.
    
    :param cfg: a dictionary with metainformation to be instantiated in the knowledge graph:
        'forecast_start_date': the start date of the forecast
        'frequency': the frequency of the time series data
        'horizon': the length of the forecast
        'data_length': the length of the time series data to retrieve before the forecast_start_date
        'ts_iri': the iri of the timeseries which is forecasted
        'fc_model': the configuration of the model
        'model_configuration_name': the name of the model configuration
        'loaded_data_bounds': the lower and upper bounds of the time series
        'forecast_name': the name of the forecast
    :param forecast: the forecast darts series object
    :param tsClient: a client for the timeseries database
    :param kgClient: a client to the knowledge graph
    """
    #  instantiate forecast in KG
    forecast_iri = KB + 'Forecast_' + str(uuid.uuid4())
    update = ""

    try:
        # get unit from dataIRI and add to forecast
        unit = {OM_HASUNIT: get_unit(cfg['dataIRI'], kgClient)}
        cfg['unit'] = unit[OM_HASUNIT]
    except KGException as e:
        # no measurement -> no unit
        unit = {}

    time_format = get_time_format(cfg['dataIRI'], kgClient)

    ONTOEMS_HASFORECASTINPUTLENGHT = ONTOEMS + "hasForecastInputLength"
    ONTOEMS_HASCOVARIATE = ONTOEMS + "hasCovariate"
    ONTOEMS_HASFORECASTMODEL = ONTOEMS + "hasForecastModel"
    covariate_update = {
        ONTOEMS_HASCOVARIATE: cfg['covariates_iris']} if cfg['covariates_iris'] else {}
    update += get_properties_for_subj(subj=forecast_iri, verb_obj={
        RDF_TYPE: ONTOEMS_FORECAST,
        **unit,
        **covariate_update,
    }, verb_literal={
        #ONTOEMS_HASFORECASTMODEL: cfg['fc_model']['name'],
        ONTOEMS_HASFORECASTINPUTLENGHT: cfg['forecast_input_length'],
    })

    # call client
    tsClient.tsclient.initTimeSeries([forecast_iri], [cfg['data_type']], time_format,
                                     tsClient.conn)
    ts = TSClient.create_timeseries([str(x) for x in forecast.time_index], [
                                    forecast_iri], [forecast.values().squeeze().tolist()])
    tsClient.tsclient.addTimeSeriesData(ts, tsClient.conn)

    update += get_properties_for_subj(subj=cfg['dataIRI'], verb_obj={
        ONTOEMS_HASFORECASTEDVALUE: forecast_iri})

    kgClient.performUpdate(add_insert_data(update))
    cfg['forecast_iri'] = forecast_iri

