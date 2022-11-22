################################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk) #
# Date: 31 Oct 2022                            #
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
#import agentlogging
from forecasting.datamodel.iris import *
from forecasting.errorhandling.exceptions import KGException
from forecasting.kgutils.kgclient import KGClient
#from forecasting.utils.api_endpoints import HM_SPARQL_ENDPOINT
#from forecasting.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT
from forecasting.kgutils.tsclient import TSClient
from forecasting.utils.properties import *
from forecasting.utils.tools import *
from forecasting.utils.useful_queries import *

# Initialise logger
#logger = agentlogging.get_logger("prod")


def forecast(dataIRI, horizon, forecast_start_date=None, force_configuration=None, data_length=None):
    """
    Forecast a time series using a pre trained model or Prophet.
    returns a dictionary with the forecast and some metadata.
    
    :param dataIRI: The IRI of the time series you want to forecast
    :param horizon: The number of time steps to forecast
    :param forecast_start_date: The date from which you want to start forecasting
    :param force_configuration: If you want to force a specific mapping, you can do so here
    :param data_length: The number of time steps which should be loaded from the DB
    :return: The forecast is being returned.
    """
    # initialise the  client
    kgClient = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)
    tsClient = TSClient(kg_client=kgClient)

    covariates = None

    # get the mapping dictionary either from the a specific force_configuration
    # or from identifying the dataIRI
    if force_configuration is not None:
        print(f'Using forced  mapping {force_configuration}')
        mapping_name = force_configuration
    else:
        mapping_name = get_config(dataIRI, kgClient)

    cfg = MAPPING[mapping_name].copy()
    cfg['mapping_name'] = mapping_name
    cfg['dataIRI'] = dataIRI
    cfg['horizon'] = horizon
    if data_length is not None:
        cfg['data_length'] = data_length
    print('Using mapping: ', cfg)

    if forecast_start_date is not None:
        cfg['forecast_start_date'] = pd.Timestamp(
            isoparse(forecast_start_date)).tz_convert('UTC').tz_localize(None)
    else:
        cfg['forecast_start_date'] = None

    # use mapping function to get the correct dataIRI timeseries and its covariates
    series, covariates = load_ts_data(
        cfg, kgClient, tsClient)

    # split series at forecast_start_date
    # if more time steps are available after forecast_start_date
    # backtest can be performed later
    try:
        series, backtest_series = series.split_before(
            cfg['forecast_start_date'])
    except ValueError as e:
        # Timestamp out of series range
        print(
            f'Cannot split series at {cfg["forecast_start_date"]} - out of range')
        backtest_series = None

    # load the model
    # TODO: If you have multiple models and you need different loading functions,
    # you can add them here. Maybe even add a function to the mapping dictionary
    if 'model_path_ckpt_link' in cfg['fc_model']:
        model = load_pretrained_model(
            cfg)
        # other models than TFT can have different key then 'input_chunk_length'
        cfg['fc_model']['input_length'] = model.model_params['input_chunk_length']

    else:
        model = Prophet()
        cfg['fc_model']['input_length'] = len(series)

    forecast = get_forecast(series, covariates, model, cfg)
    # metadata
    # input series range
    print(f"Input data range: {series.start_time()} - {series.end_time()}")
    start_date = series.end_time() - series.freq * \
        (cfg['fc_model']['input_length'] - 1)
    end_date = series.end_time()
    print(f"Model input range: {start_date} - {end_date}")
    print(
        f"Model output range: {forecast.start_time()} - {forecast.end_time()}")
    print(f'Done with forecast using  ')

    cfg['model_input_interval'] = [start_date, end_date]
    cfg['model_output_interval'] = [forecast.start_time(), forecast.end_time()]
    cfg['created_at'] = pd.to_datetime('now')

    # delete not json serializable objects from cfg
    keys_to_delete = ['load_covariates_func',
                      'time_delta', 'ts_data_type', 'frequency']
    for key in keys_to_delete:
        if key in cfg:
            del cfg[key]

    #cfg = {}

    #instantiate_forecast(cfg = cfg, forecast=forecast, tsClient=tsClient, kgClient=kgClient)

    if backtest_series is not None:
        # calculate error if future target is available
        cfg['error'] = calculate_error(backtest_series, forecast)
        print(cfg['error'])
    return cfg


def get_forecast(series, covariates, model, cfg):
    """
    It takes a series, covariates, model, and a mapping as inputs, and returns a forecast.
    
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
    
    
    :param cfg: a dictionary with the mapping with the following keys:
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

    if cfg['forecast_start_date'] is None:
        # get the last value of ts and set next date as forecast start date
        latest = tsClient.tsclient.getLatestData(cfg['ts_iri'], tsClient.conn)
        cfg['forecast_start_date'] = pd.Timestamp(isoparse(latest.getTimes(
        )[0].toString())).tz_convert('UTC').tz_localize(None) + cfg['frequency']

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
            print(
                f'\n\nNot enough covariates for complete future horizon. Default model is used instead.')
            cfg['fc_model'] = MAPPING['DEFAULT']['fc_model'].copy()
            covariates = None
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


def load_pretrained_model(cfg, forece_download=False):
    """
    It downloads a model from a link, and then loads it into a Darts model
    
    :param cfg: a dictionary containing the configuration of the model
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
        'Models' / cfg['mapping_name'] / 'checkpoints'

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

    # try to load model from downloaded checkpoint
    # model = TFTModel.load(
    #    path_pth.__str__())
    model = TFTModel.load_from_checkpoint(
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
        'mapping_name': the name of the mapping
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


def get_config(dataIRI, kgClient):
    """
    It takes a dataIRI and a kgClient as input and returns the name of the mapping function to be used by identifying the dataIRI in the KG.
    
    :param dataIRI: the IRI of the data to be mapped
    :param kgClient: the client for the knowledge graph
    :return: The name of the config to be used.
    """
    # identify the dataIRI for right mapping function

    # get properties of dataIRI and its neighbors
    predecessor_dict = get_predecessor_type_and_predicate(dataIRI, kgClient)

    # use properties to identify the right mapping function
    # TODO: specify identification in mapping functions
    # case 1 - heat supply data identified
    if OHN_HASHEATDEMAND in predecessor_dict and predecessor_dict[OHN_HASHEATDEMAND] == OHN_CONSUMER:
        # get the data
        mappping_name = 'TFT_HEAT_SUPPLY'

    # add more cases here ordered by priority
    # elif case 2 ...

    # case n - default case
    else:
        # no match use default mapping
        mappping_name = 'DEFAULT'
        print(f'Using mapping DEFAULT')

    print(f'Using mapping {mappping_name}')
    return mappping_name
