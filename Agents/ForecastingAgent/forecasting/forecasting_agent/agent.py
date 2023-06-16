################################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk)   #
#          Markus Hofmeister (mh807@cam.ac.uk) # 
# Date: 30 Nov 2022                            #
################################################

# This module represents the main forecasting agent to forecast a time series using 
# a trained model or Prophet (as default). The forecast is then stored in the KG.

import os
import uuid
import urllib
from pathlib import Path
import pandas as pd
import torch

from dateutil.parser import isoparse
from darts import TimeSeries
from darts.models import Prophet, TFTModel
from darts.dataprocessing.transformers import Scaler
from darts.metrics import mape, mse, rmse, smape

from py4jps import agentlogging

from forecasting.utils.tools import *
from forecasting.utils.default_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT, \
                                              DB_URL, DB_USER, DB_PASSWORD
from forecasting.datamodel.iris import *
from forecasting.datamodel.data_mapping import *
from forecasting.kgutils.kgclient import KGClient
from forecasting.kgutils.tsclient import TSClient
from forecasting.errorhandling.exceptions import KGException

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


def forecast(iri, horizon, forecast_start_date=None, use_model_configuration=None, data_length=None,
             query_endpoint=QUERY_ENDPOINT, update_endpoint=UPDATE_ENDPOINT, 
             db_url=DB_URL, db_user=DB_USER, db_password=DB_PASSWORD):
    """
    Forecast a time series using a pre trained model or Prophet.
    returns a dictionary with the forecast and some metadata.

    :param iri: The IRI of the time series you want to forecast
    :param horizon: The number of time steps to forecast
    :param forecast_start_date: The date from which you want to start forecasting
    :param use_model_configuration: If you want to force a specific model configuration, you can do so here
    :param data_length: The number of time steps which should be loaded from the DB
    :param query_endpoint: The endpoint to query the KG
    :param update_endpoint: The endpoint to update the KG
    :param db_url: The url to the RDB
    :param db_user: The user to the RDB
    :param db_password: The password to the RDB
    
    :return: The forecast is being returned.
    """
    # Initialise the KG and TS clients
    kgClient = KGClient(query_endpoint=query_endpoint, update_endpoint=update_endpoint)
    tsClient = TSClient(kg_client=kgClient, rdb_url=db_url, rdb_user=db_user, rdb_password=db_password)

    covariates, backtest_series = None, None

    # get the model configuration dictionary from the a specific use_model_configuration or use default
    if use_model_configuration is not None:
        model_configuration_name = use_model_configuration
    else:
        model_configuration_name = "DEFAULT"
        
    if model_configuration_name not in MODEL_MAPPING:
        raise KeyError(f'No model configuration found for the given key: {model_configuration_name}')
    
    cfg = MODEL_MAPPING[model_configuration_name].copy()
    cfg['model_configuration_name'] = model_configuration_name
    cfg['iri'] = iri
    cfg['horizon'] = horizon
    if data_length is not None:
        cfg['data_length'] = data_length
        
    cfg['dataIRI'] = get_data_iri(cfg, kgClient)
    cfg['forecast_start_date'] = get_forecast_start_date(
        forecast_start_date, tsClient, cfg)
    logger.info(f'Using the forecast start date: {cfg["forecast_start_date"]}')
    
    # calculate lower and upper bound for timeseries to speed up query
    lowerbound, upperbound = get_ts_lower_upper_bound(cfg)
    cfg['loaded_data_bounds'] = {
        'lowerbound': lowerbound, 'upperbound': upperbound}
    logger.info(f'Using the following time bounds for the query: {cfg["loaded_data_bounds"]}')
    
    # use model configuration function to get the correct iri timeseries and its covariates
    series, covariates = load_ts_data(
        cfg, kgClient, tsClient)

    # split series at forecast_start_date
    # check if forecast_start_date is in series
    if cfg['forecast_start_date'] in series.time_index:
        series, backtest_series = series.split_before(
            cfg['forecast_start_date'])
        logger.info('Forecast start date is in series. Splitting series at forecast start date.')

    # or the next date
    elif cfg['forecast_start_date'] == series.time_index[-1] + series.freq:
        # keep whole series
        logger.info('Forecast start date is the next date after the last date in the series. Keeping whole series.')
        pass

    # Timestamp out of series range
    else:
        logger.error('Forecast start date is out of series range.')
        raise ValueError(
            f'forecast_start_date: {cfg["forecast_start_date"]} - out of range of series start {series.start_time()} and end {series.end_time()}')

    # load the model
    # NOTE: If you have multiple different models, you need to edit here the loading function,
    # you can use the model_configuration_name to load the correct model or
    # add a function to the model config like for loading the covariates
    if 'TFT_HEAT_SUPPLY' == cfg['model_configuration_name']:
        model = load_pretrained_model(
            cfg, TFTModel)
        # other models than TFT can have different key then 'input_chunk_length'
        cfg['fc_model']['input_length'] = model.model_params['input_chunk_length']
        # check if length of series is long enough for the model input length
        if len(series) < cfg['fc_model']['input_length']:
            logger.error('Series is too short for the model input length. Set data_length to at least the length of the model input length or use an other model.')
            raise ValueError(
                f'Length of series: {len(series)} is shorter than the required input length of the model: {cfg["fc_model"]["input_length"]}')
        # check that horizon is bigger than output_chunk_length
        if cfg['horizon'] < model.model_params['output_chunk_length']:
            logger.error('Horizon is less than the the model output length. Set horizon to at least the length of the model output length or use an other model.')
            raise ValueError(
                f'horizon: {cfg["horizon"]} is smaller than output_chunk_length: {model.model_params["output_chunk_length"]}. Specify a horizon bigger than the output_chunk_length of your model.')
            
    elif 'DEFAULT' == cfg['model_configuration_name']:
        logger.info('Using Prophet model.')
        model = Prophet()
        cfg['fc_model']['input_length'] = len(series)
    #TODO: Potentially to be revisited / aligned with other configurations
    elif 'PIRMASENS' == cfg['model_configuration_name']:
        logger.info('Using Prophet model.')
        model = Prophet()
        cfg['fc_model']['input_length'] = len(series)
    else:
        logger.error('No model configuration found for the given name.')
        raise ValueError(
            f'No model configuration found for the given name{cfg["model_configuration_name"]}, use one of {MODEL_MAPPING.keys()}')

    forecast = get_forecast(series, covariates, model, cfg)

    # create metadata
    # input series range
    start_date = series.end_time() - series.freq * \
        (cfg['fc_model']['input_length'] - 1)
    cfg['model_input_interval'] = [start_date, series.end_time()]
    cfg['model_output_interval'] = [forecast.start_time(), forecast.end_time()]
    cfg['created_at'] = pd.to_datetime('now', utc=True)
    logger.info(f'Created forecast at: {cfg["created_at"]}')
    logger.info(f'Output interval: {cfg["model_output_interval"]}')
    logger.info(f'Input interval: {cfg["model_input_interval"]}')
    cfg['tsIRI'] = get_tsIRI_from_dataIRI(cfg['dataIRI'], kgClient)
    
    # Get unit from IRI and add to forecast
    unit = get_unit(cfg['iri'], kgClient)
    if unit:
        cfg['unit'] = unit

    # Get time format from tsIRI
    cfg['time_format'] = get_time_format(cfg['iri'], kgClient)

    update = get_forecast_update(cfg=cfg)
    
    kgClient.performUpdate(add_insert_data(update))
    logger.info(f'Added forecast ontology to KG')
    
    # call client
    instantiate_forecast_timeseries(tsClient, cfg, forecast)
    
    if backtest_series is not None:
        cfg['error'] = calculate_error(backtest_series, forecast)
        logger.info(f'Calculated error: {cfg["error"]}')
    
    # delete keys which you dont want in response, e.g. not json serializable objects
    keys_to_delete = ['load_covariates_func',
                      'time_delta', 'ts_data_type', 'frequency']
    for key in keys_to_delete:
        if key in cfg:
            del cfg[key]
    return cfg


def instantiate_forecast_timeseries(tsClient, cfg, forecast):
    try:
        with tsClient.connect() as conn:
            tsClient.tsclient.initTimeSeries([cfg['forecast_iri']], [cfg['ts_data_type']], cfg['time_format'],
                                            conn)
            ts = TSClient.create_timeseries([str(x) for x in forecast.time_index.tz_localize('UTC').strftime(TIME_FORMAT)], [
                                            cfg['forecast_iri']], [forecast.values().squeeze().tolist()])
            tsClient.tsclient.addTimeSeriesData(ts, conn)
            logger.info(f'Added forecast timeseries to TSDB.')

            
    except:
        logger.error('Could not instantiate forecast timeseries.')
        raise KGException(f'Could not instantiate forecast timeseries {cfg["forecast_iri"]}')


def get_forecast_start_date(forecast_start_date, tsClient, cfg):
    if forecast_start_date is not None:
        return pd.Timestamp(
            isoparse(forecast_start_date)).tz_convert('UTC').tz_localize(None)
    else:
        # get the last value of ts and set next date as forecast start date
        try:
            with tsClient.connect() as conn:
                latest = tsClient.tsclient.getLatestData(cfg['dataIRI'], conn)
        except:
            raise KGException(
                f"No time series data could be retrieved for the given IRI: {cfg['dataIRI']}")
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

    if cfg['fc_model']['scale_data']:
        # neural methods perform better with scaled inputs
        logger.info(f'Scaling data with data of length:{len(series)}. Make sure that series is long enough (controlled with "data_length") to represent the whole data distribution. ')
        scaler = Scaler()
        series = scaler.fit_transform(series)

    # make forecast with covariates
    if covariates is not None:
        if cfg['fc_model']['train_again']:
            logger.info(f'Training model again with covariates.')
            # train again with covariates
            model.fit(series, future_covariates=covariates)
        logger.info(f'Making forecast with covariates.')
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
            logger.info(f'Training model again without covariates.')
            model.fit(series)
        logger.info(f'Making forecast without covariates.')
        forecast = model.predict(n=cfg['horizon'])

    if cfg['fc_model']['scale_data']:
        # scale back
        logger.info(f'Scaling back data.')
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
    error['max_error'] = max_error(target, forecast)
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

    if 'load_covariates_func' in cfg:
        # load covariates
        logger.info('Loading covariates with function: ' + cfg['load_covariates_func'].__name__)
        covariates_iris, covariates = cfg['load_covariates_func'](
            kgClient, tsClient, cfg['loaded_data_bounds']['lowerbound'], cfg['loaded_data_bounds']['upperbound'])
        cfg['fc_model']['covariates_iris'] = covariates_iris

        # check if covariates are given for complete future horizon from forecast_start_date
        check_if_enough_covs_exist(cfg, covariates)
    else:
        logger.info('No covariates loading function given, no covariates are loaded')
        covariates = None

    # load timeseries which should be forecasted
    df = get_df_of_ts(cfg['dataIRI'], tsClient, cfg['loaded_data_bounds']['lowerbound'],
                      cfg['loaded_data_bounds']['upperbound'], column_name="Series", date_name="Date")

    # df to darts timeseries
    #NOTE Inlcuded resampling for irregularly spaced time series data to avoid issues
    #     with Darts being unable to retrieve frequency of time series
    #     (likely to be refined in the future)

    if cfg.get('resample_data'):
        df_resampled = df.set_index('Date').copy()
        df_resampled = df_resampled.resample(cfg.get('resample_data')).mean()
        df = df_resampled.reset_index()

    try:
        # Build a deterministic TimeSeries instance from DataFrame as is
        series = TimeSeries.from_dataframe(df, time_col='Date', value_cols="Series")
    except ValueError:
        # Fill missing times with NaN values if processing DataFrame as is fails;
        # requires possibility to infer the frequency from the provided timestamps
        series = TimeSeries.from_dataframe(df, time_col='Date', value_cols="Series",
                                           fill_missing_dates=True, 
                                           freq=None)

    # remove nan values at beginning and end
    series = series.strip()

    logger.info('Done with loading timeseries from KG and TSDB')
    return series, covariates


def get_data_iri(cfg, kgClient):
    """
    Retrieves the time series IRI from the KG.
    Either the iri has directly object with 'hasTimeSeries' with 'Measure' in between, 
    or the iri has a 'hasTimeSeries' with no 'Measure' in between, 
    in both cases the iri of the object after 'hasTimeSeries' is returned.

    1. iri -> hasValue -> Measure -> hasTimeSeries -> ts_iri
    2. iri -> hasTimeSeries -> ts_iri

    :param cfg: a dictionary containing the model configuration
    :param kgClient: the client object for the knowledge graph
    """
    try:
        # try if ts hasValue where the actual ts is stored
        dataIRI = get_dataIRI_for_ts_with_hasValue_iri(cfg['iri'], kgClient)
    except IndexError as e:
        dataIRI = cfg['iri']

    return dataIRI


def check_if_enough_covs_exist(cfg, covariates):
    if covariates is not None and (cfg['forecast_start_date'] + cfg['frequency'] * (cfg['horizon'] - 1) > covariates.end_time()):
        logger.error(
            f'Not enough covariates exist for the given forecast_start_date and horizon. The last covariate date is {covariates.end_time()}')
        raise ValueError(
            f'Not enough covariates for complete future horizon. Covariates end at {covariates.end_time()} but forecast horizon ends at {cfg["forecast_start_date"] + cfg["frequency"] * (cfg["horizon"] - 1)}')
    return True


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


def load_pretrained_model(cfg, ModelClass, force_download=False):
    """
    It downloads a model from a link, and then loads it into a Darts model

    :param cfg: a dictionary containing the configuration of the model
    :param ModelClass: the class of the model
    :param force_download: If you want to download the model again if a folder already exists, set this to True, defaults to False
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

    if os.path.exists(path_to_store) and not force_download:
        # model already exists
        path_ckpt = path_to_store / "best-model.ckpt"
        path_pth = ""
    else:

        # create folder
        if not os.path.exists(path_to_store):
            os.makedirs(path_to_store)

        if model_path_ckpt_link.startswith("https://"):
            # download checkpoint model
            path_ckpt, _ = urllib.request.urlretrieve(
                model_path_ckpt_link, path_to_store / "best-model.ckpt")
            logger.info(
                f'Downloaded checkpoint model from {model_path_ckpt_link} to {path_ckpt}')

        if model_path_pth_link.startswith("https://"):
            # download model
            path_pth, _ = urllib.request.urlretrieve(
                model_path_pth_link, path_to_store.parent.absolute() / "_model.pth.tar")
            logger.info(f'Downloaded model from {model_path_pth_link} to {path_pth}')

    # TFT model has been trained and saved on a CUDA device (i.e., using GPUs);
    # Attempting to deserialize saved model on a CPU-only machine will fail

    # Check if current system supports CUDA
    if torch.cuda.is_available():
        # Use easy load from checkpoint if yes
        model = ModelClass.load_from_checkpoint(path_ckpt.parent.parent.__str__())
    else:
        # Otherwise use torch.load with map_location=torch.device('cpu') to map your storages to the CPU
        device = torch.device('cpu')
        checkpoint = torch.load(path_ckpt, map_location=device)
        # Initialise 'new' model with matching input/output lengths
        model = ModelClass(input_chunk_length=checkpoint['hyper_parameters']['input_chunk_length'],
                           output_chunk_length=checkpoint['hyper_parameters']['output_chunk_length'])
        # Overwrite model parameters with saved model
        model.__dict__.update(checkpoint['state_dict'])
        #model= torch.load('forecasting_agent/Models/TFT_HEAT_SUPPLY/_model.pth.tar')

    logger.info(f'Loaded model from  {path_ckpt.parent.parent.__str__()}')

    # convert loaded model to device
    pl_trainer_kwargs = {"accelerator": 'cpu'}
    model.model_params['pl_trainer_kwargs'] = pl_trainer_kwargs
    model.trainer_params = pl_trainer_kwargs
    logger.info(f'Moved model to device  {pl_trainer_kwargs["accelerator"]}')
    return model


def get_forecast_update(cfg):
    """
    Takes a model configuration and returns the forecast SPARQL update query to instantiate the forecast in the KG

    :param cfg: a dictionary with meta information:
        'forecast_start_date': the start date of the forecast
        'frequency': the frequency of the time series data
        'horizon': the length of the forecast
        'data_length': the length of the time series data to retrieve before the forecast_start_date
        'dataIRI': the iri of the timeseries which is forecasted
        'iri': the iri which has the predicate hasTimeSeries
        'fc_model': the configuration of the model
        'model_configuration_name': the name of the model configuration
        'loaded_data_bounds': the lower and upper bounds of the time series
        'forecast_name': the name of the forecast
    """
    #  instantiate forecast in KG
    cfg['forecast_iri'] = KB + 'Forecast_' + str(uuid.uuid4())
    update = ""

    unit = {OM_HASUNIT: cfg['unit']} if 'unit' in cfg else {}
    covariate_update = {
        TS_HASCOVARIATE: cfg['covariates_iris']} if 'covariates_iris' in cfg else {}

    # if Train again is given, set the hasTrainingTimeSeries to the new training series
    training_series = {TS_HASTRAININGTIMESERIES: cfg['tsIRI']} if 'train_again' in cfg['fc_model'] and cfg['fc_model']['train_again'] else {}
    
    # model
    forecastingModel_iri = KB + 'ForecastingModel_' + str(uuid.uuid4())
    update += get_properties_for_subj(subj=forecastingModel_iri, verb_obj={
        RDF_TYPE: TS_FORECASTINGMODEL,
        **covariate_update,
        **training_series,
        
    }, verb_literal={
        RDFS_LABEL: cfg['fc_model']['name'],
    })

    if 'model_path_ckpt_link' in cfg['fc_model']:
        # url chkpt
        update += get_properties_for_subj(subj=forecastingModel_iri, verb_literal={
            TS_HASURL: [cfg['fc_model']['model_path_ckpt_link'], XSD_STRING],
        })
    if 'model_path_pth_link' in cfg['fc_model']:
        # url pth
        update += get_properties_for_subj(subj=forecastingModel_iri, verb_literal={
            TS_HASURL: [cfg['fc_model']['model_path_pth_link'], XSD_STRING],
        })

    # intervals
    outputTimeInterval_iri = KB + 'Interval_' + str(uuid.uuid4())
    inputTimeInterval_iri = KB + 'Interval_' + str(uuid.uuid4())

    outputEnd_iri, q = get_time_instant(cfg['model_output_interval'][1])
    update += q
    outputBeginning_iri, q = get_time_instant(cfg['model_output_interval'][0])
    update += q
    inputEnd_iri, q = get_time_instant(cfg['model_input_interval'][1])
    update += q
    inputBeginning_iri, q = get_time_instant(cfg['model_input_interval'][0])
    update += q

    update += get_properties_for_subj(subj=outputTimeInterval_iri, verb_obj={
        RDF_TYPE: TIME_INTERVAL,
        TIME_HASBEGINNING: outputBeginning_iri,
        TIME_HASEND: outputEnd_iri
    })
    update += get_properties_for_subj(subj=inputTimeInterval_iri, verb_obj={
        RDF_TYPE: TIME_INTERVAL,
        TIME_HASBEGINNING: inputBeginning_iri,
        TIME_HASEND: inputEnd_iri
    })

    update += get_properties_for_subj(subj=cfg['forecast_iri'], verb_obj={
        RDF_TYPE: TS_FORECAST,
        **unit,
        TS_HASFORECASTINGMODEL: forecastingModel_iri,
        TS_HASOUTPUTTIMEINTERVAL: outputTimeInterval_iri,
        TS_HASINPUTTIMEINTERVAL: inputTimeInterval_iri,
    }, verb_literal={
        TS_CREATEDAT: [cfg['created_at'], XSD_DATETIMESTAMP], }
    )

    update += get_properties_for_subj(subj=cfg['iri'], verb_obj={
        TS_HASFORECAST: cfg['forecast_iri']})

    return update


def convert_date_to_timestamp(date):
    """
    Converts a date to a timestamp

    :param date: a date
    :return: the timestamp
    """
    # convert date to timestamp
    if isinstance(date, int):
        time_stamp = date
    elif isinstance(date, str):
        time_stamp = pd.Timestamp(
            isoparse(date)).tz_convert('UTC').tz_localize(None).timestamp()
    elif isinstance(date, pd.Timestamp):
        time_stamp = date.timestamp()
    else:
        raise ValueError(
            f'Unknown date format: {date}. Please use int, str or pd.Timestamp')
    return int(time_stamp)


def get_time_instant(date):

    time_stamp = convert_date_to_timestamp(date)

    instant_iri = KB + 'Instant_' + str(uuid.uuid4())
    update = ''

    timePosition_iri = KB + 'TimePosition_' + str(uuid.uuid4())

    update += get_properties_for_subj(subj=timePosition_iri, verb_obj={
        RDF_TYPE: TIME_TIMEPOSITION,
        TIME_HASTRS: UNIX_TIME
    }, verb_literal={
        TIME_NUMERICPOSITION: [time_stamp, XSD_INTEGER]
    })

    update += get_properties_for_subj(subj=instant_iri, verb_obj={
        RDF_TYPE: TIME_INSTANT,
        TIME_INTIMEPOSITION: timePosition_iri})

    return instant_iri, update
