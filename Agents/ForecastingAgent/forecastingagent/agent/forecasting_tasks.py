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

from dateutil.parser import isoparse
from darts import TimeSeries
from darts.models import Prophet, TFTModel
from darts.dataprocessing.transformers import Scaler
from darts.metrics import mape, mse, rmse, smape

from py4jps import agentlogging

from forecastingagent.utils.tools import *
from forecastingagent.utils.env_configs import SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT, \
                                               DB_USER, DB_PASSWORD
from forecastingagent.datamodel.iris import *
from forecastingagent.agent.forcasting_config import *
from forecastingagent.kgutils.kgclient import KGClient
from forecastingagent.kgutils.tsclient import TSClient
from forecastingagent.errorhandling.exceptions import KGException

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


def forecast(iri, config, db_url, time_format, kgClient=None,
             query_endpoint=SPARQL_QUERY_ENDPOINT, update_endpoint=SPARQL_UPDATE_ENDPOINT,
             db_user=DB_USER, db_password=DB_PASSWORD):
    """
    Forecast a time series using a pre trained model or Prophet.
    returns a dictionary with the forecast and some metadata.

    Arguments:
        iri {str} - instance IRI to forecast (i.e., IRI which will get ts:hasForecast relationship)
        kgClient {KGClient} - KG client to use
        time_format {str} - time format to use to initialise the forecast TimeSeries
        query_endpoint {str} - endpoint to query the KG
        update_endpoint {str} - endpoint to update the KG
        db_url {str} - URL to the RDB
        db_user {str} - username for the RDB
        db_password {str} - password for the RDB
    
    Returns:
        dict - A dictionary with the forecast and some metadata
    """

    # Initialise the KG and TS clients
    if not kgClient:
        kgClient = KGClient(query_endpoint=query_endpoint, update_endpoint=update_endpoint)
    tsClient = TSClient(kg_client=kgClient, rdb_url=db_url, rdb_user=db_user, 
                        rdb_password=db_password)

    # Update/condition forecasting configuration
    cfg = config.copy()
    cfg['iri'] = iri 
    
    # Calculate lower and upper bound for time series to speed up TSClient queries
    # (i.e., only query the time series data that is needed)
    lowerbound, upperbound = get_ts_lower_upper_bound(cfg, time_format=time_format)
    cfg['loaded_data_bounds'] = {
        'lowerbound': lowerbound, 
        'upperbound': upperbound
        }
    
    # Log some information before calculating forecast
    logger.info(f'Creating forecast for: {cfg["iri"]}')
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
    if cfg['fc_model'].get('label') == 'prophet':
        logger.info('Using default Prophet model to forecast.')
        model = Prophet()
        cfg['fc_model']['input_length'] = len(series) 

    # TODO to be reworked for TFT & others
    elif 'TFT_HEAT_SUPPLY' == cfg['model_configuration_name']:
    # NOTE: If you have multiple different models, you need to edit here the loading function,
    # you can use the model_configuration_name to load the correct model or
    # add a function to the model config like for loading the covariates
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

    # Create forecast time series
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

    update = get_forecast_update(cfg=cfg)
    
    kgClient.performUpdate(add_insert_data(update))
    logger.info(f'Added forecast ontology to KG')
    
    # call client
    instantiate_forecast_timeseries(tsClient, cfg, forecast)
    
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


def get_forecast(series, covariates, model, cfg):
    """
    It takes a series, covariates, model, and a model configuration as inputs, 
    and returns a forecast.

    :param series: the time series data
    :param covariates: darts series  of covariates
    :param model: the model to use for forecasting
    :param cfg: a dictionary containing the following keys:
        'fc_model': a dictionary containing the following keys:
            'input_length': the number of time steps to use as input for the model
    :return: The forecasted values
    """

    if cfg['fc_model'].get('scale_data'):
        # Neural methods perform better with scaled inputs
        msg = f'Scaling input data with data length of {len(series)} timesteps.'
        msg += 'Make sure that series is long enough (controlled with "data_length") to '
        msg += 'represent the whole data distribution, i.e., at least one full cycle.'
        logger.info(msg)
        scaler = Scaler()
        series = scaler.fit_transform(series)

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
        forecast = model.predict(n=cfg['horizon'])

    if cfg['fc_model'].get('scale_data'):
        # Scale predicted data back
        logger.info('Scaling back predicted data, i.e., back-transform into actual values.')
        forecast = scaler.inverse_transform(forecast)

    return forecast


def load_ts_data(cfg, kgClient, tsClient):
    """
    Load time series and covariates data (if applicable) from RDB. 

    :param cfg: dicti.onary with forecast configuration as returned by 'create_forecast_configuration'
        Relevant keys:
            'fc_model': configuration of forecasting model to use, i.e. relevant covariates
            'dataIRI': dataIRI of the time series to forecast (i.e., for which to retrieve data)
            'loaded_data_bounds': lowerbound and upperbound between which to load data
            'resample_data': string describing potential resampling frequency (for pandas resample function)
    :param kgClient: initialised KG client
    :param tsClient: initialised TS client
    
    :return: darts.TimeSeries objects of timeseries (and covariates)
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

    :param cfg: a dictionary with the following keys:
        'fc_start_timestamp': the start of the forecast (pd.Timestamp)
        'frequency': the frequency of the time series data (dt.timedelta)
        'horizon': the length of the forecast, i.e. number of time steps (int)
        'data_length': the length of the time series data to retrieve before the
                       fc_start_timestamp, i.e. number of time steps (int)

    :return: the lower and upper bounds of the time series as strings
    """

    # upper bound is fc_start_timestamp + forecast horizon
    upperbound = cfg['fc_start_timestamp'] + \
                 cfg['frequency'] * (cfg['horizon'] - 1)

    # lower bound is fc_start_timestamp - (historical) data_length
    lowerbound = cfg['fc_start_timestamp'] - \
        cfg['frequency'] * (cfg['data_length'])
    
    return lowerbound.strftime(time_format), upperbound.strftime(time_format)


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


    # load pre-trained model from best checkpoints
    # NOTE: TFT model has been trained and saved on a CUDA device (i.e., using GPUs);
    #       Attempting to deserialize saved model on a CPU-only machine requires
    #       torchmetrics==0.9.3 and pytorch-lightning==1.7.7 (and will fail otherwise)
    model = ModelClass.load_from_checkpoint(path_ckpt.parent.parent.as_posix())
    logger.info(f'Loaded model from  {path_ckpt.parent.parent.__str__()}')

    # convert loaded model to device
    trainer_params = {
            "accelerator": "auto",
            "devices": "auto",
            "logger": False,
        }
    model.trainer_params.update(trainer_params)

    return model


def get_forecast_update(cfg):
    """
    Takes a model configuration and returns the forecast SPARQL update query to instantiate the forecast in the KG

    :param cfg: a dictionary with meta information:
        'fc_start_timestamp': the start date of the forecast
        'frequency': the frequency of the time series data
        'horizon': the length of the forecast
        'data_length': the length of the time series data to retrieve before the fc_start_timestamp
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
    Converts a date to a unix timestamp

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