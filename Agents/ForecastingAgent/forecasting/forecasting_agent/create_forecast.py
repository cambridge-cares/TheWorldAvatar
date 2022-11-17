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
from forecasting.kgutils.querytemplates import *
from forecasting.kgutils.tsclient import TSClient
from forecasting.utils.properties import *
from forecasting.utils.tools import *
from forecasting.utils.useful_queries import *

# Initialise logger
#logger = agentlogging.get_logger("prod")


def forecast(dataIRI, horizon, forecast_start_date=None, force_mapping=None):
    # initialise the  client
    kgClient = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)
    tsClient = TSClient(kg_client=kgClient)

    covariates =  None

    # get the mapping dictionary either from the a specific force_mapping
    # or from identifying the dataIRI
    if force_mapping is not None:
        print(f'Using forced  mapping {force_mapping}')
        mapping_name = force_mapping
    else:
        mapping_name = get_mapping(dataIRI, kgClient)

    res = MAPPING[mapping_name].copy()
    res['mapping_name'] = mapping_name
    res['dataIRI'] = dataIRI
    res['horizon'] = horizon
    print('Using mapping: ', res)

        
    if forecast_start_date is not None:
        res['forecast_start_date'] = pd.Timestamp(isoparse(forecast_start_date)).tz_convert('UTC').tz_localize(None)
    else:
        res['forecast_start_date'] = None

    # use mapping function to get the correct dataIRI timeseries and its covariates
    series, covariates = load_ts_data(
        res, kgClient, tsClient)
    
    # split series at forecast_start_date 
    # if more time steps are available after forecast_start_date
    # backtest can be performed later
    try:
        series, backtest_series = series.split_before(res['forecast_start_date'])
    except ValueError as e:
        # Timestamp out of series range
        print(f'Cannot split series at {res["forecast_start_date"]} - out of range')
        backtest_series = None
        

    # load the model
    # TODO: If you have multiple models and you need different loading functions,
    # you can add them here. Maybe even add a function to the mapping dictionary
    if 'model_path_ckpt_link' in res['fc_model']:
        model = load_pretrained_model(
            res)
        # other models than TFT can have different key then 'input_chunk_length'
        res['fc_model']['input_length'] = model.model_params['input_chunk_length']

    else:
        model = Prophet()
        res['fc_model']['input_length'] = len(series)
        
    forecast = get_forecast(series, covariates, model, res)
    # metadata
    # input series range
    print(f"Input data range: {series.start_time()} - {series.end_time()}")
    start_date = series.end_time() - series.freq * (res['fc_model']['input_length'] - 1)
    end_date = series.end_time()
    print(f"Model input range: {start_date} - {end_date}")
    print(f"Model output range: {forecast.start_time()} - {forecast.end_time()}")
    print(f'Done with forecast using  ')

    res['model_input_interval'] = [start_date, end_date]
    res['model_output_interval'] = [forecast.start_time(), forecast.end_time()]
    res['created_at'] = pd.to_datetime('now')
    
    
    # delete not json serializable objects from res
    keys_to_delete = ['load_covariates_func', 'time_delta', 'ts_data_type', 'frequency']
    for key in keys_to_delete:
        if key in res:
            del res[key]
    
    #res = {}
    
    #instantiate_forecast(res = res, forecast=forecast, tsClient=tsClient, kgClient=kgClient)

    if backtest_series is not None:
        # calculate error if future target is available
        res['error'] = calculate_error(backtest_series, forecast)
        print(res['error'])
    return res

def get_forecast(series, covariates, model, res):
    
    print(f"Forecasting with {res['fc_model']['name']} model")
    if res['fc_model']['scale_data']:
        # neural methods perform better with scaled inputs
        scaler = Scaler()
        series = scaler.fit_transform(series)

    # make forecast with covariates
    if covariates is not None:
        if res['fc_model']['train_again']:
            # TODO: add option to train model with covariates
            # train again with covariates
            model.fit(series, future_covariates=covariates)

        try:
            forecast = model.predict(
                n=res['horizon'], future_covariates=covariates, series=series)
        except RuntimeError as e:
            # prediction failed often, because of wrong dtype -> convert to same dtype
            series = series.astype('float32')
            covariates = covariates.astype('float32')
            forecast = model.predict(
                n=res['horizon'], future_covariates=covariates, series=series)
        

    # make prediction without covariates
    else:
        if res['fc_model']['train_again']:
            model.fit(series)

        forecast = model.predict(n=res['horizon'])

    if res['fc_model']['scale_data']:
        # scale back
        forecast = scaler.inverse_transform(forecast)
        
    return forecast




def calculate_error(target, forecast):
    """Calculate error metrics between target and forecast

    Args:
        target (TimeSeries): target timeseries
        forecast (TimeSeries): forecast timeseries

    Returns:
        dict: dictionary with error metrics
    """
    error = {}
    error['mape'] = mape(target, forecast)
    error['smape'] = smape(target, forecast)
    error['mse'] = mse(target, forecast)
    error['rmse'] = rmse(target, forecast)
    return error

def load_ts_data(res, kgClient, tsClient):

    #logger.info('Loading timeseries from KG')
    # get the data
    try:
        # try if ts hasValue where the actual ts is stored
        res['ts_iri'] = get_ts_value_iri(res['dataIRI'], kgClient)
    except KeyError as e:
        res['ts_iri'] = res['dataIRI']

    if res['forecast_start_date'] is None:
        # get the last value of ts and set next date as forecast start date
        latest = tsClient.tsclient.getLatestData(res['ts_iri'], tsClient.conn)
        res['forecast_start_date']  = pd.Timestamp(isoparse(latest.getTimes()[0].toString())).tz_convert('UTC').tz_localize(None) + res['frequency']       

    # calculate lower and upper bound for timeseries to speed up query
    lowerbound, upperbound = get_ts_lower_upper_bound(res)

    if 'load_covariates_func' in res:
        # load covariates
        covariates_iris, covariates = res['load_covariates_func'](
            kgClient, tsClient, lowerbound, upperbound)
        res['fc_model']['covariates_iris'] = covariates_iris

        # check if covariates are given for complete future horizon from forecast_start_date
        if covariates is not None and( res['forecast_start_date'] + res['frequency'] * (res['horizon'] - 1) > covariates.end_time()):
            # use default model
            print(f'\n\nNot enough covariates for complete future horizon. Default model is used instead.')
            res['fc_model'] = MAPPING['DEFAULT']['fc_model'].copy()
            covariates = None            
    else:
        covariates = None
    
    # load timeseries which should be forecasted
    df =  get_df_of_ts(res['ts_iri'], tsClient ,lowerbound, upperbound, column_name = "Series", date_name = "Date")

    # df to darts timeseries
    series = TimeSeries.from_dataframe(
        df, time_col='Date', value_cols="Series")  # , fill_missing_dates =True
    # remove nan values at beginning and end
    series = series.strip()


    res['loaded_data_bounds'] = {'lowerbound': lowerbound, 'upperbound': upperbound}
    
    print('Done with loading timeseries from KG and TSDB')
    return series, covariates


def get_ts_lower_upper_bound(res):
    # upper bound is forecast_start_date + horizon
    upperbound = res['forecast_start_date'] + res['frequency'] * (res['horizon'] - 1)

    # lower bound is forecast_start_date - input_length
    lowerbound = res['forecast_start_date'] - res['frequency'] * (res['data_length'])
    return lowerbound.strftime(TIME_FORMAT), upperbound.strftime(TIME_FORMAT)


def load_pretrained_model(res, forece_download=False):
    model_path_ckpt_link, model_path_pth_link = res['fc_model']['model_path_ckpt_link'], res['fc_model']['model_path_pth_link']

    # try to load from checkpoint link
    path_ckpt = ""
    path_pth = ""
    path_to_store = Path(__file__).parent.absolute() / \
        'Models' / res['fc_model']['name'] / 'checkpoints'

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
            path_ckpt, res = urllib.request.urlretrieve(
                model_path_ckpt_link, path_to_store / "best-model.ckpt")
            print(f'Downloaded checkpoint model from {model_path_ckpt_link} to {path_ckpt}')

        if model_path_pth_link.startswith("https://"):
            # download model
            path_pth, res = urllib.request.urlretrieve(
                model_path_pth_link, path_to_store.parent.absolute() / "_model.pth.tar")
            print(f'Downloaded model from {model_path_pth_link} to {path_pth}')
    
    # try to load model from downloaded checkpoint
    # model = TFTModel.load(
    #    path_pth.__str__())
    model = TFTModel.load_from_checkpoint(
        path_ckpt.parent.parent.__str__())
    res['fc_model']['name'] = model_path_ckpt_link.__str__()
    print(f'Loaded model from  {path_ckpt.parent.parent.__str__()}')

    # convert loaded model to device
    pl_trainer_kwargs = {"accelerator": 'cpu'}
    model.model_params['pl_trainer_kwargs'] = pl_trainer_kwargs
    model.trainer_params = pl_trainer_kwargs
    print(f'Moved model to device  {pl_trainer_kwargs["accelerator"]}')
    return model


def instantiate_forecast(res, forecast, tsClient, kgClient):
    #  instantiate forecast in KG
    forecast_iri = KB + 'Forecast_' + str(uuid.uuid4())
    update = ""

    try:
        # get unit from dataIRI and add to forecast
        unit = {OM_HASUNIT: get_unit(res['dataIRI'], kgClient)}
        res['unit'] = unit[OM_HASUNIT]
    except KGException as e:
        # no measurement -> no unit
        unit = {}

    time_format = get_time_format(res['dataIRI'], kgClient)

    ONTOEMS_HASFORECASTINPUTLENGHT = ONTOEMS + "hasForecastInputLength"
    ONTOEMS_HASCOVARIATE = ONTOEMS + "hasCovariate"
    ONTOEMS_HASFORECASTMODEL = ONTOEMS + "hasForecastModel"
    covariate_update = {ONTOEMS_HASCOVARIATE: res['covariates_iris']} if res['covariates_iris'] else {}
    update += get_properties_for_subj(subj=forecast_iri, verb_obj={
        RDF_TYPE: ONTOEMS_FORECAST,
        **unit,
        **covariate_update,
    }, verb_literal={
        ONTOEMS_HASFORECASTMODEL: res['fc_model']['name'] ,
        ONTOEMS_HASFORECASTINPUTLENGHT: res['forecast_input_length'],
    })

    # call client
    tsClient.tsclient.initTimeSeries([forecast_iri], [res['data_type']], time_format,
                                     tsClient.conn)
    ts = TSClient.create_timeseries([str(x) for x in forecast.time_index], [
                                    forecast_iri], [forecast.values().squeeze().tolist()])
    tsClient.tsclient.addTimeSeriesData(ts, tsClient.conn)

    update += get_properties_for_subj(subj=res['dataIRI'], verb_obj={
        ONTOEMS_HASFORECASTEDVALUE: forecast_iri})

    kgClient.performUpdate(add_insert_data(update))
    res['forecast_iri'] = forecast_iri


def get_mapping(dataIRI, kgClient):
    # identify the dataIRI for right mapping function

    # get properties of dataIRI and its neighbors
    predecessor_dict = get_predecessor_type_and_predicate(dataIRI, kgClient)

    # use properties to identify the right mapping function
    # TODO: specify identification in mapping functions
    # case 1 - heat supply data identified
    if OHN_HASHEATDEMAND in predecessor_dict and predecessor_dict[OHN_HASHEATDEMAND] == OHN_CONSUMER:
        # get the data
        mappping_name = 'TFT_HEAT_SUPPLY'
        print(f'Using mapping TFT_HEAT_SUPPLY')

    # add more cases here ordered by priority
    # elif case 2 ...

    # case n - default case
    else:
        # no match use default mapping
        mappping_name = 'DEFAULT'
        print(f'Using mapping DEFAULT')

    return mappping_name
