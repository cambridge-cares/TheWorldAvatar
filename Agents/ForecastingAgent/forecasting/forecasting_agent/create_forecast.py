################################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk) #
# Date: 31 Oct 2022                            #
################################################

# The purpose of this module is to forecast a time series using a trained model or Prophet

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

    covariates_iris, covariates, input_length = None, None, None

    # get the mapping dictionary either from the a specific force_mapping
    # or from identifying the dataIRI
    mapping = {}
    if force_mapping is not None:
        print(f'Using forced  mapping {force_mapping}')
        mapping_name = force_mapping
    else:
        mapping_name = get_mapping(dataIRI, kgClient)

    mapping = MAPPING[mapping_name]
    mapping['name'] = mapping_name
    mapping['dataIRI'] = dataIRI
    mapping['horizon'] = horizon
    mapping['forecast_start_date'] = pd.Timestamp(dt.datetime.strptime(
        forecast_start_date, TIME_FORMAT))

    # use mapping function to get the correct dataIRI timeseries and its covariates
    series, covariates, mapping = load_ts_data(
        mapping, kgClient, tsClient)

    # split series at forecast_start_date 
    # if more time steps are available after forecast_start_date
    # backtest can be performed later
    try:
        series, backtest_series = series.split_after(mapping['forecast_start_date'])
    except:
        backtest_series = None
        
    # load the model
    if 'model' in mapping:
        model = load_pretrained_model(
            mapping)
        # TODO: other models than TFT can have different key
        input_length = model.model_params['input_chunk_length']

    else:
        model = Prophet()
        model.model_name = "Prophet"
        input_length = len(series)

    print(f"Forecasting with {model.model_name} model")
    if 'scale_data' in mapping and mapping['scale_data']:
        # neural methods perform better with scaled inputs
        scaler = Scaler()
        series = scaler.fit_transform(series)

    # make prediction with covariates
    if covariates is not None:
        # prediction failed often, because of wrong dtype -> convert to same dtype
        series = series.astype('float32')
        covariates = covariates.astype('float32')

        if 'train_again' in mapping and mapping['train_again']:
            # train again with covariates
            model.fit(series, future_covariates=covariates)

        forecast = model.predict(
            n=horizon, future_covariates=covariates, series=series)

    # make prediction without covariates
    else:
        if 'train_again' in mapping and mapping['train_again']:
            model.fit(series)

        forecast = model.predict(n=horizon)

    if 'scale_data' in mapping and mapping['scale_data']:
        # scale back
        forecast = scaler.inverse_transform(forecast)

    # metadata
    # input series range
    print(f"Input data range: {series.start_time()} - {series.end_time()}")
    start_date = series.end_time() - series.freq * input_length
    end_date = series.end_time()
    print(f"Model input range: {start_date} - {end_date}")
    print(f"Model output range: {forecast.start_time()} - {forecast.end_time()}")
    print(f'Done with forecast using {model.model_name}')

    # data type
    ts_data_type = DOUBLE

    res = instantiate_forecast(forecast=forecast, dataIRI=dataIRI, model_name=model.model_name, forecast_input_length=input_length,
                               covariates=covariates, tsClient=tsClient, data_type=ts_data_type, cov_iris=covariates_iris, kgClient=kgClient)
    #logger.info('Done with instantiating forecast')

    # calculate error if future target is available
    if backtest_series is not None:
        #logger.info(f'Calculate error')
        target = backtest_series[:horizon]
        error = {}
        error['mape'] = mape(forecast, target)
        error['mase'] = mase(forecast, target, series)
        error['smape'] = smape(forecast, target)
        error['mse'] = mse(forecast, target)
        error['rmse'] = rmse(forecast, target)
        res['error'] = error
        print(error)
    return res


def load_ts_data(mapping, kgClient, tsClient):

    #logger.info('Loading timeseries from KG')
    # get the data
    try:
        # try if ts hasValue where the actual ts is stored
        ts_iri = get_ts_value_iri(mapping['dataIRI'], kgClient)
    except KeyError as e:
        ts_iri = mapping['dataIRI']

    if mapping['forecast_start_date'] is None:
        # get the last value of ts and set it as forecast start date
        latest = tsClient.tsclient.getLatestData(ts_iri, tsClient.conn)
        mapping['forecast_start_date'] = latest['timestamp']

    # calculate lower and upper bound for timeseries to speed up query
    lowerbound, upperbound = get_ts_lower_upper_bound(mapping)

    # load timeseries which should be forecasted
    dates, values = get_ts_data(
        ts_iri, tsClient,  lowerbound=lowerbound, upperbound=upperbound)
    df = pd.DataFrame(zip(values, dates), columns=["ForecastColumn", "Date"])

    if 'load_covariates_func' in mapping:
        # load covariates
        covariates_iris, covariates = mapping['load_covariates_func'](
            kgClient, tsClient, lowerbound, upperbound)
    else:
        covariates = None
        covariates_iris = None

    # df to darts timeseries
    df = df.sort_values(by="Date")
    # remove timezone
    df.Date = pd.to_datetime(df.Date).dt.tz_localize(None)
    series = TimeSeries.from_dataframe(
        df, time_col='Date', value_cols="ForecastColumn")  # , fill_missing_dates =True
    # remove nan values at beginning and end
    series = series.strip()

    mapping['covariates_iris'] = covariates_iris
    mapping['ts_iri'] = ts_iri
    mapping['bounds'] = {'lowerbound': lowerbound, 'upperbound': upperbound}
    print('Done with loading timeseries from KG and TSDB')
    return series, covariates, mapping


def get_ts_lower_upper_bound(mapping):
    # upper bound is forecast_start_date + horizon
    upperbound = mapping['forecast_start_date'] + mapping['frequency'] * (mapping['horizon'] + 1)

    # lower bound is forecast_start_date - input_length
    lowerbound = mapping['forecast_start_date'] - mapping['frequency'] * (mapping['data_length'] + 1)
    return lowerbound.strftime(TIME_FORMAT), upperbound.strftime(TIME_FORMAT)


def load_pretrained_model(mapping, forece_download=False):
    model_path_ckpt_link, model_path_pth_link = mapping['model']['model_path_ckpt_link'], mapping['model']['model_path_pth_link']

    # try to load from checkpoint link
    path_ckpt = ""
    path_pth = ""
    path_to_store = Path(__file__).parent.absolute() / \
        mapping['name'] / 'checkpoints'

    # TODO: until now we need to download both, checkpoint and model file
    # maybe you find a better way to just have one link
    
    if os.path.exists(path_to_store) and not forece_download:
        # model already exists
        pass
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
    model.model_name = model_path_ckpt_link.__str__()
    print(f'Loaded model from  {path_ckpt.parent.parent.__str__()}')

    # convert loaded model to device
    pl_trainer_kwargs = {"accelerator": 'cpu'}
    model.model_params['pl_trainer_kwargs'] = pl_trainer_kwargs
    model.trainer_params = pl_trainer_kwargs
    print(f'Moved model to device  {pl_trainer_kwargs["accelerator"]}')
    return model


def instantiate_forecast(forecast, dataIRI, model_name, forecast_input_length, covariates, tsClient, data_type, cov_iris, kgClient):
    #  instantiate forecast in KG
    forecast_iri = KB + 'Forecast_' + str(uuid.uuid4())
    update = ""

    try:
        # get unit from dataIRI and add to forecast
        unit = {OM_HASUNIT: get_unit(dataIRI, kgClient)}
    except KGException as e:
        # no measurement -> no unit
        unit = {}

    time_format = get_time_format(dataIRI, kgClient)

    ONTOEMS_HASFORECASTINPUTLENGHT = ONTOEMS + "hasForecastInputLength"
    ONTOEMS_HASCOVARIATE = ONTOEMS + "hasCovariate"
    ONTOEMS_HASFORECASTMODEL = ONTOEMS + "hasForecastModel"
    covariate_update = {ONTOEMS_HASCOVARIATE: cov_iris} if cov_iris else {}
    update += get_properties_for_subj(subj=forecast_iri, verb_obj={
        RDF_TYPE: ONTOEMS_FORECAST,
        **unit,
        **covariate_update,
    }, verb_literal={
        ONTOEMS_HASFORECASTMODEL: model_name,
        ONTOEMS_HASFORECASTINPUTLENGHT: forecast_input_length,
    })

    # call client
    tsClient.tsclient.initTimeSeries([forecast_iri], [data_type], time_format,
                                     tsClient.conn)
    ts = TSClient.create_timeseries([str(x) for x in forecast.time_index], [
                                    forecast_iri], [forecast.values().squeeze().tolist()])
    tsClient.tsclient.addTimeSeriesData(ts, tsClient.conn)

    update += get_properties_for_subj(subj=dataIRI, verb_obj={
        ONTOEMS_HASFORECASTEDVALUE: forecast_iri})

    kgClient.performUpdate(add_insert_data(update))
    return {'forecast_iri': forecast_iri, 'model': model_name, 'forecast_input_length': forecast_input_length, 'covariates': cov_iris}


def get_mapping(dataIRI, kgClient):
    # identify the dataIRI for right mapping function

    # get properties of dataIRI and its neighbors
    predecessor_dict = get_predecessor_type_and_predicate(dataIRI, kgClient)

    # use properties to identify the right mapping function

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
