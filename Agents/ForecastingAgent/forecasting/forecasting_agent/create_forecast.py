################################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk) #
# Date: 31 Oct 2022                            #
################################################

# The purpose of this module is to forecast a time series using a trained model or Prophet

import uuid
from forecasting.utils.useful_queries import *
from forecasting.utils.tools import *
import urllib

from darts.dataprocessing.transformers import Scaler
import time
from darts.models import TFTModel, Prophet
import datetime as dt
import os
from darts import TimeSeries
import pandas as pd

#import agentlogging
from forecasting.datamodel.iris import *
from forecasting.datamodel.data_mapping import *
from forecasting.errorhandling.exceptions import KGException
from forecasting.kgutils.kgclient import KGClient
from forecasting.kgutils.tsclient import TSClient
#from forecasting.utils.api_endpoints import HM_SPARQL_ENDPOINT
#from forecasting.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT
from forecasting.kgutils.querytemplates import *
from forecasting.utils.properties import *


# Initialise logger
#logger = agentlogging.get_logger("prod")


def forecast(dataIRI, horizon=24 * 7, forecast_start_date=None, model_path_ckpt_link=None, model_path_pth_link=None):
    # initialise the  client
    kgClient = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)
    tsClient = TSClient(kg_client=kgClient)
    max_input_length = 365 * 24 * 2
    time_format = "%Y-%m-%dT%H:%M:%SZ"
    ts_freq = dt.timedelta(hours=1)
    cov_iris, covariates = None, None
    lowerbound, upperbound = None, None
    
    try:
        # load model
        model = load_pretrained_model(model_path_ckpt_link, model_path_pth_link)
        input_length = model.model_params['input_chunk_length']
        use_pretrained_model = True
        
    except Exception as e:
        # if pretrained model fails, use prophet
        use_pretrained_model = False
        #logger.info(f"Could not forecast with pretrained model. Prophet is used. \nError: {e}")
        model = Prophet()
        model.model_name = "Prophet"
        # set max input length for prophet for time complexity reason
        input_length = max_input_length
        
    # calculate lower and upper bound for the input data for faster retrieval
    # if forecast_start_date is not given, load whole series
    if forecast_start_date is not None:
        if isinstance(forecast_start_date, str):
            upperbound = forecast_start_date
            forecast_start_date = pd.Timestamp(dt.datetime.strptime(
                forecast_start_date, time_format)) 
            lowerbound = forecast_start_date - ts_freq * input_length
            lowerbound = lowerbound.astype(str)
    try:
        # identify the dataIRI for right mapping function
        predecessor_type = get_predecessor_type_by_predicate(
            dataIRI, OHN_HASHEATDEMAND, kgClient)
        # use mapping function to get the correct dataIRI timeseries and its covariates
        df, cov_iris, covariates = mapping_type_data_function[predecessor_type](
            dataIRI, kgClient, tsClient, lowerbound, upperbound)
        #logger.info('Retrieving instantiated timeseries for dataIRI ...')
    except:
        # use default mapping function if could not identify the dataIRI
        predecessor_type = 'Default'
        try:
            df, _, _ = mapping_type_data_function[predecessor_type](
                dataIRI, kgClient, tsClient)
        except Exception as e:
            raise KGException(
                'Error in retrieving instantiated timeseries for dataIRI: {}. {}'.format(dataIRI, e))

    # remove timezone
    df.Date = pd.to_datetime(df.Date).dt.tz_localize(None)

    # create darts TimeSeries object
    series = TimeSeries.from_dataframe(
        df, time_col='Date', value_cols="ForecastColumn")  # , fill_missing_dates =True

    # remove nan values at beginning and end
    series = series.strip()

    # split series at forecast_start_date if given
    if forecast_start_date is not None:
        if isinstance(forecast_start_date, str):
            forecast_start_date = pd.Timestamp(dt.datetime.strptime(
                forecast_start_date, time_format))  # .timestamp()
        try:
            series, not_used_future_data = series.split_before(
                forecast_start_date)  # reduce size
        except Exception as e:
            # split not possible
            series = series
            # forecast start is most recent date
            forecast_start_date = series.end_time() + series.freq
            raise Warning("Forecast start date is not in the timeseries. Please choose a date between {} and {}. End of series is used as forecast start.".format(
                series.start_time(), series.end_time()))
    else:
        # forecast start is most recent date
        forecast_start_date = series.end_time() + series.freq
    #logger.info(f'forecast start date: {forecast_start_date}')

    # try to forecast with pretrained model which includes future covariates
    if use_pretrained_model:
        # neural methods perform better with scaled inputs
        scaler = Scaler()
        series_scaled = scaler.fit_transform(series)
        #logger.info(f'Scaled timeseries for better performance of Transformer model')
        if len(series_scaled) < input_length:
            raise ValueError(
                f"given series is too short, must be at least input_length {input_length} long, Prophet is used instead")

        # make prediction
        forecast = model.predict(
            n=horizon, future_covariates=covariates, series=series_scaled)
        # scale back
        forecast = scaler.inverse_transform(forecast) 
    else:
        # make prediction with prophet
        model.fit(series)
        forecast = model.predict(n=horizon)
        
    # metadata
    # input series range
    #logger.info(f"Input data range: {series.start_time()} - {series.end_time()}")
    start_date = series.end_time() - series.freq * input_length
    end_date = series.end_time()
    #logger.info(f"Model input range: {start_date} - {end_date}")
    #logger.info(f"Model output range: {forecast.start_time()} - {forecast.end_time()}")
    #logger.info(f'Done with forecast using {model.model_name}')

    # data type
    data_type = DOUBLE

    res = instantiate_forecast(forecast=forecast, dataIRI=dataIRI, model_name=model.model_name, forecast_input_length=input_length,
                               covariates=covariates, tsClient=tsClient, data_type=data_type, cov_iris=cov_iris, kgClient=kgClient)
    #logger.info('Done with instantiating forecast')
    return res

def load_pretrained_model(model_path_ckpt_link, model_path_pth_link):

    # try to load from checkpoint link
    path_ckpt = ""
    path_pth = ""
    path_to_store = Path(__file__).parent.absolute() / \
        'Model' / 'checkpoints'

    # until now we need to download both, checkpoint and model file
    # maybe you find a better way to just have one link
    # create folder
    if not os.path.exists(path_to_store):
        os.makedirs(path_to_store)
    if model_path_ckpt_link.startswith("https://"):
        # download checkpoint model
        path_ckpt, res = urllib.request.urlretrieve(
            model_path_ckpt_link, path_to_store / "best-model.ckpt")
        #logger.info(f'Downloaded checkpoint model from {model_path_ckpt_link} to {path_ckpt}')

    if model_path_pth_link.startswith("https://"):
        # download model
        path_pth, res = urllib.request.urlretrieve(
            model_path_pth_link, path_to_store.parent.absolute() / "_model.pth.tar")
        #logger.info(f'Downloaded model from {model_path_pth_link} to {path_pth}')

    # try to load model from downloaded checkpoint
    model = TFTModel.load_from_checkpoint(
        path_ckpt.parent.parent.__str__())
    model.model_name = model_path_ckpt_link.__str__()
    #logger.info(f'Loaded model from  {path_ckpt.parent.parent.__str__()}')

    # convert loaded model to device
    pl_trainer_kwargs = {"accelerator": 'cpu'}
    model.model_params['pl_trainer_kwargs'] = pl_trainer_kwargs
    model.trainer_params = pl_trainer_kwargs
    #logger.info(f'Moved model to device  {pl_trainer_kwargs["accelerator"]}')


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
    return {'forecast_iri': forecast_iri, 'model_name': model_name, 'v': forecast_input_length, 'covariates': cov_iris}
