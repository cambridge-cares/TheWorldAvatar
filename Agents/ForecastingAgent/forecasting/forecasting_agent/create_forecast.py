################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 12 Oct 2022                            #
################################################

# The purpose of this module is to instantiate/update data retrieved from
# the HM Land Registry Open Data SPARQL endpoint according to OntoBuiltEnv

import re
import json
import uuid
from forecasting.utils.useful_queries import *
from forecasting.utils.tools import *
import urllib

from darts.dataprocessing.transformers import Scaler
import time
import sys
from darts.models import TFTModel, Prophet
import datetime as dt
import os
from darts import TimeSeries
import numpy as np
import pandas as pd
from darts import concatenate
import pandas as pd
from fuzzywuzzy import fuzz, process

import agentlogging
from forecasting.kgutils.javagateway import jpsBaseLibGW
from forecasting.datamodel.iris import *
from forecasting.datamodel.data_mapping import *
from forecasting.datamodel.data_mapping import TIME_FORMAT, DATACLASS
from forecasting.errorhandling.exceptions import KGException
from forecasting.kgutils.kgclient import KGClient
from forecasting.kgutils.tsclient import TSClient
from forecasting.utils.api_endpoints import HM_SPARQL_ENDPOINT
from forecasting.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT
from forecasting.kgutils.querytemplates import *

# Initialise logger
logger = agentlogging.get_logger("prod")




def forecast(dataIRI, horizon = 24 *7, forecast_start_date = None, model_path_ckpt_link  = None, model_path_pth_link = None):
    # initialise the  client
    kgClient = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)
    tsClient = TSClient(kg_client=kgClient)
    
    # identify the dataIRI
    predecessor_type = get_predecessor_type_by_predicate(dataIRI, OHN_HASHEATDEMAND)
    

    # use mapping function to get the correct dataIRI timeseries and its covariates
    logger.info('Retrieving instantiated timeseries for dataIRI ...')
    df, cov_iris, covariates = mapping_type_data_function[predecessor_type](dataIRI)

    # remove timezone
    df.Date = pd.to_datetime(df.Date).dt.tz_localize(None)
    
    # create darts TimeSeries object
    series = TimeSeries.from_dataframe(df, time_col = 'Date', value_cols =  "ForecastColumn" ) #, fill_missing_dates =True

    # remove nan values at beginning and end
    series = series.strip()

    # split series at forecast_start_date if given
    if forecast_start_date is not None:
        if isinstance(forecast_start_date, str):
            forecast_start_date = pd.Timestamp(dt.datetime.strptime(forecast_start_date, "%Y-%m-%d %H:%M:%S")) #.timestamp()
        try:
            series, not_used_future_data = series.split_before(forecast_start_date) # reduce size
        except:
            # split not possible
            series = series
    else:
        # forecast start is most recent date
        forecast_start_date = series.end_time() + series.freq
    logger.info(f'forecast start date: {forecast_start_date}')


    # try to forecast with pretrained model
    try:
        # check that covariates are given fore forecasting horizon
        if covariates is None or (covariates.end_time() - horizon * covariates.freq) < forecast_start_date:
            raise ValueError(f"given covariates are too short or None, must be at least {horizon} longer than end of target series from  {series.end_time()} to {series.end_time() + horizon * covariates.freq}")

        # try to load from checkpoint link
        path_ckpt = ""
        path_pth = ""
        path_to_store = Path(__file__).parent.absolute() / 'Model' / 'checkpoints'
        
        # until now we need to download both, checkpoint and model file
        # maybe you find a better way to just have one link
        # create folder
        if not os.path.exists(path_to_store):
            os.makedirs(path_to_store)
        if model_path_ckpt_link.startswith("https://"):
            # download checkpoint model
            path_ckpt, res = urllib.request.urlretrieve(model_path_ckpt_link, path_to_store / "best-model.ckpt")
            logger.info(f'Downloaded checkpoint model from {model_path_ckpt_link} to {path_ckpt}')

        if model_path_pth_link.startswith("https://"):
            # download model
            path_pth, res = urllib.request.urlretrieve(model_path_pth_link, path_to_store.parent.absolute() / "_model.pth.tar")
            logger.info(f'Downloaded model from {model_path_pth_link} to {path_pth}')
            
    
        # try to load model from downloaded checkpoint
        model = TFTModel.load_from_checkpoint(path_ckpt.parent.parent.__str__())
        model.model_name = model_path_ckpt_link.__str__()
        logger.info(f'Loaded model from  {path_ckpt.parent.parent.__str__()}')

        # convert loaded model to device
        pl_trainer_kwargs = {"accelerator": 'cpu'}
        model.model_params['pl_trainer_kwargs'] = pl_trainer_kwargs
        model.trainer_params = pl_trainer_kwargs
        logger.info(f'Moved model to device  {pl_trainer_kwargs["accelerator"]}')

        # neural methods perform better with scaled inputs
        scaler = Scaler()
        series_scaled = scaler.fit_transform(series)
        logger.info(f'Scaled timeseries for better performance of Transformer model')

        # check lenght
        input_length = model.model_params['input_chunk_length']
        if len(series_scaled) < input_length:
            raise ValueError(f"given series is too short, must be at least input_length {input_length} long, Prophet is used instead")

        # make prediction
        forecast = model.predict(n=horizon, future_covariates=covariates, series = series_scaled)

        forecast = scaler.inverse_transform(forecast) #None
        #model = TFTModel.load_from_checkpoint(pretrained)

    
    except Exception as e:
        # if pretrained model fails, use prophet
        logger.info(f"Could not forecast with pretrained model. Prophet is used. \nError: {e}")
        model = Prophet()
        model.model_name = "Prophet"
        model.fit(series)
        input_length = len(series)  
        forecast = model.predict(n=horizon)   
    

    # metadata
    # input series range
    logger.info(f"Input data range: {series.start_time()} - {series.end_time()}")
    start_date = series.end_time() - series.freq * input_length
    end_date = series.end_time()
    logger.info(f"Model input range: {start_date} - {end_date}")
    logger.info(f"Model output range: {forecast.start_time()} - {forecast.end_time()}")
    logger.info(f'Done with forecast using {model.model_name}')
    
    
    # data type
    data_type = DOUBLE

    update = instantiate_forecast(forecast, dataIRI, model.model_name, input_length, covariates, tsClient, data_type, cov_iris)
    kgClient.performUpdate(add_insert_data(update))
    logger.info('Done with instantiating forecast')



def instantiate_forecast(forecast, dataIRI, model_name, forecast_input_length, covariates, tsClient, data_type, covs):
    #  instantiate forecast in KG 
    forecast_iri = KB + 'Forecast_' + str(uuid.uuid4())
    update = ""

    # get unit from dataIRI and add to forecast
    unit = get_unit(dataIRI)
    time_format = get_time_format(dataIRI)
    
    ONTOEMS_HASFORECASTINPUTLENGHT = ONTOEMS + "hasForecastInputLength"
    ONTOEMS_HASCOVARIATE = ONTOEMS + "hasCovariate"
    ONTOEMS_HASFORECASTMODEL = ONTOEMS + "hasForecastModel"

    update += get_properties_for_subj(subj=forecast_iri, verb_obj={
        RDF_TYPE: ONTOEMS_FORECAST,
        OM_HASUNIT: unit, 
        ONTOEMS_HASCOVARIATE: covs,
    }, verb_literal={
        ONTOEMS_HASFORECASTMODEL: model_name,
        ONTOEMS_HASFORECASTINPUTLENGHT: forecast_input_length,
        })
    
    # call client
    tsClient.tsclient.initTimeSeries([forecast_iri], [data_type], time_format,
                                        tsClient.conn)
    ts = TSClient.create_timeseries([str(x) for x in forecast.time_index], [forecast_iri], [forecast.values().squeeze().tolist()])
    tsClient.tsclient.addTimeSeriesData(ts, tsClient.conn)

    update += get_properties_for_subj(subj=dataIRI, verb_obj={
                    ONTOEMS_HASFORECASTEDVALUE: forecast_iri})

    return update