################################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk)   #
#          Markus Hofmeister (mh807@cam.ac.uk) #
################################################

# The purpose of this module is to provide all routes to interact with the
# Forecasting Agent Flask App

import traceback
from flask import Blueprint, request, jsonify

from py4jps import agentlogging

from forecasting.forecasting_agent.agent import forecast

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


forecastingtasks_bp = Blueprint(
    ' forecastingtasks_bp', __name__
)


# Define route for API to forecast
@forecastingtasks_bp.route("/api/forecastingAgent/forecast", methods=["POST"])
def api_forecast():
    # Get received 'query' JSON object which holds all HTTP parameters
    try:
        query = request.json["query"]
        logger.info('recieved query - start to extract data')
    except Exception as ex:
        logger.error('No JSON "query" object could be identified.')
        return jsonify({'status': '500', 'msg': 'No JSON "query" object could be identified.'}), 500
    # Retrieve data IRI to be updated
    try:
        iri = str(query['iri'])
        # remove < and > from iri
        if iri.startswith('<'):
            iri = iri[1:]
        if iri.endswith('>'):
            iri = iri[:-1]
        logger.info('iri: ' + iri)
    except Exception as ex:
        logger.error('No "iri" provided.')
        return jsonify({'status': '500', 'msg': '"iri" must be provided.'}), 500
    
    # Retrieve horizon 
    try:
        horizon = int(query['horizon'])
        logger.info('horizon: ' + str(horizon))
    except Exception as ex:
        logger.error('No "horizon" provided.')
        return jsonify({'status': '500', 'msg': '"horizon" (how many steps to forecast) must be provided.'}), 500

    if horizon <= 0:
        logger.error('Invalid "horizon" provided. Must be higher than 0.')
        return jsonify({'status': '500', 'msg': 'Invalid "horizon" provided. Must be higher than 0.'}), 500        
    
    # Retrieve forecast_start_date 
    try:
        forecast_start_date = query['forecast_start_date']
        logger.info('forecast_start_date: ' + forecast_start_date)
    except KeyError as ex:
        logger.warning('No forecast_start_date, using most recent date.')
        # use last available date as forecast_start_date
        forecast_start_date = None
    
    # Retrieve if specific model configuration should be foreced
    try:
        use_model_configuration = query['use_model_configuration']
        logger.info('use_model_configuration: ' + use_model_configuration)
    except KeyError as ex:
        logger.warning('No use_model_configuration, using "DEFAULT".')

        use_model_configuration = None
    
    # Retrieve data_length 
    try:
        data_length = int(query['data_length'])
        logger.info('data_length: ' + str(data_length))
    except KeyError as ex:
        logger.warning('No data_length, using data_length from "DEFAULT" configuration.')

        data_length = None

    # Retrieve KG and RDB settings
    endpoints = {}
    endpoints['query_endpoint'] = query.get('query_endpoint')
    endpoints['update_endpoint'] = query.get('update_endpoint')
    endpoints['rdb_url'] = query.get('rdb_url')
    endpoints['rdb_user'] = query.get('rdb_user')
    endpoints['rdb_password'] = query.get('rdb_password')
    # Remove None values
    given_endpoints = {k: v for k, v in endpoints.items() if v is not None}


    try:
        # Forecast iri
        res = forecast(iri, horizon, forecast_start_date, use_model_configuration, data_length=data_length,
                       **given_endpoints)
        res['status'] = '200'
        logger.info('forecasting successful')
        return jsonify(res)
    except Exception as ex:
        logger.error("Unable to forecast.", ex)
        logger.error(traceback.format_exc())
        return jsonify({'status': '500', 'msg': 'Forecast failed. \n' + str(ex)}), 500

