################################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk)   #
# Date: 30 Nov 2022                            #
################################################


from flask import Blueprint, request, jsonify
import traceback

#import agentlogging
import json
from forecasting.errorhandling.exceptions import InvalidInput
from forecasting.forecasting_agent.agent import forecast

# Initialise logger
#logger = agentlogging.get_logger("prod")


inputtasks_bp = Blueprint(
    'inputtasks_bp', __name__
)


# Define route for API to forecast
@inputtasks_bp.route("/api/forecastingAgent/forecast", methods=["POST"])
def api_forecast():
    # Get received 'query' JSON object which holds all HTTP parameters
    try:
        query = request.json["query"]
        print('recieved query - start to extract data')
    except Exception as ex:
        #logger.('No JSON "query" object could be identified.')
        raise InvalidInput('No JSON "query" object could be identified.') from ex
    
    # Retrieve data IRI to be updated
    try:
        iri = str(query['iri'])
        # remove < and > from iri
        if iri.startswith('<'):
            iri = iri[1:]
        if iri.endswith('>'):
            iri = iri[:-1]
        print('iri: ' + iri)
    except Exception as ex:
        #logger.('Invalid "iri" provided.')
        raise InvalidInput('"iri" must be provided.') from ex
    
    # Retrieve horizon 
    try:
        horizon = int(query['horizon'])
        print('horizon: ' + str(horizon))
    except Exception as ex:
        #logger.info('No horizon, using default.')
        raise InvalidInput('"horizon" (how many steps to forecast) must be provided.') from ex

    if horizon <= 0:
        #logger.('Invalid "horizon" provided. Must be higher than 0.')
        raise InvalidInput('Invalid "horizon" provided. Must be higher than 0.')
    
    # Retrieve forecast_start_date 
    try:
        forecast_start_date = query['forecast_start_date']
        print('forecast_start_date: ' + forecast_start_date)
    except KeyError as ex:
        #logger.info('No forecast_start_date, using most recent date.')
        # use last available date as forecast_start_date
        forecast_start_date = None
    
    # Retrieve if specific model configuration should be foreced
    try:
        use_model_configuration = query['use_model_configuration']
        print('use_model_configuration: ' + use_model_configuration)
    except KeyError as ex:
        use_model_configuration = None
    
    # Retrieve data_length 
    try:
        data_length = int(query['data_length'])
        print('data_length: ' + str(data_length))
    except KeyError as ex:
        data_length = None
        
    try:
        # Forecast iri
        res = forecast(iri, horizon, forecast_start_date, use_model_configuration, data_length = data_length)
        res['status'] = '200'
        return jsonify(res)
    except Exception as ex:
        #logger.("Unable to forecast.", ex)
        print(traceback.format_exc())
        return jsonify({'status': '500', 'msg': 'Forecast failed. \n' + str(ex)})

