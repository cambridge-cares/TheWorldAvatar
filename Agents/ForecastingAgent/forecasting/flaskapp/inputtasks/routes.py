################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 12 Oct 2022                            #
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


# Define route for API request to update transaction record for single property
# or list of provided properties
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
        dataIRI = str(query['dataIRI'])
        # remove < and > from dataIRI
        if dataIRI.startswith('<'):
            dataIRI = dataIRI[1:]
        if dataIRI.endswith('>'):
            dataIRI = dataIRI[:-1]
        print('dataIRI: ' + dataIRI)
    except Exception as ex:
        #logger.('Invalid "dataIRI" provided.')
        raise InvalidInput('"dataIRI" must be provided.') from ex
    
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
    
    # Retrieve if specific mapping should be foreced
    try:
        force_configuration = query['force_configuration']
        print('force_configuration: ' + force_configuration)
    except KeyError as ex:
        force_configuration = None
    
    # Retrieve data_length 
    try:
        data_length = int(query['data_length'])
        print('data_length: ' + str(data_length))
    except KeyError as ex:
        data_length = None
        
    try:
        # Forecast dataIRI
        res = forecast(dataIRI, horizon, forecast_start_date, force_configuration, data_length = data_length)
        res['status'] = '200'
        return jsonify(res)
    except Exception as ex:
        #logger.("Unable to forecast.", ex)
        print(traceback.format_exc())
        return jsonify({'status': '500', 'msg': 'Forecast failed. \n' + str(ex)})

