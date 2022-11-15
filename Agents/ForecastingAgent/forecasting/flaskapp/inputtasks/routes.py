################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 12 Oct 2022                            #
################################################

from flask import Blueprint, request, jsonify
import traceback

#import agentlogging
import json
from forecasting.errorhandling.exceptions import InvalidInput
from forecasting.forecasting_agent.create_forecast import forecast

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
        force_mapping = query['force_mapping']
        print('force_mapping: ' + force_mapping)
    except KeyError as ex:
        force_mapping = None
    
    
    try:
        # Forecast dataIRI
        res = forecast(dataIRI, horizon, forecast_start_date, force_mapping)
        res['status'] = '200'
        return jsonify(res)
    except Exception as ex:
        #logger.("Unable to forecast.", ex)
        print(traceback.format_exc())
        return jsonify({'status': '500', 'msg': 'Forecast failed. \n' + str(ex)})

@inputtasks_bp.route("/api/forecastingAgent/help", methods=["GET"])
def forecastingAgent_help():
    msg = 'This is the help page of the forecasting agent. \n'
    msg += 'The forecasting agent can be used to forecast a time series. \n'
    msg += 'The following parameters are required: \n'
    msg += 'dataIRI: the IRI of the time series to be forecasted \n'
    msg += 'horizon: the number of steps to forecast \n\n'
    msg += 'The following parameters are optional: \n'
    msg += 'forecast_start_date: the date from which to start the forecast (optional, if not provided the most recent date will be used) \n'
    msg += 'data_length: the length of the time series which is loaded as input. This determines two things:\n'
    msg += '1. For the prophet model, this is the length of the time series which is used to fit the model. \n'
    msg += '2. For the pretrained models, this is the length of the time series which is used to scale the data. \n'
    msg += 'The pretrained models use their own input length parameter (for TFT "input_chunk_length") as data length for the forecast.\n'
    msg += 'The default value for data_length is stored in forecast_properties.py and can be changed there. \n'
    msg += 'The default value is: ' + str(DEFAULT_DATA_LENGTH) + '\n'
    

    res = {'status': '200', 'msg': msg}
    return jsonify(res)