################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 12 Oct 2022                            #
################################################

from flask import Blueprint, request, jsonify

#import agentlogging

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
    except Exception as ex:
        #logger.('No JSON "query" object could be identified.')
        raise InvalidInput('No JSON "query" object could be identified.') from ex
    
    # Retrieve data IRI to be updated
    try:
        dataIRI = str(query['dataIRI'])
        
    except Exception as ex:
        #logger.('Invalid "dataIRI" provided.')
        raise InvalidInput('Invalid "dataIRI" provided.') from ex
    
    # Retrieve horizon 
    try:
        horizon = int(query['horizon'])
    except Exception as ex:
        #logger.info('No horizon, using default.')
        horizon = 7
    if horizon <= 0:
        #logger.('Invalid "horizon" provided. Must be higher than 0.')
        raise InvalidInput('Invalid "horizon" provided. Must be higher than 0.')
    
    # Retrieve forecast_start_date 
    try:
        forecast_start_date = query['forecast_start_date']
    except Exception as ex:
        #logger.info('No forecast_start_date, using most recent date.')
        forecast_start_date = None
    
    # Retrieve model_path_ckpt_link 
    try:
        model_path_ckpt_link = query['model_path_ckpt_link']
    except Exception as ex:
        #logger.info('No model_path_ckpt_link, using Prophet.')
        model_path_ckpt_link = ''
    
    
    # Retrieve model_path_pth_link 
    try:
        model_path_pth_link = query['model_path_pth_link']
    except Exception as ex:
        #logger.info('No model_path_pth_link, using Prophet.')
        model_path_pth_link = ''
    
    
    try:
        # Forecast dataIRI
        forecast(dataIRI, horizon, forecast_start_date, model_path_ckpt_link, model_path_pth_link)
        return jsonify({'status': '200'})
    except Exception as ex:
        #logger.("Unable to forecast.", ex)
        return jsonify({'status': '500', 'msg': 'Forecast failed. \n' + str(ex)})

