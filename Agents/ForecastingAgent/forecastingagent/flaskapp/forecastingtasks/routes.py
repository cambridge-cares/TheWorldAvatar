################################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk)   #
#          Markus Hofmeister (mh807@cam.ac.uk) #
################################################

# The purpose of this module is to provide all routes to interact with the
# Forecasting Agent Flask App

import traceback
from flask import Blueprint, request, jsonify

from forecastingagent.agent.agent import forecast
from forecastingagent.utils.default_configs import STACK_NAME, NAMESPACE, DATABASE, \
                                              DB_URL, DB_USER, DB_PASSWORD, \
                                              QUERY_ENDPOINT, UPDATE_ENDPOINT
from forecastingagent.utils.stack_configs import retrieve_stack_settings                                              
from forecastingagent.errorhandling.exceptions import InvalidInput

from py4jps import agentlogging

# Initialise logger instance (ensure consistent logger level`)
#logger = agentlogging.get_logger('dev')
logger = agentlogging.get_logger('prod')


forecastingtasks_bp = Blueprint(
    ' forecastingtasks_bp', __name__
)


# Define route for API to forecast
@forecastingtasks_bp.route("/forecast", methods=["POST"])
def api_forecast():
    # Get received 'query' JSON object which holds all HTTP parameters
    try:
        query = request.json["query"]
        logger.info('Received query, starting to extract data...')
    except Exception as ex:
        logger.error('No JSON "query" object could be identified.')
        return jsonify({'status': '500', 'msg': 'No JSON "query" object could be identified.'}), 500
    
    #
    ### Retrieve KG and RDB connection parameters ###
    #
    # Retrieve parameters from HTTP request
    http_conn_params = {}
    http_conn_params['namespace'] = query.get('namespace')
    http_conn_params['database'] = query.get('database')
    http_conn_params['query_endpoint'] = query.get('query_endpoint')
    http_conn_params['update_endpoint'] = query.get('update_endpoint')
    http_conn_params['db_url'] = query.get('db_url')
    http_conn_params['db_user'] = query.get('db_user')
    http_conn_params['db_password'] = query.get('db_password')
    
    # Ensure that required connection parameters are provided, 
    # either in HTTP request or by default values in docker-compose file
    try:
        conn_params = validate_connection_parameters(http_conn_params)
        logger.info('Retrieved connection parameters for current forecast:')
        for key, value in conn_params.items():
            logger.info(f'{key}: {value}')
    except InvalidInput as ex:
        logger.error("Missing connection parameters to process forecast.", ex)
        return jsonify({'status': '500', 'msg': 'Missing connection parameters to process forecast: ' + str(ex)}), 500

    #
    ### Retrieve forecast parameters ###
    #
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

    # Execute forecast
    try:
        # Forecast iri
        res = forecast(iri, horizon, forecast_start_date, use_model_configuration, data_length=data_length,
                       **conn_params)
        res['status'] = '200'
        logger.info('forecasting successful')
        return jsonify(res)
    except Exception as ex:
        logger.error("Unable to forecast.", ex)
        logger.error(traceback.format_exc())
        return jsonify({'status': '500', 'msg': 'Forecast failed: ' + str(ex)}), 500


def validate_connection_parameters(provided_http_parameters):
    """
        Verify that all required connection parameters have been provided,
        either by defaults in environment variables or by HTTP request parameters
    """
    # Initialise connection parameters to use
    conn_params = {}
    stack_params = {}

    # 1) Standalone deployment
    if STACK_NAME == '':
        relevant = ['query_endpoint', 'update_endpoint', 'db_url', 'db_user', 'db_password']
        for r in relevant:
            # Retrieve parameter from HTTP request if provided ...
            if provided_http_parameters[r] is not None:
                conn_params[r] = str(provided_http_parameters[r])
            else:
                # ... otherwise use default value
                default = eval(r.upper())
                if not default:                    
                    # In case no default has been provided, raise exception
                    logger.error(f'No "{r}" value provided in HTTP request despite missing default value.')
                    raise InvalidInput(f'No "{r}" value provided in HTTP request, despite missing default value.')
                else:
                    conn_params[r] = default

    # 2) Stack deployment
    else:
        relevant = ['namespace', 'database']
        for r in relevant:
            # Retrieve parameter from HTTP request if provided ...
            if provided_http_parameters[r] is not None:
                stack_params[r] = str(provided_http_parameters[r])
            else:
                # ... otherwise use default value
                default = eval(r.upper())
                if not default:
                    # In case no default has been provided, raise exception          
                    logger.error(f'No "{r}" value provided in HTTP request despite missing default value.')
                    raise InvalidInput(f'No "{r}" value provided in HTTP request, despite missing default value.')
                else:
                    stack_params[r] = default
        # Retrieve connection parameters from stack clients (returned as:
        # DB_URL, DB_USER, DB_PASSWORD, QUERY_ENDPOINT, UPDATE_ENDPOINT)
        stack_conn_param = retrieve_stack_settings(**stack_params)

        # Construct conn_params
        param_oder = ['db_url', 'db_user', 'db_password', 'query_endpoint', 'update_endpoint']
        conn_params = dict(zip(param_oder, stack_conn_param))

    return conn_params
