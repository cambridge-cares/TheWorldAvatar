import pandas as pd
from flask import Blueprint, request, jsonify

import agentlogging
from agent.calculation.calculation import *
from agent.errorhandling.exceptions import InvalidInput

# Initialise logger
logger = agentlogging.get_logger("prod")

calculationtask_bp = Blueprint(
    'calculationtask_bp', __name__
)

# Define route for API request to retrieve station and reading data and create
# output files for Digital Twin Visualisation Framework
# All query parameters are expected as SINGLE JSON object 'query' (to follow
# the convention introduced in the JPS_BASE_LIB)
@calculationtask_bp.route('/api/lsoacalculationagent_inequality_index/calculation/inequality_index', methods=['GET'])
def api_calculation_fuel_cost():
    #
    # Check arguments (query parameters)
    #
    inputs = {'df_change_of_cost':None,
             'df_fuel_poverty':None
    }

        # Get received 'query' JSON object which holds all parameters
    logger.info("Checking arguments...")
    try:
        query = request.json['query']
    except Exception as ex:
        logger.error('No JSON "query" object could be identified.')
        raise InvalidInput('No JSON "query" object could be identified.') from ex

        # Get df_change_of_cost
    try:
        inputs['df_change_of_cost'] = pd.DataFrame.from_dict(query['df_change_of_cost'])
    except Exception as ex:
        logger.error('Required df_change_of_cost could not be determined.')
        raise InvalidInput('Required df_change_of_cost could not be determined.') from ex

        # Get df_fuel_poverty
    try:
        inputs['df_fuel_poverty'] = pd.DataFrame.from_dict(query['df_fuel_poverty'])
    except Exception as ex:
        logger.error('Required df_fuel_poverty could not be determined.')
        raise InvalidInput('Required df_fuel_poverty could not be determined.') from ex

    if 'min change of cost nth-percentile' in query.keys():
        min_dc = float(query['min change of cost nth-percentile'])
        logger.info(f'Specify min change of cost as {min_dc}th-percentile')
    else:
        min_dc = 1
        logger.info('min change of cost as default value: 1th-percentile')

    if 'max change of cost nth-percentile' in query.keys():
        max_dc = float(query['max change of cost nth-percentile'])
        logger.info(f'Specify max change of cost as {max_dc}th-percentile')
    else:
        max_dc = 99
        logger.info('max change of cost as default value: 99th-percentile')

    if 'min fuel poverty' in query.keys():
        min_fp = float(query['min fuel poverty'])
        logger.info(f'Specify min fuel poverty as {min_fp}')
    else:
        min_fp = 0
        logger.info('min fuel poverty as default value: 0')

    if 'max fuel poverty' in query.keys():
        max_fp = float(query['max fuel poverty'])
        logger.info(f'Specify max fuel poverty as {max_fp}')
    else:
        max_fp = 0.2
        logger.info('max fuel poverty as default value: 0.2')

    try: 
        df_index = calculate_df(inputs['df_change_of_cost'],inputs['df_fuel_poverty'], \
                                        min_dc, max_dc, min_fp, max_fp)
        
        output = {'df_inequality_index': df_index.to_dict(orient='list')
                }

    except Exception as ex:
        print(ex)
        output = {"status": '500', 'errormsg': 'Retrieving outputs failed'}
    
    return jsonify(output)