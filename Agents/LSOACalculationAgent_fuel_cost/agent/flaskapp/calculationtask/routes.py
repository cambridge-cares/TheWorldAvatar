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
@calculationtask_bp.route('/api/lsoacalculationagent_fuel_cost/calculation/fuel_cost', methods=['GET'])
def api_calculation_cop():
    #
    # Check arguments (query parameters)
    #
    inputs = {'df_electricity':None,
             'df_gas':None,
             'year':None,
             'annual':None
    }

        # Get received 'query' JSON object which holds all parameters
    logger.info("Checking arguments...")
    try:
        query = request.json['query']
    except Exception as ex:
        logger.error('No JSON "query" object could be identified.')
        raise InvalidInput('No JSON "query" object could be identified.') from ex

        # Get df_electricity
    if 'df_electricity' in query.keys():
        inputs['df_electricity'] = pd.DataFrame.from_dict(query['df_electricity'])
        logger.info('Electricity consumption data recieved, only cost of electricity will be returned...')

        # Get df_gas
    if 'df_gas' in query.keys():
        inputs['df_gas'] = pd.DataFrame.from_dict(query['df_gas'])
        logger.info('Gas consumption data recieved, only cost of electricity will be returned...')
    
        # Make sure at least one consumption is given otherwise you are kidding me
    if 'df_gas' not in query.keys() and 'df_electricity' not in query.keys():
        logger.error('Both Gas and Eleciricty consumption data are not provided! No cost to determine...')
        raise InvalidInput('Both Gas and Eleciricty consumption data are not provided! No cost to determine...')

        # Get year
    try:
        inputs['year'] = str(query['year'])
    except Exception as ex:
        logger.error('Required year could not be determined.')
        raise InvalidInput('Required year could not be determined.') from ex

    if 'annual' in query.keys():
        inputs['annual'] = bool(query['annual'])
        logger.info('Annual data will be returned as per specified in query')
    else:
        inputs['annual'] = False
        logger.info('No Annual data will be returned as per specified in query')


    try: 
        if 'df_gas' in query.keys() and 'df_electricity' in query.keys():
            df_cost_total, df_cost_elec, df_cost_gas = calculating_fuel_cost(inputs['df_electricity'],inputs['df_gas'], \
                                                                            inputs['year'], inputs['annual'] )
            
            output = {'df_cost_total': df_cost_total.to_dict(orient='list'),
                    'df_cost_elec': df_cost_elec.to_dict(orient='list'),
                    'df_cost_gas': df_cost_gas.to_dict(orient='list'),
                    }
        if 'df_gas' in query.keys() and 'df_electricity' not in query.keys():
            df_cost_gas = calculating_single_cost_gas(inputs['df_gas'],inputs['year'], inputs['annual'] )
            output = {
                    'df_cost_gas': df_cost_gas.to_dict(orient='list')
                    }
        if 'df_gas' not in query.keys() and 'df_electricity' in query.keys():
            df_cost_elec = calculating_single_cost_elec(inputs['df_electricity'],inputs['year'], inputs['annual'] )
            output = {
                    'df_cost_elec': df_cost_elec.to_dict(orient='list')
                    }

    except Exception as ex:
        print(ex)
        output = {"status": '500', 'errormsg': 'Retrieving outputs failed'}
    
    return jsonify(output)