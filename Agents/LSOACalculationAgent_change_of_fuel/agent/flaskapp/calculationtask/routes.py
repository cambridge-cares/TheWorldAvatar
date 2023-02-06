import numpy as np
from flask import Blueprint, request, jsonify

import agentlogging
from agent.errorhandling.exceptions import InvalidInput
from agent.calculation.calculation import delta_elec, delta_gas

# Initialise logger
logger = agentlogging.get_logger("prod")

calculationtask_bp = Blueprint(
    'calculationtask_bp', __name__
)

# Define route for API request to retrieve station and reading data and create
# output files for Digital Twin Visualisation Framework
# All query parameters are expected as SINGLE JSON object 'query' (to follow
# the convention introduced in the JPS_BASE_LIB)
@calculationtask_bp.route('/api/lsoacalculationagent_change_of_fuel/calculation/change_of_fuel', methods=['GET'])
def api_calculation_cop():
    #
    # Check arguments (query parameters)
    #
    inputs = {'uptake':None,
             'gas consumption':None
    }

        # Get received 'query' JSON object which holds all parameters
    logger.info("Checking arguments...")
    try:
        query = request.json['query']
    except Exception as ex:
        logger.error('No JSON "query" object could be identified.')
        raise InvalidInput('No JSON "query" object could be identified.') from ex

        # Get uptake
    try:
        inputs['uptake'] = float(query['uptake'])
    except Exception as ex:
        logger.error('Required uptake could not be determined.')
        raise InvalidInput('Required uptake could not be determined.') from ex

        # Get gas consumption
    try:
        inputs['gas consumption'] = np.array(query['gas consumption'])
    except Exception as ex:
        logger.error('Required gas consumption could not be determined.')
        raise InvalidInput('Required gas consumption could not be determined.') from ex
    
    if 'propotion of heating' in query.keys():
        propotion_heating = float(query['propotion of heating'])
    else:
        propotion_heating = 0.9

    if 'boiler efficiency' in query.keys():
        boiler_efficiency = float(query['boiler_efficiency'])
    else:
        boiler_efficiency = 0.8

    try: 
        change_of_gas = delta_gas(inputs['uptake'],inputs['gas consumption'],propotion_heating)
        output = {'change of gas': change_of_gas.tolist()
                }
        if 'cop' in query.keys():
            cop = np.array(query['cop'])
            # check the shape of change of cop and change of gas
            if not change_of_gas.shape == cop.shape:
                logger.error('array of cop have different shape comparing to the input gas consumption array!')
                raise InvalidInput('array of cop have different shape comparing to the input gas consumption array!')
            change_of_electricity = delta_elec(change_of_gas, cop, boiler_efficiency)
            output['change of electricity'] = change_of_electricity.tolist()
    
    except Exception as ex:
        print(ex)
        output = {"status": '500', 'errormsg': 'Retrieving outputs failed'}
    
    return jsonify(output)