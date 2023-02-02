import numpy as np
from flask import Blueprint, request, jsonify

import agentlogging
from agent.errorhandling.exceptions import InvalidInput
from agent.datamodel.iris import *
from agent.calculation.calculation import COP_degreeC,COP_kelvin,COP_Fahrenheit

# Initialise logger
logger = agentlogging.get_logger("prod")

calculationtask_bp = Blueprint(
    'calculationtask_bp', __name__
)

# Define route for API request to retrieve station and reading data and create
# output files for Digital Twin Visualisation Framework
# All query parameters are expected as SINGLE JSON object 'query' (to follow
# the convention introduced in the JPS_BASE_LIB)
@calculationtask_bp.route('/api/lsoacalculationagent_cop/calculation/cop', methods=['GET'])
def api_calculation_cop():
    #
    # Check arguments (query parameters)
    #
    inputs = {'temperature':None,
             'unit':None,
    }

        # Get received 'query' JSON object which holds all parameters
    logger.info("Checking arguments...")
    try:
        query = request.json['query']
    except Exception as ex:
        logger.error('No JSON "query" object could be identified.')
        raise InvalidInput('No JSON "query" object could be identified.') from ex

        # Get temperature
    try:
        inputs['temperature'] = np.array(query['temperature'])
    except Exception as ex:
        logger.error('Required temperature could not be determined.')
        raise InvalidInput('Required temperature could not be determined.') from ex

        # Get unit
    try:
        inputs['unit'] = str(query['unit'])
    except Exception as ex:
        logger.error('Required unit could not be determined.')
        raise InvalidInput('Required unit could not be determined.') from ex
    
    if 't_h' in query.keys():
        t_h = float(query['t_h'])
    else:
        t_h = 318.15 

    if 'hp_efficiency' in query.keys():
        hp_efficiency = float(query['hp_efficiency'])
    else:
        hp_efficiency = 0.35

    try: 
        if inputs['unit'] == OM_DEGREE_C:
            cop = COP_degreeC(inputs['temperature'],hp_efficiency,t_h)
        if inputs['unit'] == OM_KELVIN:
            cop = COP_kelvin(inputs['temperature'],hp_efficiency,t_h)
        if inputs['unit'] == OM_FAHRENHEIT:
            cop = COP_Fahrenheit(inputs['temperature'],hp_efficiency,t_h)
        output = {'COP': cop.tolist()
                }
            
    except Exception as ex:
        print(ex)
        output = {"status": '500', 'errormsg': 'Retrieving outputs failed'}
    
    return jsonify(output)