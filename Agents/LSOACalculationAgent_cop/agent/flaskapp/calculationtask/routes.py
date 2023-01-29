import numpy as np
import datetime as dt
import json
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
             'lsoa_sequence':None
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

    try: 
        if inputs['unit'] == OM_DEGREE_C:
            cop = COP_degreeC(inputs['temperature'])
        if inputs['unit'] == OM_KELVIN:
            cop = COP_kelvin(inputs['temperature'])
        if inputs['unit'] == OM_FAHRENHEIT:
            cop = COP_Fahrenheit(inputs['temperature'])
        # Get lsoa_sequence
        if 'lsoa_sequence' in query:
            inputs['lsoa_sequence'] = np.array(query['lsoa_sequence'])
            output = {'COP': cop.tolist(),
                    'lsoa_sequence':inputs['lsoa_sequence'].tolist()
                }
        else:
            logger.warning('No lsoa sequence provided! the input temperature is \'blind\' at the moment (temperature have no reference to which lsoa area)')
            output = {'COP': cop.tolist()
                }
            
    except Exception as ex:
        print(ex)
        output = {"status": '500', 'errormsg': 'Retrieving outputs failed'}
    
    return jsonify(output)