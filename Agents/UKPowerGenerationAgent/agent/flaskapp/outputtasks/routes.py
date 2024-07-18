################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 14 Apr 2022                            #
######################################~#########

import pathlib
import datetime as dt
from flask import Blueprint, request, jsonify

from py4jps import agentlogging
from agent.dataretrieval.generator_data_query import query_all_generator_data
from agent.utils.readings_mapping import TIME_FORMAT
from agent.errorhandling.exceptions import InvalidInput

# Initialise logger
logger = agentlogging.get_logger("prod")


outputtasks_bp = Blueprint(
    'outputtasks_bp', __name__
)

# Defines route for API request to query for retrieving data about generators
# and create output files for Digital Twin Visualisation Framework.
# All query parameters are expected as SINGLE JSON object 'query' (to follow
# the convention introduced in the JPS_BASE_LIB)
@outputtasks_bp.route('/api/ukpowergenerationagent/retrieve/all', methods=['GET'])
def api_retrieve_all_generators():
    """
    Creates DTVF-compatiable output files
    """
    try:
        # Instantiate stations
        query_all_generator_data()
        print("Output files written")
        return jsonify({"status": '200', "msg": "success"})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Retrieving outputs failed'})
