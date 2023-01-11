################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

from flask import Blueprint, request, jsonify

import agentlogging
from agent.datainstantiation.readings import upload_all,upload_data_to_KG,upload_timeseries_to_KG

# Initialise logger
logger = agentlogging.get_logger("prod")


inputtasks_bp = Blueprint(
    'inputtasks_bp', __name__
)

# Define route for API request to instantiate all readings
@inputtasks_bp.route('/api/electricityconsumptionagent/instantiate/readings', methods=['GET'])
def api_instantiate_readings():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        #print("Query parameters provided, although not required. " \
        #      + "Provided arguments will be neglected.")
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Instantiate all
        len_query, _, _ = upload_data_to_KG()
        print(f'Number of instantiated LSOA area:{len_query}')
        return jsonify({"LSOA areas": len_query})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Instantiation failed'})
