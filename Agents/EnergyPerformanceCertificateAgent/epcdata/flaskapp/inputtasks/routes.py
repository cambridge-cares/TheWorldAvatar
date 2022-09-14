################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 08 Apr 2022                            #
################################################

from flask import Blueprint, request, jsonify

import agentlogging
from epcdata.kgutils.initialise_kb import initialise_kb
#from epcdata.datainstantiation.readings import update_all_stations


# Initialise logger
logger = agentlogging.get_logger("prod")

inputtasks_bp = Blueprint(
    'inputtasks_bp', __name__
)

# Define route for API request to initialise knowledge base with ontology
@inputtasks_bp.route('/api/epcagent/initialise', methods=['GET'])
def api_initialise_kb():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        print("Query parameters provided, although not required. " \
              + "Provided arguments will be neglected.")
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Initialise KB
        initialise_kb()
        return jsonify({'status': '200', 'msg': 'Initialisation successful'})

    except Exception as ex:
        logger.error("Unable to initialise knowledge base with TBox and ABox.", ex)
        return jsonify({'status': '500', 'msg': f'Initialisation failed'})


# # Define route for API request to update all stations and readings (i.e. instantiate
# #  missing stations), and add latest time series readings for all time series
# @inputtasks_bp.route('/api/metofficeagent/update/all', methods=['GET'])
# def api_update_all_stations():
#     # Check arguments (query parameters)
#     if len(request.args) > 0:
#         print("Query parameters provided, although not required. " \
#               + "Provided arguments will be neglected.")
#         #logger.warning("Query parameters provided, although not required. \
#         #                Provided arguments will be neglected.")
#     try:
#         # Instantiate stations
#         response = update_all_stations()
#         print(f"Number of instantiated stations: {response[0]}")
#         print(f"Number of instantiated readings: {response[1]}")
#         print(f"Number of updated time series readings (i.e. dataIRIs): {response[2]}")
#         return jsonify({"stations": response[0], "readings": response[1], 
#                         "reading_timeseries": response[2]})

#     except Exception as ex:
#         print(ex)
#         return jsonify({"status": '500', 'errormsg': 'Update failed'})
