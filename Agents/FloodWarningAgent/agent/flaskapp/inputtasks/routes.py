################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 12 Feb 2023                            #
################################################

import traceback

from flask import Blueprint, request, jsonify

from agent.datainstantiation.warnings import update_warnings

from py4jps import agentlogging

# Initialise logger
#TODO: update logger level
logger = agentlogging.get_logger("dev")


inputtasks_bp = Blueprint(
    'inputtasks_bp', __name__
)


# Define route for API request to update all flood alerts/warnings 
# (i.e. instantiate missing ones, update existing ones, and archive outdated ones)
@inputtasks_bp.route('/update/all', methods=['POST'])
def api_update_all_warnings():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Instantiate stations
        response = update_warnings()
        logger.info(f"Number of instantiated warnings: {response[0]}")
        logger.info(f"Number of updated warnings: {response[1]}")
        logger.info(f"Number of archived warnings: {response[2]}")
        return jsonify({'Instantiated warnings': response[0], 
                        'Upddated warnings': response[1], 
                        'Archived warnings': response[2]}), 200

    except Exception as ex:
        logger.error("Unable to forecast.", ex)
        #logger.error(traceback.format_exc())
        return jsonify({'status': '500', 'msg': 'Update failed: ' + str(ex)}), 500


#TODO: to be implemented
#Potential challenge: secure retrieval of county specific flood warnings from KG to
#                     avoid unwanted deletion of warnings from other counties
# Define route for API request to update flood alerts/warnings for specific county
# (i.e. instantiate missing ones, update existing ones, and archive outdated ones)
# @inputtasks_bp.route('/update', methods=['POST'])
# def api_update_warnings():
#     # Check arguments (query parameters)
    
#     try:
#         # Instantiate stations
#         response = update_warnings()
#         print(f"Number of instantiated warnings: {response[0]}")
#         print(f"Number of updated warnings: {response[1]}")
#         print(f"Number of archived warnings: {response[2]}")
#         return jsonify({"Instantiated warnings": response[0], 
#                         "Upddated warnings": response[1], 
#                         "Archived warnings": response[2]})

#     except Exception as ex:
#         print(ex)
#         return jsonify({"status": '500', 'errormsg': 'Update failed.'})
