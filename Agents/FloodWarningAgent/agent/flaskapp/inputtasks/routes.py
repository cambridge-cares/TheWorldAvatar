################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 12 Feb 2023                            #
################################################

import os

from flask import Blueprint, request, jsonify

from agent.datainstantiation.warnings import update_warnings
from agent.errorhandling.exceptions import InvalidInput

from py4jps import agentlogging

# Initialise logger
logger = agentlogging.get_logger("prod")


inputtasks_bp = Blueprint(
    'inputtasks_bp', __name__
)


# Define route for API request to update all flood alerts/warnings 
# (i.e. instantiate missing ones, update existing ones, and archive outdated ones)
@inputtasks_bp.route('/floodwarnings/update/all', methods=['POST'])
def api_update_all_warnings():
    #
    # Check arguments (query parameters)
    #
    inputs = { 'mock_api': None }
    # Get received 'query' JSON object which holds all parameters
    try:
        query = request.json['query']
    except Exception as ex:
        logger.info('No "query" parameters provided, querying all flood warnings from EA API.')
        query = {}
    
    if query:
        # Retrieve mock API .json path
        try:
            inputs['mock_api'] = str(query['file_path'])
            if not os.path.isfile(inputs['mock_api']):
                raise InvalidInput('Specified file path does not exist.')
        except Exception as ex:
            logger.error('Unable to retrieve mock API .json path: ' + str(ex))
            raise InvalidInput('Unable to retrieve mock API .json path.') from ex
    try:
        # Instantiate flood warnings
        response = update_warnings(**inputs)
        return_dict = {'Instantiated areas': response[0],
                       'Instantiated warnings': response[1],
                       'Updated warnings': response[2],
                       'Deleted warnings': response[3] }
        return jsonify(return_dict), 200

    except Exception as ex:
        logger.error('Update failed: ' + str(ex), ex)
        return jsonify({'msg': 'Update failed: ' + str(ex)}), 500


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
