################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 14 Sep 2022                            #
################################################

from flask import Blueprint, request, jsonify

import agentlogging

from epcdata.errorhandling.exceptions import InvalidInput
from epcdata.kgutils.initialise_kb import initialise_kb
from epcdata.datainstantiation.postcodes import initialise_postcodes


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


# Define route for API request to initialise postcodes for provided local
# authority district
@inputtasks_bp.route('/api/epcagent/instantiate/postcodes', methods=['POST'])
def api_initialise_postcodes():
    #
    # Check arguments (query parameters)
    #
    inputs = { 'local_authority_district': None }
    # Get received 'query' JSON object which holds all parameters
    try:
        query = request.json['query']
    except:
        logger.error('No JSON "query" object could be identified.')
        raise InvalidInput('No JSON "query" object could be identified.')
    # Retrieve local authority district code
    try:
        inputs['local_authority_district'] = str(query['district'])
    except:
        logger.error('Invalid local authority district code provided.')
        raise InvalidInput('Invalid local authority district code provided.')
    try:
        # Instantiate stations
        response = initialise_postcodes(**inputs)
        if not response:
            return jsonify({"status": '200', 'msg': 'Local authority code already instantiated'})
        else:
            return jsonify({"Postcodes": response})
    except Exception as ex:
        logger.error("Unable to instantiate local authority with postcodes.", ex)
        return jsonify({"status": '500', 'msg': 'Postcode instantiation failed'})
