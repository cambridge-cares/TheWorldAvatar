################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 12 Oct 2022                            #
################################################

from flask import Blueprint, request, jsonify

import agentlogging

from landregistry.errorhandling.exceptions import InvalidInput
from landregistry.datainstantiation.sales_instantiation import update_transaction_records, \
                                                               update_all_transaction_records


# Initialise logger
logger = agentlogging.get_logger("prod")


inputtasks_bp = Blueprint(
    'inputtasks_bp', __name__
)


# Define route for API request to update transaction record for single property
# or list of provided properties
@inputtasks_bp.route('/api/landregistry/update', methods=['POST'])
def api_update_transaction_records():
    # Get received 'query' JSON object which holds all HTTP parameters
    try:
        query = request.json['query']
    except Exception as ex:
        logger.error('No JSON "query" object could be identified.')
        raise InvalidInput('No JSON "query" object could be identified.') from ex
    # Retrieve property IRIs to be updated
    try:
        iris = str(query['property_iris'])
        # Create list of property IRIs
        iris = iris.replace(' ', '').replace('<', '').replace('>', '').split(',')
    except Exception as ex:
        logger.error('Invalid (list of) "property_iris" provided.')
        raise InvalidInput('Invalid (list of) "property_iris" provided.') from ex
    try:
        # Update sales transaction record for (list of) property IRIs
        res = update_transaction_records(property_iris=iris)
        return jsonify({'status': '200', 
                        'Updated property transactions': res})
    except Exception as ex:
        logger.error("Unable to update property sales transactions.", ex)
        return jsonify({'status': '500', 'msg': 'Updating property sales transactions failed.'})


# Define route for API request to update transaction records for all instantiated properties 
# (also updates associated property price indices)
@inputtasks_bp.route('/api/landregistry/update_all', methods=['POST'])
def api_update_all_transaction_records():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        print("Query parameters provided, although not required. " \
              + "Provided arguments will be neglected.")
        logger.warning("Query parameters provided, although not required. " \
                       + "Provided arguments will be neglected.")
    try:
        # Update sales transaction record for all instantiated property IRIs
        res = update_all_transaction_records()
        return jsonify({'status': '200', 
                        'Updated property transactions': res[0],
                        'Updated property price indices': res[1],})
    except Exception as ex:
        logger.error("Unable to update property sales transactions.", ex)
        return jsonify({'status': '500', 'msg': 'Updating property sales transactions failed.'})