################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 12 Oct 2022                            #
################################################

from flask import Blueprint, request, jsonify

from py4jps import agentlogging

from agent.errorhandling.exceptions import InvalidInput
from agent.datainstantiation.sales_instantiation import add_ocgml_building_data, update_transaction_records, \
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
    # Retrieve minimum required confidence score
    try:
        min_match = int(query['min_confidence'])
    except Exception as ex:
        logger.info('No confidence score provided, using default.')
        min_match = 90
    if min_match not in range(0, 100):
        logger.error('Invalid "min_confidence" provided. Supported range: 0-100.')
        raise InvalidInput('Invalid "min_confidence" provided. Supported range: 0-100.')
    try:
        # Update sales transaction record for (list of) property IRIs
        res = update_transaction_records(property_iris=iris, min_conf_score=min_match)
        return jsonify({'status': '200', 
                        'Instantiated property transactions': res[0],
                        'Updated property transactions': res[1]})
    except Exception as ex:
        logger.error("Unable to update property sales transactions.", ex)
        return jsonify({'status': '500', 'msg': 'Updating property sales transactions failed.'})


# Define route for API request to update transaction records for all instantiated properties 
# (also updates associated property price indices)
@inputtasks_bp.route('/api/landregistry/update_all', methods=['POST'])
def api_update_all_transaction_records():
    # Check arguments (query parameters)
    if len(request.get_data()) == 0:
        print("No query parameters provided, using default confidence score for matching.")
        logger.warning("No query parameters provided, using default confidence score for matching.")
        min_match = 90
    else:
        # Get received 'query' JSON object which holds all HTTP parameters
        try:
            query = request.json['query']
        except Exception as ex:
            logger.error('No JSON "query" object could be identified.')
            raise InvalidInput('No JSON "query" object could be identified.') from ex
        # Retrieve minimum required confidence score
        try:
            min_match = int(query['min_confidence'])
        except Exception as ex:
            logger.info('No confidence score provided, using default.')
            min_match = 90
        if min_match not in range(0, 100):
            logger.error('Invalid "min_confidence" provided. Supported range: 0-100.')
            raise InvalidInput('Invalid "min_confidence" provided. Supported range: 0-100.')
    try:
        # Update sales transaction record for all instantiated property IRIs
        res = update_all_transaction_records(min_conf_score=min_match)
        return jsonify({'status': '200', 
                        'Instantiated property transactions': res[0],
                        'Updated property transactions': res[1],
                        'Instantiated property price indices': res[2],
                        'Updated property price indices': res[3]})
    except Exception as ex:
        logger.error("Unable to update property sales transactions.", ex)
        return jsonify({'status': '500', 'msg': 'Updating property sales transactions failed.'})


# Define route for API request to retrieve relevant building information from OCGML and 
# instantiate according to OntoBuiltEnv
@inputtasks_bp.route('/api/landregistry/add_ocgml_info', methods=['GET'])
def api_add_ocgml_building_data():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        print("Query parameters provided, although not required. " \
              + "Provided arguments will be neglected.")
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Retrieve and instantiate building elevation
        res = add_ocgml_building_data()
        return jsonify({'status': '200', 
                        'Instantiated PostGIS footprints': res[0],
                        'Already instantiated PostGIS footprints': res[1],
                        'Deleted building elevations': res[2],
                        'Instantiated building elevations': res[3]})

    except Exception as ex:
        logger.error("Unable to instantiate PostGIS features and/or OntoBuiltEnv building elevations.", ex)
        return jsonify({'status': '500', 'msg': f'Instantiating OntoCityGml data failed.'})