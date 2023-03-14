################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 12 Oct 2022                            #
################################################

from flask import Blueprint, request, jsonify

from agent.errorhandling.exceptions import InvalidInput
from agent.datainstantiation.sales_instantiation import update_transaction_records, \
                                                        update_all_transaction_records
from agent.datainstantiation.data_manipulation import scale_property_price_index

# Initialise logger
from py4jps import agentlogging
logger = agentlogging.get_logger("prod")


inputtasks_bp = Blueprint(
    'inputtasks_bp', __name__
)


# Define route for API request to update transaction record for single property
# or list of provided properties
@inputtasks_bp.route('/landregistry/update', methods=['POST'])
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
        return_dict ={'Instantiated property transactions': res[0],
                      'Updated property transactions': res[1]}
        # Print results
        for key, value in return_dict.items():
            print(key, ' : ', value)
        # Return HTTP response
        return jsonify(return_dict), 200   
    
    except Exception as ex:
        logger.error('Unable to update property sales transactions: {}'.format(ex))
        return jsonify({'msg': 'Updating property sales transactions failed: ' + str(ex)}), 500


# Define route for API request to update transaction records for all instantiated properties 
# (also updates associated property price indices)
@inputtasks_bp.route('/landregistry/update_all', methods=['POST'])
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
        return_dict ={'Instantiated property transactions': res[0],
                        'Updated property transactions': res[1],
                        'Instantiated property price indices': res[2],
                        'Updated property price indices': res[3]}
        # Print results
        for key, value in return_dict.items():
            print(key, ' : ', value)
        # Return HTTP response
        return jsonify(return_dict), 200
    
    except Exception as ex:
        logger.error('Unable to update property sales transactions: {}'.format(ex))
        return jsonify({'msg': 'Updating property sales transactions failed: ' + str(ex)}), 500
    

# Define route for API request to scale instantiated property price index data
@inputtasks_bp.route('/landregistry/scale_ppi', methods=['POST'])
def api_scale_property_price_index():
    # Get received 'query' JSON object which holds all HTTP parameters
    try:
        query = request.json['query']
    except Exception as ex:
        logger.error('No JSON "query" object could be identified.')
        raise InvalidInput('No JSON "query" object could be identified.') from ex
    
    # Retrieve property price index IRI which to scale
    try:
        ppi_iri = str(query['ppi_iri'])
    except Exception as ex:
        logger.error('Invalid "Property Price Index IRI" provided.')
        raise InvalidInput('Invalid "Property Price Index IRI" provided.') from ex
    # Retrieve scaling factor
    try:
        scaler = float(query['scaler'])
    except Exception as ex:
        logger.error('Invalid "Scaling Factor" provided.')
        raise InvalidInput('Invalid "Scaling Factor" provided.') from ex
    try:
        # Scale property price index data
        scale_property_price_index(ppi_iri=ppi_iri, scaler=scaler)
        return jsonify({'msg': 'Property price index data scaled successfully'}), 200
    except Exception as ex:
        logger.error('Unable to scale property price index data: {}'.format(ex))
        return jsonify({'msg': 'Unable to scale property price index data: ' + str(ex)}), 500
