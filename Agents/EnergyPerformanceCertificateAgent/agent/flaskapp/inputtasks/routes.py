################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 14 Sep 2022                            #
################################################

from flask import Blueprint, request, jsonify

import agentlogging

from agent.errorhandling.exceptions import InvalidInput
from agent.utils.env_configs import OCGML_ENDPOINT
from agent.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT
from agent.kgutils.initialise_kb import create_blazegraph_namespace, upload_ontology
from agent.kgutils.initialise_ocgml import upload_ocgml_quads
from agent.datainstantiation.postcodes import initialise_postcodes
from agent.datainstantiation.epc_instantiation import instantiate_epc_data_for_certificate, \
                                                        instantiate_epc_data_for_all_postcodes, \
                                                        add_ocgml_building_data


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
        # Create OntoBuiltEnv namespace
        create_blazegraph_namespace(endpoint=UPDATE_ENDPOINT, 
                                    quads=False, geospatial=False)
        # Initialise KB
        upload_ontology()
        return jsonify({'status': '200', 'msg': 'Initialisation successful'})

    except Exception as ex:
        logger.error("Unable to initialise knowledge base with TBox and ABox.", ex)
        return jsonify({'status': '500', 'msg': f'Initialisation failed'})


# Define route for API request to initialise OntoCityGml knowledge base with 
# previously instantiated and exported quads
@inputtasks_bp.route('/api/ocgml/initialise', methods=['GET'])
def api_initialise_ocgml():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        print("Query parameters provided, although not required. " \
              + "Provided arguments will be neglected.")
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Create OntoCityGml namespace
        create_blazegraph_namespace()
        # Upload OntoCityGml quads
        upload_ocgml_quads()
        return jsonify({'status': '200', 'msg': 'OntoCityGml quad upload successful'})

    except Exception as ex:
        logger.error("Unable to upload OntoCityGml quads.", ex)
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
    except Exception as ex:
        logger.error('No JSON "query" object could be identified.')
        raise InvalidInput('No JSON "query" object could be identified.') from ex
    # Retrieve local authority district code
    try:
        inputs['local_authority_district'] = str(query['district'])
    except Exception as ex:
        logger.error('Invalid local authority district code provided.')
        raise InvalidInput('Invalid local authority district code provided.') from ex
    try:
        # Instantiate postcodes
        response = initialise_postcodes(**inputs)
        if not response:
            return jsonify({"status": '200', 'msg': 'Local authority code already instantiated'})
        else:
            return jsonify({"Postcodes": response})
    except Exception as ex:
        logger.error("Unable to instantiate local authority with postcodes.", ex)
        return jsonify({"status": '500', 'msg': 'Postcode instantiation failed'})


# Define route for API request to instantiate single EPC data (i.e. for one UPRN)
@inputtasks_bp.route('/api/epcagent/instantiate/certificates/single', methods=['POST'])
def api_instantiate_epc_data_for_certificate():
    #
    # Check arguments (query parameters)
    #
    inputs = { 'lmk_key': None, 
               'epc_endpoint': None,
               'ocgml_endpoint': None 
             }
    # Get received 'query' JSON object which holds all parameters
    try:
        query = request.json['query']
    except Exception as ex:
        logger.error('No JSON "query" object could be identified.')
        raise InvalidInput('No JSON "query" object could be identified.') from ex
    # Retrieve certificate's unique lodgement identifier
    try:
        inputs['lmk_key'] = str(query['lmk_key'])
    except Exception as ex:
        logger.error('Invalid "lmk_key" provided.')
        raise InvalidInput('Invalid "lmk_key" provided.') from ex
    # Retrieve EPC API endpoint
    try:
        endpoint = str(query['epc_endpoint'])
        if endpoint in ['domestic', 'non-domestic', 'display']:
            inputs['epc_endpoint'] = endpoint
        else:
            raise InvalidInput('Invalid EPC API endpoint provided.')
    except Exception as ex:
        logger.error('Invalid EPC API endpoint provided.')
        raise InvalidInput('Invalid EPC API endpoint provided.') from ex
    # Retrieve endpoint to instantiated OntoCityGml instances
    ocgml_endpoint = None
    try:
        ocgml_endpoint = str(query['ocgml_endpoint'])
    except Exception as ex:
        logger.error('Invalid OntoCityGml endpoint provided.')
        raise InvalidInput('Invalid OntoCityGml endpoint provided.') from ex
    if not ocgml_endpoint:
        ocgml_endpoint = OCGML_ENDPOINT
        logger.info('Using default OntoCityGml endpoint.')
    inputs['ocgml_endpoint'] = ocgml_endpoint
    try:
        # Instantiate EPC
        response = instantiate_epc_data_for_certificate(**inputs)
        return jsonify({'status': '200', 
                        'Newly instantiated EPCs': response[0],
                        'Updated EPCs': response[1]})
    except Exception as ex:
        logger.error("Unable to instantiate EPC data.", ex)
        return jsonify({"status": '500', 'msg': 'EPC data instantiation failed'})


# Define route for API request to instantiate all latest EPC data for all 
# instantiated postcodes (i.e. latest available data for all UPRNs)
@inputtasks_bp.route('/api/epcagent/instantiate/certificates/all', methods=['POST'])
def api_instantiate_epc_data_for_all_uprns():
    #
    # Check arguments (query parameters)
    #
    inputs = { 'epc_endpoint': None,
               'ocgml_endpoint': None 
             }
    # Get received 'query' JSON object which holds all parameters
    try:
        query = request.json['query']
    except Exception as ex:
        logger.error('No JSON "query" object could be identified.')
        raise InvalidInput('No JSON "query" object could be identified.') from ex
    # Retrieve EPC API endpoint
    try:
        endpoint = str(query['epc_endpoint'])
        if endpoint in ['domestic', 'non-domestic', 'display']:
            inputs['epc_endpoint'] = endpoint
        else:
            raise InvalidInput('Invalid EPC API endpoint provided.')
    except Exception as ex:
        logger.error('Invalid EPC API endpoint provided.')
        raise InvalidInput('Invalid EPC API endpoint provided.') from ex
    # Retrieve endpoint to instantiated OntoCityGml instances
    ocgml_endpoint = None
    try:
        ocgml_endpoint = str(query['ocgml_endpoint'])
    except Exception as ex:
        logger.error('Invalid OntoCityGml endpoint provided.')
        raise InvalidInput('Invalid OntoCityGml endpoint provided.') from ex
    if not ocgml_endpoint:
        ocgml_endpoint = OCGML_ENDPOINT
        logger.info('Using default OntoCityGml endpoint.')
    inputs['ocgml_endpoint'] = ocgml_endpoint
    try:
        # Instantiate EPC
        response = instantiate_epc_data_for_all_postcodes(**inputs)
        return jsonify({'status': '200', 
                        'Newly instantiated EPCs': response[0][0],
                        'Updated EPCs': response[0][1],
                        'Newly instantiated parent buildings': response[1][0],
                        'Updated parent buildings': response[1][1]})
    except Exception as ex:
        logger.error("Unable to instantiate EPC data.", ex)
        return jsonify({"status": '500', 'msg': 'EPC data instantiation failed'})

#
# HTTP requests to be run after Building Matching Agent has linked
# OntoBuiltEnv building instances with OntoCityGml ones
#

# Define route for API request to retrieve relevant building information from OCGML and 
# instantiate according to OntoBuiltEnv
@inputtasks_bp.route('/api/epcagent/add/ocgml_info', methods=['GET'])
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