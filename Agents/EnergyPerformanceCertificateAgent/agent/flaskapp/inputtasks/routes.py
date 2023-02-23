################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 22 Feb 2023                            #
################################################

from celery.result import AsyncResult
from flask import Blueprint, request, jsonify

from py4jps import agentlogging

from . import tasks
from agent.errorhandling.exceptions import InvalidInput
from agent.utils.env_configs import OCGML_ENDPOINT
from agent.utils.stack_configs import UPDATE_ENDPOINT
from agent.kgutils.initialise_kb import create_blazegraph_namespace, upload_ontology
from agent.kgutils.initialise_ocgml import upload_ocgml_quads

# Initialise logger
logger = agentlogging.get_logger("prod")


inputtasks_bp = Blueprint(
    'inputtasks_bp', __name__
)


# Fetch the result of previously started task with provided 'id'
# Return whether the task is finished (ready), whether it finished successfully, 
# and what the return value (or error) was if it is finished.
@inputtasks_bp.get("/epcagent/result/<id>")
def task_result(id: str):
    result = AsyncResult(id)
    return {"ready": result.ready(),
            "successful": result.successful(),
            "response": result.result if result.ready() else None}


# Define agent route to initialise postcodes for provided local authority district
@inputtasks_bp.route('/epcagent/instantiate/postcodes', methods=['POST'])
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
    
    # Start Celery task
    result = tasks.task_initialise_postcodes.delay(**inputs)
    print(f'Submitted job to instantiate postcodes - job id: {result.id}')
    return jsonify({'result_id': result.id}), 200


# Define agent route to instantiate single EPC data (i.e. for one UPRN)
@inputtasks_bp.route('/epcagent/instantiate/certificates/single', methods=['POST'])
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
        ocgml_endpoint = str(query.get('ocgml_endpoint'))
    except Exception as ex:
        # In case missing or not string castable endpoint is provided
        logger.error('Invalid OntoCityGml endpoint provided.')
    if not ocgml_endpoint:
        ocgml_endpoint = OCGML_ENDPOINT
        logger.info('Using default OntoCityGml endpoint.')
    inputs['ocgml_endpoint'] = ocgml_endpoint

    # Start Celery task
    result = tasks.task_instantiate_epc_data_for_certificate.delay(**inputs)
    print(f'Submitted job to update single EPC - job id: {result.id}')
    return jsonify({'result_id': result.id}), 200


# Define agent route to instantiate all latest EPC data for all instantiated postcodes
# (i.e. instantiate/update latest available data for all UPRNs)
@inputtasks_bp.route('/epcagent/instantiate/certificates/all', methods=['POST'])
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
        endpoint = query.get('epc_endpoint')
        if not endpoint:
            # If no endpoint is specified, defaults to query all endpoints
            pass
        elif endpoint in ['domestic', 'non-domestic', 'display']:
            inputs['epc_endpoint'] = endpoint
        else:
            raise InvalidInput('Invalid EPC API endpoint provided.')
    except Exception as ex:
        logger.error('Invalid EPC API endpoint provided.')
        raise InvalidInput('Invalid EPC API endpoint provided.') from ex
    # Retrieve endpoint to instantiated OntoCityGml instances
    ocgml_endpoint = None
    try:
        ocgml_endpoint = str(query.get('ocgml_endpoint'))
    except Exception as ex:
        # In case missing or not string castable endpoint is provided
        logger.error('Invalid OntoCityGml endpoint provided.')
    if not ocgml_endpoint:
        ocgml_endpoint = OCGML_ENDPOINT
        logger.info('Using default OntoCityGml endpoint.')
    inputs['ocgml_endpoint'] = ocgml_endpoint
    
    # Start Celery task
    result = tasks.task_instantiate_epc_data_for_all_uprns.delay(**inputs)
    print(f'Submitted job to update all EPCs - job id: {result.id}')
    return jsonify({'result_id': result.id}), 200

#
# HTTP requests to be run after Building Matching Agent has linked
# OntoBuiltEnv building instances with OntoCityGml ones
#

# Define agent route to retrieve relevant building information from OCGML and 
# instantiate it using OntoBuiltEnv (required for PostGIS/Geoserver visualisation)
@inputtasks_bp.route('/epcagent/add/ocgml_info', methods=['GET'])
def api_add_ocgml_building_data():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")

    # Start Celery task
    result = tasks.task_add_ocgml_building_data.delay()
    print(f'Submitted job to retrieve OCGML data - job id: {result.id}')
    return jsonify({'result_id': result.id}), 200

#
# DEPRECATED ENDPOINTS
#

# Define route for API request to initialise knowledge base with ontology
#NOTE: kept for reference, but knowledge graph now initialised automatically on agent startup
@inputtasks_bp.route('/epcagent/initialise', methods=['GET'])
def api_initialise_kb():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Create OntoBuiltEnv namespace
        create_blazegraph_namespace(endpoint=UPDATE_ENDPOINT, 
                                    quads=False, geospatial=False)
        # Initialise KG with ontology
        upload_ontology()
        return jsonify({'msg': 'Initialisation successful.'}), 200

    except Exception as ex:
        logger.error("Unable to initialise knowledge base with TBox and ABox: " + str(ex))
        return jsonify({'msg': 'Initialisation failed: ' + str(ex)}), 500


# Define route for API request to initialise OntoCityGml knowledge base with 
# previously instantiated and exported quads
#NOTE: kept for reference, but now preferably done using Stack-Data-Uploader
@inputtasks_bp.route('/ocgml/initialise', methods=['GET'])
def api_initialise_ocgml():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Create OntoCityGml namespace
        create_blazegraph_namespace()
        # Upload OntoCityGml quads
        upload_ocgml_quads()
        return jsonify({'msg': 'OntoCityGml quad upload successful'}), 200

    except Exception as ex:
        logger.error("Unable to upload OntoCityGml quads: " + str(ex))
        return jsonify({'msg': 'Initialisation failed: ' + str(ex)}), 500