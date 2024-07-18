################################################
# Authors: Feroz Farazi (msff2@cam.ac.uk)      #    
# Date: 27 June 2023                           #
################################################

from flask import Blueprint, request, jsonify

from py4jps import agentlogging
from agent.datainstantiation.generator_data_instantiator import instantiate_all_generators

# Initialise logger
logger = agentlogging.get_logger("prod")


inputtasks_bp = Blueprint(
    'inputtasks_bp', __name__
)

# Defines route for API request to instantiate all relevant power generators
@inputtasks_bp.route('/api/ukpowergenerationagent/instantiate/generators', methods=['GET'])
def api_instantiate_all_generators():
    # Check arguments (query parameters)
    if len(request.args) > 0:
        print("Query parameters provided, although not required. " \
             + "Provided arguments will be neglected.")
        logger.warning("Query parameters provided, although not required. \
                        Provided arguments will be neglected.")
    try:
        # Instantiate all relevant generators
        response = instantiate_all_generators()
        print(f"Number of instantiated generators: {response}")
        return jsonify({"generators": response})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': 'Instantiation failed'})
