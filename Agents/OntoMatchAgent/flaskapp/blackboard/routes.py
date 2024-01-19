import traceback

from flask import Blueprint, request, jsonify, make_response
import os
import ontomatch.utils.blackboard
import flaskapp.parameterVerifier.verifier as verifier

blackboard_bp = Blueprint(
    'blackboard_bp', __name__
)
bp_root = os.path.abspath(ontomatch.utils.blackboard.LOCAL_BLACKBOARD_DIR)

# Define a route for API requests
@blackboard_bp.route('/api/blackboard', methods=['GET'])
def read():
    try:
        # Check parameters
        if 'handle' not in request.args:
            raise Exception("invalid request. No handle specified.")
        handle = request.args['handle']
        print(bp_root)
        verifier.verifyRelativePathExists(handle, bp_root)

    except Exception as ex:
        print(ex)
        return jsonify({'errormsg': 'Invalid request\n'+traceback.format_exc()}), 400

    # Run the agent
    try:
        object = ontomatch.utils.blackboard.Agent().read(handle)
        response = object
        return jsonify({"result": response})

    except Exception as ex:
        print(ex)
        return jsonify({'errormsg': 'Agent fails\n'+traceback.format_exc()}), 500

@blackboard_bp.route('/api/blackboard', methods=['POST'])
def write():
    try:
        # Check parameters
        if 'addr' not in request.args or 'serialized_object' not in request.args:
            raise Exception("invalid request")
        serialized_object = request.args['serialized_object']
        addr = request.args["addr"]
        #TODO: addr as file creatable

    except Exception as ex:
        print(ex)
        return jsonify({'errormsg': 'Invalid request\n'+traceback.format_exc()}), 400
        # Run the agent
    try:
        handle = ontomatch.utils.blackboard.Agent().write(addr, serialized_object)
        response = {}
        response["handle"] = handle
        return jsonify({"result": response})

    except Exception as ex:
        print(ex)
        return jsonify({'errormsg': 'Agent fails\n'+traceback.format_exc()}), 500