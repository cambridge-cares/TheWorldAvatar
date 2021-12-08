import traceback

from flask import Blueprint, request, jsonify, make_response
import os
import ontomatch.utils.blackboard

blackboard_bp = Blueprint(
    'blackboard_bp', __name__
)

# Define a route for API requests
@blackboard_bp.route('/api/blackboard', methods=['GET'])
def read():
    try:
        # Check parameters
        if 'handle' not in request.args:
            raise Exception("invalid request. No handle specified.")
        handle = request.args['handle']
        checkAddr(handle)
        # Run the agent
        object = ontomatch.utils.blackboard.Agent().read(handle)
        response = {}
        response["object"] = object
        #TODO: needs format conversion on the recieving side
        return jsonify({"result": response})

    except Exception as ex:
        print(ex)
        return jsonify({'errormsg': 'Invalid request'}), 500

@blackboard_bp.route('/api/blackboard', methods=['POST'])
def api():
    try:
        # Check parameters
        if 'addr' not in request.args or 'serialized_object' not in request.args:
            raise Exception("invalid request")
        serialized_object = request.args['serialized_object']
        addr = request.args["addr"]
        checkAddr(addr)
        #TODO: serialized_object needs any validation?

        # Run the agent
        handle = ontomatch.utils.blackboard.Agent().write(addr, serialized_object)
        response = {}
        response["handle"] = handle
        return jsonify({"result": response})

    except Exception as ex:
        print(ex)
        return jsonify({'errormsg': 'Invalid request'}), 500

def checkAddr(filePath):
    #TODO: handle relative path
    #if not os.path.exists(filePath):
    #    raise Exception("invalid parameter addr")
    pass
