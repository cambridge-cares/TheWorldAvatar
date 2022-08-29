import traceback

from flask import Blueprint, request, jsonify, make_response
import os
import ontomatch.coordinator
import ontomatch.kgoperations
coordinator_bp = Blueprint(
    'coordinator_bp', __name__
)
#extra params: remote
# Define a route for API requests
@coordinator_bp.route('/api/coordinator', methods=['POST'])
def api():
    try:
        # Check parameters
        if 'config' not in request.args:
            raise Exception("invalid request")
        config = request.args["config"]
        print(config)

        # Run the agent
    except Exception as ex:
        print(ex)
        return jsonify({'errormsg': 'Invalid request\n'+traceback.format_exc()}), 400

    try:
        if 'remote' in request.args:
            ontomatch.kgoperations.Agent().download(config)
        #kgcaller download file
        ontomatch.coordinator.Agent().start(config, True)
        #kg caller upload file
        if 'remote' in request.args:
            ontomatch.kgoperations.Agent().upload(config)
        return jsonify({"result": {"done":True}})

    except Exception as ex:
        print(ex)
        return jsonify({'errormsg': 'Agent fails\n'+traceback.format_exc()}), 500
