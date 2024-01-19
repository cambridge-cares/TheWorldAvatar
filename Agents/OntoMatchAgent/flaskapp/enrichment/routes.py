import traceback

from flask import Blueprint, request, jsonify, make_response
import os
import ontomatch.knowledge.enrichment
import flaskapp.parameterVerifier.verifier as verifier
enrichment_bp = Blueprint(
    'enrichment_bp', __name__
)

# Define a route for API requests
@enrichment_bp.route('/api/enrichment', methods=['POST'])
def api():

    try:
        # Check parameters
        if 'addr' not in request.args or 'add_knowledge' not in request.args:
            raise Exception("invalid request")
        addr = request.args['addr']
        add_knowledge = request.args['add_knowledge']

        verifier.verifyRelativePathExists(addr,os.getcwd())
        verifier.verifyChoice(add_knowledge, ["False","ontomatch.knowledge.geocoding","ontomatch.knowledge.geoNames"])
    except Exception as ex:
        print(ex)
        return jsonify({'errormsg': 'Invalid request'}), 400

    try:
        # Run the agent
        enriched, handle = ontomatch.knowledge.enrichment.Agent().start(addr, add_knowledge, http=True)
        response = {}
        response["enriched"] = enriched
        response["handle"] = handle
        return jsonify({"result": response})
    except Exception as ex:
        print(ex)
        return jsonify({'errormsg': 'Agent could not run.'}), 500


