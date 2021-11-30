import traceback

from flask import Blueprint, request, jsonify, make_response
import os
import ontomatch.knowledge.enrichment

enrichment_bp = Blueprint(
    'enrichment_bp', __name__
)

# Define a route for API requests
@enrichment_bp.route('/api/enrichment', methods=['POST'])
def api():
    # TODO: Check arguments validity(query parameters)

    try:
        # Check parameters
        if 'addr' not in request.args or 'add_knowledge' not in request.args:
            raise Exception("invalid request")
        addr = request.args['addr']
        add_knowledge = request.args['add_knowledge']
        checkAddr(addr)
        add_knowledge = check_add_knowledge(add_knowledge)

        # Run the agent
        enriched, handle = ontomatch.knowledge.enrichment.Agent().start(addr, add_knowledge, http=False)
        response = {}
        response["enriched"] = enriched
        response["handle"] = handle
        return jsonify({"result": response})

    except Exception as ex:
        print(ex)
        return jsonify({"status": '500', 'errormsg': str(traceback.format_exc())})

def checkAddr(filePath):
    #TODO: handle relative path
    #if not os.path.exists(filePath):
    #    raise Exception("invalid parameter addr")
    pass

def check_add_knowledge(n):
    n = n.lower()
    print(n)
    if n =="false":
        return False
    elif n == "true" or n == "ontomatch.knowledge.geocoding":
        return "ontomatch.knowledge.geocoding"
    else:
        raise Exception("invalid parameter add_knowledge.")
