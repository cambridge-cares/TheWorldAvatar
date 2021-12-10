import traceback

from flask import Blueprint, request, jsonify, make_response
import ontomatch.matchManager
import flaskapp.parameterVerifier.verifier as verifier

matchmanager_bp = Blueprint(
    'matchmanager_bp', __name__
)

# Define a route for API requests
@matchmanager_bp.route('/api/matchmanager', methods=['POST'])
def api():
    # TODO: Check arguments validity(query parameters)
    try:
        # Check parameters
        if 'config_handle' not in request.args or 'src_graph_handle' not in request.args or 'tgt_graph_handle' not in request.args:
            raise Exception("invalid request, missing query parameter")
        config_handle = request.args['config_handle']
        src_graph_handle = request.args['src_graph_handle']
        tgt_graph_handle = request.args['tgt_graph_handle']
        choice = request.args["choice"]
        verifier.verifyRelativePathExists(src_graph_handle)
        verifier.verifyRelativePathExists(tgt_graph_handle)
        verifier.verifyChoice(choice, ["scoring_weight","classifier","autocalibration"])
        agent = agentChoice(choice)
    except Exception as ex:
        print(ex)
        return jsonify({'errormsg': 'Invalid request\n'+traceback.format_exc()}), 400
    try:
        # Run the agent
        agent.start(config_handle, src_graph_handle, tgt_graph_handle)

        return jsonify({"result": {"done":True}})

    except Exception as ex:
        print(ex)
        return jsonify({'errormsg': 'Agent fails'}), 500

def agentChoice(n):
    n = n.lower()
    print(n)
    if n =="default":
        matcher = ontomatch.matchManager.matchManager()
    elif n == "scoring_weight":
        matcher = ontomatch.instancematching.InstanceMatcherWithScoringWeights()
    elif n == "classifier":
        matcher = ontomatch.instancematching.InstanceMatcherClassifier()
    elif n == "autocalibration":
        matcher = ontomatch.instancematching.InstanceMatcherWithAutoCalibration()
    else:
        raise Exception("invalid parameter matching choice.")
    return matcher