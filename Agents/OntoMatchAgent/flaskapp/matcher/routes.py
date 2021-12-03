import traceback

from flask import Blueprint, request, jsonify, make_response
import ontomatch.matchManager

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
        checkAddr(config_handle)
        checkAddr(src_graph_handle)
        checkAddr(tgt_graph_handle)
        agent = checkChoice(choice)

        # Run the agent
        enriched, handle = agent.start()
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

def checkChoice(n):
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