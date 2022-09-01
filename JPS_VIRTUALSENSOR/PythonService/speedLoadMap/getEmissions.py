from speedLoadMap.ADMSMapSpeedTorqueNOxSoot import calcEmissions
from flask import Blueprint, request, jsonify
import subprocess
import json

ROUTE = "/getEmissions"

getEmissions_bp = Blueprint('getEmissions_bp', __name__)

# two required parameters
# 1) speed (rpm)
# 2) torque (Nm)
@getEmissions_bp.route(ROUTE, methods=['GET'])
def api():
    speed = request.args["speed"]
    torque = request.args["torque"]

    # the following does not work when called from flask
    # result = calcEmissions(speed, torque)

    # hence the following horrible way of running this
    result = subprocess.check_output(["python", "/app/speedLoadMap/ADMSMapSpeedTorqueNOxSoot.py", speed, torque]) 
    result_json = json.loads(result.decode("utf-8"))
    return jsonify(result_json), 200