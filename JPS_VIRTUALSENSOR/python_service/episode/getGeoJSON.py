from episode.postprocessEpisode import postprocessEpisode
from flask import Blueprint, request, jsonify

ROUTE = "/getGeoJSON"

getGeoJSON_bp = Blueprint('getGeoJSON_bp', __name__)

@getGeoJSON_bp.route(ROUTE, methods=['GET'])
def api():
    dispMatrix = request.args["dispMatrix"]
    crsName = request.args["crsName"]
    result = postprocessEpisode(dispMatrix, crsName)
    return jsonify(result), 200
