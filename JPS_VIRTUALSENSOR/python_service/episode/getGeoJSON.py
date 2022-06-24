from episode.postprocessEpisode import postprocessEpisode
from flask import Blueprint, request, jsonify
import agentlogging

ROUTE = "/getGeoJSON"

getGeoJSON_bp = Blueprint('getGeoJSON_bp', __name__)
logger = agentlogging.get_logger("dev")

@getGeoJSON_bp.route(ROUTE, methods=['GET'])
def api():
    logger.info('Received GET request to produce GeoJSON')
    dispMatrix = request.args["dispMatrix"]
    crsName = request.args["crsName"]
    
    result = postprocessEpisode(dispMatrix, crsName)
    return jsonify(result), 200
