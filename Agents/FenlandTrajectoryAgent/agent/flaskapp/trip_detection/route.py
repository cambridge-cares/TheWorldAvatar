from flask import Blueprint, request, jsonify
from agent.exposure_calculator.exposure_utilities import ExposureUtils

trip_detection_bp = Blueprint('trip_detection_bp', __name__)
exposure_util = ExposureUtils()

@trip_detection_bp('/trip_detection', methods=['POST'])
def trip_detect_for_TSCLIENT():
    
    return jsonify(final_results)


