from flask import Blueprint, request, jsonify
from agent.exposure_calculator.exposure_utilities import ExposureUtils

trip_detection_bp = Blueprint('exposure_bp', __name__)