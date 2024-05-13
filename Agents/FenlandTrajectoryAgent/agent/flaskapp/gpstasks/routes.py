from flask import Blueprint, request, jsonify
import os
import sys
import glob
from agent.utils.env_configs import DB_URL, DB_USER, DB_PASSWORD
import agent.datainstantiation.gps_client as gdi
from agent.kgutils.utils import utils
from agent.utils.baselib_gateway import jpsBaseLibGW
from agent.kgutils.kgclient import KGClient
from agent.kgutils.tsclient import TSClient

# Blueprint configuration
gps_instantiation_bp = Blueprint('gps_instantiation_bp', __name__)

# Define the route for instantiating GPS data
@gps_instantiation_bp.route('/fenlandtrajectoryagent/process_and_instantiate', methods=['POST'])
def process_and_instantiate():
    file_path = request.json.get('file_path')
    gps_object = gdi.process_gps_csv_file(file_path)
    if not gps_object:
        return jsonify({"error": "Failed to process file or file has missing/incomplete columns"}), 400
    kg_client, ts_client, double_class = gdi.setup_clients()
    result = gdi.instantiate_gps_data(gps_object, kg_client, ts_client, double_class)
    if result is None:
        return jsonify({"error": "Failed to instantiate GPS data"}), 500
    return jsonify({"message": "GPS data successfully processed and instantiated"}), 200


# if __name__ == '__main__':
#     @gps_instantiation_bp.run(debug=True, host='0.0.0.0', port=5000)