from flask import Blueprint, request, jsonify
import os
import sys
import glob
from agent.utils.stack_configs import DB_URL, DB_USER, DB_PASSWORD
import agent.datainstantiation.gps_client as gdi
from agent.utils.baselib_gateway import jpsBaseLibGW
from agent.kgutils.kgclient import KGClient
from agent.kgutils.tsclient import TSClient
from agent.kgutils.utils import *
import logging

logging.basicConfig(level=logging.INFO)

# Blueprint configuration
gps_instantiation_bp = Blueprint('gps_instantiation_bp', __name__)

# Define the route for instantiating GPS data
@gps_instantiation_bp.route('/fenlandtrajectoryagent/process_and_instantiate', methods=['POST'])
def process_and_instantiate():
    try:
        file_path = request.json.get('file_path')
        logging.info(f"Received request to process files at: {file_path}")

        csv_files = glob.glob(os.path.join(file_path, '*.csv'))
        if not csv_files:
            logging.error("No CSV files found at the specified path")
            return jsonify({"error": "No CSV files found at the specified path"}), 400

        results = []
        for csv_file in csv_files:
            gps_object = gdi.process_gps_csv_file(csv_file)
            if not gps_object:
                results.append({"file": csv_file, "status": "failed"})
                continue

            kg_client, ts_client, double_class = gdi.setup_clients()
            gdi.instantiate_gps_data(gps_object, kg_client, ts_client, double_class)
            results.append({"file": csv_file, "status": "success"})

        if all(res["status"] == "failed" for res in results):
            logging.error("Failed to process and instantiate all files")
            return jsonify({"error": "Failed to process and instantiate all files"}), 500

        logging.info("GPS data processed and instantiated successfully")
        return jsonify({"message": "GPS data processed and instantiated", "results": results}), 200
    except Exception as e:
        logging.error(f"Error in process_and_instantiate: {e}")
        return jsonify({"error": "Internal Server Error"}), 500
