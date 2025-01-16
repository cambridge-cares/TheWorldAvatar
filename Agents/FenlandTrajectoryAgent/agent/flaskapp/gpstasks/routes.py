from flask import Blueprint, request, jsonify
import os
import sys
import glob
from agent.utils.stack_configs import DB_URL, DB_USER, DB_PASSWORD,SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT
import agent.datainstantiation.gps_client as gdi
##from agent.datainstantiation.jpsSingletons import jpsBaseLibGW
from agent.datainstantiation.jpsSingletons import stackClientsGw
from agent.kgutils.kgclient import KGClient
from agent.kgutils.tsclient import TSClient
from agent.kgutils.utils import *
from agent.layergenerator.geoserver_gen import create_functions, create_geoserver_layer
import logging

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# Blueprint configuration
gps_instantiation_bp = Blueprint('gps_instantiation_bp', __name__)

# Define the route for instantiating GPS data
@gps_instantiation_bp.route('/preprocess', methods=['POST'])
def load_and_preprocess():
    try:
        file_path = request.json.get('file_path')
        if not file_path:
            logger.error("File path is missing in the request.")
            return jsonify({"error": "File path is missing in the request."}), 400

        logger.info(f"Received request to load and preprocess files at: {file_path}")
        csv_files = glob.glob(os.path.join(file_path, '*.csv'))
        if not csv_files:
            logger.error("No CSV files found at the specified path")
            return jsonify({"error": "No CSV files found at the specified path"}), 400

        results = []
        for csv_file in csv_files:
            logger.info(f"Loading and preprocessing file: {csv_file}")
            gps_object = gdi.process_gps_csv_file(csv_file)
            if not gps_object:
                logger.warning(f"Failed to preprocess file: {csv_file}")
                results.append({"file": csv_file, "status": "failed", "error": "Failed to preprocess file"})
            else:
                results.append({"file": csv_file, "status": "success"})

        if all(res["status"] == "failed" for res in results):
            logger.error("Failed to preprocess all files")
            return jsonify({"error": "Failed to preprocess all files", "results": results}), 500

        logger.info("Files loaded and preprocessed successfully")
        return jsonify({"message": "Files loaded and preprocessed", "results": results}), 200

    except Exception as e:
        logger.error(f"Error in load_and_preprocess: {str(e)}")
        return jsonify({"error": "Internal Server Error", "details": str(e)}), 500

@gps_instantiation_bp.route('/instantiate', methods=['POST'])
def process_and_instantiate():
    try:
        file_path = request.json.get('file_path')
        if not file_path:
            logger.error("File path is missing in the request.")
            return jsonify({"error": "File path is missing in the request."}), 400

        logger.info(f"Received request to process files at: {file_path}")
        csv_files = glob.glob(os.path.join(file_path, '*.csv'))
        if not csv_files:
            logger.error("No CSV files found at the specified path")
            return jsonify({"error": "No CSV files found at the specified path"}), 400

        results = []
        for csv_file in csv_files:
            logger.info(f"Processing file: {csv_file}")
            gps_object = gdi.process_gps_csv_file(csv_file)
            if not gps_object:
                logger.warning(f"Failed to process file: {csv_file}")
                results.append({"file": csv_file, "status": "failed", "error": "Failed to process file"})
                continue

            try:
                kg_client, ts_client, double_class, point_class = gdi.setup_clients()
                gdi.instantiate_gps_data(gps_object, kg_client, ts_client, double_class, point_class)
                results.append({"file": csv_file, "status": "success"})
            except Exception as e:
                logger.error(f"Error instantiating data for file {csv_file}: {e}")
                results.append({"file": csv_file, "status": "failed", "error": str(e)})

        if all(res["status"] == "failed" for res in results):
            logger.error("Failed to process and instantiate all files")
            return jsonify({"error": "Failed to process and instantiate all files", "results": results}), 500

        logger.info("GPS data processed and instantiated successfully")
        return jsonify({"message": "GPS data processed and instantiated", "results": results}), 200

    except AssertionError as e:
        logger.error(f"Assertion error during processing: {str(e)}")
        return jsonify({"error": str(e)}), 400
    except Exception as e:
        logger.error(f"Error in process_and_instantiate: {str(e)}")
        return jsonify({"error": "Internal Server Error", "details": str(e)}), 500

@gps_instantiation_bp.route('/layer_generator', methods=['POST'])
def create_layer():
    data = request.get_json()
    table_name = data.get('table_name')
    lat_column = data.get('lat_column', 'latitude')
    lon_column = data.get('lon_column', 'longitude')

    if not table_name:
        return jsonify({"message": "Table name is required"}), 400

    # Create functions in PostGIS if necessary
    try:
        create_functions()
    except Exception as e:
        return jsonify({"message": "Failed to create functions", "details": str(e)}), 500

    # Create GeoServer layer
    layer_creation_response = create_geoserver_layer(table_name, lat_column, lon_column)
    if not layer_creation_response.ok:
        return jsonify({"message": "Failed to create GeoServer layer", "details": layer_creation_response.text}), 500

    return jsonify({"message": "GeoServer layer created successfully"})