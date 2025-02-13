from flask import Blueprint, request, jsonify
import os
import sys
import glob
import shutil
from agent.utils.stack_configs import DB_URL, DB_USER, DB_PASSWORD,SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT
import agent.datainstantiation.gps_client as gdi
##from agent.datainstantiation.jpsSingletons import jpsBaseLibGW
from agent.datainstantiation.jpsSingletons import stackClientsGw
from agent.kgutils.kgclient import KGClient
from agent.kgutils.tsclient import TSClient
from agent.kgutils.utils import *
from agent.layergenerator.geoserver_gen import create_functions, create_geoserver_layer
import logging
from agent.datainstantiation.cleaning_tool import clean_gps_data

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# Blueprint configuration
gps_instantiation_bp = Blueprint('gps_instantiation_bp', __name__)

# Define the route for instantiating GPS data
@gps_instantiation_bp.route('/preprocess', methods=['POST'])
def preprocess_files():
    """
    Preprocess CSV files in the specified folder by cleaning them.
    The cleaning function removes units and retains only numeric values.
    Cleaned files are saved temporarily.
    """
    try:
        folder_path = request.json.get('file_path')
        if not folder_path:
            logger.error("File path is missing in the request.")
            return jsonify({"error": "File path is missing in the request."}), 400

        logger.info("Received request to preprocess files in folder: %s", folder_path)
        csv_files = glob.glob(os.path.join(folder_path, '*.csv'))
        if not csv_files:
            logger.error("No CSV files found in the specified folder: %s", folder_path)
            return jsonify({"error": "No CSV files found in the specified folder."}), 400

        temp_output_dir = os.path.join(folder_path, "temp_cleaned")
        results = []
        for csv_file in csv_files:
            logger.info("Cleaning file: %s", csv_file)
            cleaned_df, temp_file = clean_gps_data(csv_file, output_dir=temp_output_dir)
            if not temp_file:
                logger.error("Failed to save cleaned file for: %s", csv_file)
                results.append({"file": csv_file, "status": "failed"})
            else:
                results.append({"file": csv_file, "status": "success", "cleaned_file": temp_file})
        return jsonify({"message": "Files preprocessed successfully", "results": results}), 200
    except Exception as e:
        logger.error("Error in preprocess_files: %s", e, exc_info=True)
        return jsonify({"error": "Internal Server Error", "details": str(e)}), 500

@gps_instantiation_bp.route('/instantiate', methods=['POST'])
def instantiate_files():
    """
    Instantiate data by first cleaning CSV files from the specified folder,
    then processing the cleaned files using TSCLIENT.
    After processing, the temporary cleaned files are deleted.
    """
    try:
        folder_path = request.json.get('file_path')
        if not folder_path:
            logger.error("File path is missing in the request.")
            return jsonify({"error": "File path is missing in the request."}), 400

        logger.info("Received request to instantiate files from folder: %s", folder_path)
        csv_files = glob.glob(os.path.join(folder_path, '*.csv'))
        if not csv_files:
            logger.error("No CSV files found in the specified folder: %s", folder_path)
            return jsonify({"error": "No CSV files found in the specified folder."}), 400

        results = []
        temp_output_dir = os.path.join(folder_path, "temp_cleaned")
        if not os.path.exists(temp_output_dir):
            os.makedirs(temp_output_dir)
        for csv_file in csv_files:
            logger.info("Cleaning file: %s", csv_file)
            cleaned_df, temp_file = clean_gps_data(csv_file, output_dir=temp_output_dir)
            if not temp_file:
                logger.error("Failed to clean file: %s", csv_file)
                results.append({"file": csv_file, "status": "failed", "error": "Cleaning failed"})
                continue

            try:
                logger.info("Processing cleaned file: %s", temp_file)
                gps_object = gdi.process_gps_csv_file(temp_file)
                if not gps_object:
                    logger.warning("Failed to process cleaned file: %s", temp_file)
                    results.append({"file": csv_file, "status": "failed", "error": "Processing failed"})
                    continue

                kg_client, ts_client, double_class, point_class = gdi.setup_clients()
                gdi.instantiate_gps_data(gps_object, kg_client, ts_client, double_class, point_class)
                results.append({"file": csv_file, "status": "success", "cleaned_file": temp_file})
            except Exception as e:
                logger.error("Error instantiating data for file %s: %s", csv_file, e, exc_info=True)
                results.append({"file": csv_file, "status": "failed", "error": str(e)})

        if os.path.exists(temp_output_dir):
            shutil.rmtree(temp_output_dir)
            logger.info("Temporary cleaned files deleted: %s", temp_output_dir)

        return jsonify({"message": "Files processed and instantiated successfully", "results": results}), 200

    except Exception as e:
        logger.error("Error in instantiate_files: %s", e, exc_info=True)
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