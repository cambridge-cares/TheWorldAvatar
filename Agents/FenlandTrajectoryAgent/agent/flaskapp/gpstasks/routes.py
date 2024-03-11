# The purpose of this module is to print available HTTP requests (i.e. routes)
# at the application root

from flask import Blueprint, jsonify
import pandas as pd
import os
import glob
from datetime import datetime
from agent.kgutils.utils import utils
from agent.datainstantiation.jpsSingletons import jpsBaseLibView
import uuid

# Blueprint configuration
gps_instantiation_bp = Blueprint('gps_instantiation_bp', __name__)

# Define the route for instantiating GPS data
@gps_instantiation_bp.route('/instantiate_gps_data', methods=['GET'])
def instantiate_gps_data():
    try:
        # Set the path to the target folder containing CSV files
        script_directory = os.path.dirname(os.path.realpath(__file__))
        target_folder_path = os.path.join(script_directory, 'raw_data', 'gps_target_folder', '*.csv')

        # Find all CSV files in the target folder
        csv_files = glob.glob(target_folder_path)
        if not csv_files:
            return jsonify({'message': 'No CSV files found in the specified directory'})

        # Process each CSV file
        for csv_file in csv_files:
            # Include your data processing logic here
            # For simplicity, the example below assumes the existence of a function 'process_gps_csv_file'
            process_gps_csv_file(csv_file)

        return jsonify({'message': f'Successfully processed {len(csv_files)} GPS data files.'})

    except Exception as e:
        return jsonify({'error': str(e)}), 500

def process_gps_csv_file(csv_file):
    """
    Process a single GPS CSV file and instantiate its data.
    This function should contain the logic for reading the CSV,
    transforming data, and uploading it to the knowledge graph and PostGIS.
    """
    print(f"Processing file: {csv_file}")
    # Implement the detailed processing logic here

# Ensure to register the blueprint with your Flask app in the main app file
# app.register_blueprint(gps_instantiation_bp)
