from flask import Blueprint, jsonify
import os
import glob
from agent.datainstantiation.gps_process_package import process_gps_csv_file, instantiate_gps_data
from agent.kgutils.utils import utils
from agent.utils.baselib_gateway import jpsBaseLibGW
from kgutils.kgclient import KGClient
from kgutils.tsclient import TSClient
from agent.utils.env_configs import DB_URL, DB_USER, DB_PASSWORD

# Blueprint configuration
gps_instantiation_bp = Blueprint('gps_instantiation_bp', __name__)

# Define the route for instantiating GPS data
@gps_instantiation_bp.route('/instantiate_gps_data', methods=['GET'])
def instantiate_gps_data_route():
    try:
        # Set the path to the target folder containing CSV files
        script_directory = os.path.dirname(os.path.realpath(__file__))
        target_folder_path = os.path.join(script_directory, '..', '..', 'datainstantiation', 'raw_data', 'gps_target_folder', '*.csv')

        # Find all CSV files in the target folder
        csv_files = glob.glob(target_folder_path)
        if not csv_files:
            return jsonify({'message': 'No CSV files found in the specified directory'})

        # Initialize database and RDF store
        utils.create_postgres_db()  # Initialize PostgreSQL database
        utils.create_blazegraph_namespace()  # Initialize Blazegraph namespace for RDF data

        # Initialize KGClient and TSClient
        kg_client = KGClient(utils.QUERY_ENDPOINT, utils.UPDATE_ENDPOINT)
        ts_client = TSClient(kg_client, DB_URL, DB_USER, DB_PASSWORD)

        # Process each CSV file
        for csv_file in csv_files:
            gps_object = process_gps_csv_file(csv_file)
            instantiate_gps_data(gps_object, kg_client, ts_client)

        return jsonify({'message': f'Successfully processed {len(csv_files)} GPS data files.'})

    except Exception as e:
        return jsonify({'error': str(e)}), 500

# Ensure to register the blueprint with your Flask app in the main app file
# app.register_blueprint(gps_instantiation_bp)