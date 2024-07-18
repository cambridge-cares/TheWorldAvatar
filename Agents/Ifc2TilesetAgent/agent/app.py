"""
# Author: qhouyee, picas9dan #

This module is the entrypoint to the agent, which accepts and
validates POST request before running the agent.
"""

# Third-party imports
import ifcopenshell
from flask import Flask, jsonify, request, render_template
from py4jps import agentlogging

# Self imports
from agent.utils import find_ifc_file, cleandir, validate_asset_url
from agent.ifc2gltf import conv2gltf
from agent.ifc2tileset import gen_tilesets
from agent.config import load_properties
from agent.exceptions import InvalidInputError

# Initialise logger
logger = agentlogging.get_logger("dev")

# Shared fields
asset_url = "./glb/"


def create_app():
    # create and configure the app
    app = Flask(__name__, instance_relative_config=True)

    # Show an instructional message at the app's home page
    @app.route('/')
    def default():
        return render_template("home.html")

    # Define a route for API requests
    @app.route('/api', methods=['POST'])
    def api():
        # Check arguments (query parameters)
        logger.info("Checking parameters...")
        try:
            data = request.get_json()
        except Exception as e:
            error_msg = str(e)
            logger.error(
                "Unable to get JSON from request! Have you used double quotes?")
            return jsonify({"Error": error_msg}), 400

        if "assetUrl" not in data:
            logger.error("Missing `assetUrl` parameter in request!")
            return jsonify({"Error": "Missing `assetUrl` parameter in request!"}), 400

        if not validate_asset_url(data["assetUrl"]):
            url_error_msg = f"`assetUrl` parameter <{data['assetUrl']}> is invalid. " \
                            f"It must start with `.`, `..`, or `http://`, and must not end with `/`"
            logger.error(url_error_msg)
            return jsonify({"Error": url_error_msg}), 400

        logger.debug("assetURL is valid!")
        global asset_url
        asset_url = data["assetUrl"] + "/"

        logger.info("Retrieving properties from yaml...")
        query_endpoint, update_endpoint, solar_panel_tileset, sewage_tileset, root_tileset_data = load_properties(
            './config/properties.yaml')

        logger.info("Cleaning the data directory...")
        cleandir()

        try:
            ifc_filepath = find_ifc_file(['data', 'ifc'])
        except (FileNotFoundError, InvalidInputError) as e:
            error_msg = str(e)
            logger.error(error_msg)
            return jsonify({"Error": error_msg}), 400

        logger.info("Validating the IFC model...")
        try:
            # Open the model via ifcopenshell to verify its validity
            ifcopenshell.open(ifc_filepath)
        except Exception as ex:
            error_msg = "IFC model validation fails. Cause: " + str(ex)
            logger.error(error_msg)
            return jsonify({"Error": error_msg}), 400

        logger.info("Converting the model into geometry files...")
        asset_data, building_data = conv2gltf(
            ifc_filepath, query_endpoint, update_endpoint)

        logger.info("Generating the tilesets...")
        gen_tilesets(asset_data, building_data, solar_panel_tileset,
                     sewage_tileset, root_tileset_data)

        # Return the result in JSON format
        return jsonify(
            {"result": "IFC model has successfully been converted. Please visit the 'data' directory for the outputs"}
        )

    return app
