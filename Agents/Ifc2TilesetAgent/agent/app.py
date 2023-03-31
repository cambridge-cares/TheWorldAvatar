"""
# Author: qhouyee #

This module is the entrypoint to the agent, which accepts and
validates POST request before running the agent.
"""

# Third party imports
import ifcopenshell
from flask import Flask, jsonify, request
from py4jps import agentlogging

from agent.exceptions import InvalidInputError
# Self imports
from agent.utils import find_ifc_file, cleandir, validate_asset_url
from agent.ifc2gltf import conv2gltf
from agent.ifc2tileset import gen_tilesets
from agent.config import set_properties

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
        msg = "The Ifc2Tileset agent offers the following functionality at the specified API endpoint:<BR>"
        msg += "<BR>"
        msg += "(POST) request to convert IFC models to Cesium's 3D tilesets:<BR>"
        msg += "&nbsp&nbsp [this_url]/api<BR>"
        msg += "&nbsp&nbsp [this_url] is the host and port currently shown in the address bar"
        return msg

    # Define a route for API requests
    @app.route('/api', methods=['POST'])
    def api():
        # Check arguments (query parameters)
        logger.info("Checking parameters...")
        data = request.get_json(force=True)

        if "assetUrl" not in data:
            logger.error("Missing `assetUrl` parameter in request!")
            return jsonify({"data": "Missing `assetUrl` parameter in request!"}), 400

        if not validate_asset_url(data["assetUrl"]):
            url_error_msg = f"`assetUrl` parameter <{data['assetUrl']}> is invalid. " \
                            f"It must start with `.`, `..`, or `http://`, and must not end with `/`"
            logger.error(url_error_msg)
            return jsonify({"data": url_error_msg}), 400

        logger.debug("assetURL is valid!")
        global asset_url
        asset_url = data["assetUrl"] + "/"

        logger.info("Retrieving properties from yaml...")
        query_endpoint, update_endpoint = set_properties('./config/properties.yaml')
        logger.info("Cleaning the data directory...")
        cleandir()

        try:
            ifc_filepath = find_ifc_file(['data', 'ifc'])
        except (FileNotFoundError, InvalidInputError) as e:
            error_msg = str(e)
            logger.error(error_msg)
            return jsonify({"data": error_msg}), 400

        logger.info("Validating the IFC model...")
        try:
            # Open the model via ifcopenshell to verify its validity
            ifcopenshell.open(ifc_filepath)
        except Exception as ex:
            error_msg = "IFC model validation fails. Cause: " + str(ex)
            logger.error(error_msg)
            return jsonify({"data": error_msg}), 400

        logger.info("Converting the model into glTF files...")
        asset_data, building_iri = conv2gltf(ifc_filepath, query_endpoint, update_endpoint)

        logger.info("Generating the tilesets...")
        gen_tilesets(asset_data, building_iri)

        # Return the result in JSON format
        return jsonify(
            {"result": "IFC model has successfully been converted. Please visit the 'data' directory for the outputs"}
        )

    return app
