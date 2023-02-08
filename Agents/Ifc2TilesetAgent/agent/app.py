"""
# Author: qhouyee #

This module is the entrypoint to the agent, which accepts and
validates POST request before running the agent.
"""

# Third party imports
import ifcopenshell
from flask import Flask, jsonify, request
from py4jps import agentlogging

# Self imports
from agent.utils import read_ifc_file, cleandir, validate_asset_url
from agent.ifc2gltf import conv2gltf
from agent.ifc2tileset import gen_tilesets
from agent.exceptions import InvalidInputError
from agent.config import set_properties


# Initialise logger
logger = agentlogging.get_logger("dev")

# Shared fields
asset_url = "./gltf/"

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
        if request.method == 'POST':
            data = request.get_json(force=True)
            if "assetUrl" in data:
                global asset_url
                asset_url = validate_asset_url(data["assetUrl"])
                logger.debug("assetURL is valid!")
            else:
                raise InvalidInputError("Missing `assetUrl` parameter in request!")
        else:
            return "Invalid Request Method. Only POST request is accepted."

        logger.info("Retrieving properties from yaml...")
        query_endpoint, update_endpoint = set_properties('./config/properties.yaml')
        logger.info("Cleaning the data directory...")
        cleandir()

        logger.info("Validating the IFC model...")
        ifc_filepath = read_ifc_file(['data', 'ifc'])
        # Open the model via ifcopenshell to verify its validity
        ifcopenshell.open(ifc_filepath)

        logger.info("Converting the model into glTF files...")
        metadata = conv2gltf(ifc_filepath, query_endpoint, update_endpoint)

        logger.info("Generating the tilesets...")
        gen_tilesets(metadata, query_endpoint, update_endpoint)
        # Return the result in JSON format
        return jsonify({"result": "IFC model has successfully been converted." +
        "Please visit the 'data' directory for the outputs"})
    
    return app