# Third party imports
import ifcopenshell
from flask import Flask, jsonify, request
from py4jps import agentlogging

# Self imports
from agent.utils import read_ifc_file, cleandir
from agent.ifc2gltf import conv2gltf
from agent.ifc2tileset import gen_tilesets

# Create the Flask app object
app = Flask(__name__)

# Initialise logger
logger = agentlogging.get_logger("dev")

# Show an instructional message at the app's home page
@app.route('/')
def default():
    msg = "The Ifc2Tileset agent offers the following functionality at the specified API endpoint:<BR>"
    msg += "<BR>"
    msg += "(GET) request to convert IFC models to Cesium's 3D tilesets:<BR>"
    msg += "&nbsp&nbsp [this_url]/api<BR><BR>"
    msg += "&nbsp&nbsp [this_url] is the host and port currently shown in the address bar"
    return msg

# Define a route for API requests
@app.route('/api', methods=['POST'])
def api():
    # Check arguments (query parameters)
    logger.info("Checking parameters...")
    if request.method == 'POST':
        errormsg = "Invalid 'run' parameter. Only yes is accepted!"
        data = request.get_json(force=True)
        if data["run"].strip() != "yes":
            logger.error(errormsg)
            return errormsg
    else: 
        return "Invalid Request Method. Only POST request is accepted."

    cleandir()
    # Set the IFC model
    ifc_filepath = read_ifc_file(['data', 'ifc'])
    
    ifc = ifcopenshell.open(ifc_filepath)
    # Convert and split the ifc model into gltf files
    hashmapping = conv2gltf(ifc, ifc_filepath)

    # Generate tilesets
    gen_tilesets(hashmapping)
    # Return the result in JSON format
    return jsonify({"result": "IFC model has successfully been converted. Please visit the 'data' directory for the outputs"})

