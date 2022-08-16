from flask import Flask, jsonify, request
from .poly_model import PolyModel
import agentlogging

# Create the Flask app object
app = Flask(__name__)

# Initialise logger
logger = agentlogging.get_logger("dev")

# Show an instructional message at the app root
@app.route('/')
def default():
    msg  = "To see the result of an API call, enter a URL of the form:<BR>"
    msg += "&nbsp&nbsp [this_url]/api/v1/evaluate?val=[VAL]&order=[ORDER]<BR><BR>"
    msg += "&nbsp&nbsp (where [VAL] is a float and [ORDER] is an integer between 0 and 2)"
    msg += "&nbsp&nbsp [this_url] is the host and port currently shown in the address bar"
    return msg

# Define a route for API requests
@app.route('/api/v1/evaluate', methods=['GET'])
def api():
    
    # Check arguments (query parameters)
    logger.info("Checking arguments...")
    if 'val' in request.args:
        try:
            val = float(request.args['val'])
        except ValueError:
            logger.error("Unable to parse number.")
            return "Unable to interpret val ('%s') as a float." % request.args['val']
    else:
        return "Error: No 'val' parameter provided."

    if 'order' in request.args:
        try:
            order = int(request.args['order'])
        except ValueError:
            logger.error("Unable to parse integer.")
            return "Unable to interpret order ('%s') as an integer." % request.args['order']
    else:
        # Default to 2nd order
        order = 2

    try:
        # Construct and evaluate the model
        model = PolyModel(order)
        result = model.evaluate(val)
        # Return the result in JSON format
        return jsonify({"result": result})
    except ValueError as ex:
        return str(ex)