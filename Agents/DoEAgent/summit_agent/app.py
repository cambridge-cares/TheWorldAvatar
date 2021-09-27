from flask import Flask, jsonify, request
# from summit_doe import DoEModel
from urllib.parse import unquote
from .kgUtils import *
import json
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

# # Define a route for API requests
# @app.route('/api/v1/evaluate', methods=['GET'])
# def api():
    
#     # Check arguments (query parameters)
#     logger.info("Checking arguments...")
#     if 'val' in request.args:
#         try:
#             val = float(request.args['val'])
#         except ValueError:
#             logger.error("Unable to parse number.")
#             return "Unable to interpret val ('%s') as a float." % request.args['val']
#     else:
#         return "Error: No 'val' parameter provided."

#     if 'order' in request.args:
#         try:
#             order = int(request.args['order'])
#         except ValueError:
#             logger.error("Unable to parse integer.")
#             return "Unable to interpret order ('%s') as an integer." % request.args['order']
#     else:
#         # Default to 2nd order
#         order = 2

#     try:
#         # Construct and evaluate the model
#         model = PolyModel(order)
#         result = model.evaluate(val)
#         # Return the result in JSON format
#         return jsonify({"result": result})
#     except ValueError as ex:
#         return str(ex)

@app.route('/doe/summit/suggest', methods=['GET'])
def api():
    
    # Check arguments (query parameters)
    logger.info("Checking arguments...")
    input_decoded = unquote(request.url[len(request.base_url)+1:])
    input_json = json.loads(input_decoded)
    strategy, domain, system_response, historical_data = checkInputParameters(input_json)
    logger.info(str(type(historical_data)))



    response1 = performQuery("ontokin", "PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#> \
                                      PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>   SELECT ?mechanismIRI \
                                      WHERE   { ?mechanismIRI rdf:type ontokin:ReactionMechanism .} LIMIT 10")

    # if 'val' in request.args:
    #     try:
    #         val = float(request.args['val'])
    #     except ValueError:
    #         logger.error("Unable to parse number.")
    #         return "Unable to interpret val ('%s') as a float." % request.args['val']
    # else:
    #     return "Error: No 'val' parameter provided."

    # if 'order' in request.args:
    #     try:
    #         order = int(request.args['order'])
    #     except ValueError:
    #         logger.error("Unable to parse integer.")
    #         return "Unable to interpret order ('%s') as an integer." % request.args['order']
    # else:
    #     # Default to 2nd order
    #     order = 2

    # try:
    #     # Construct and evaluate the model
    #     model = DoEModel()
    #     result = model.suggest()
    #     # Return the result in JSON format
    #     return jsonify({"result": result})
    # except ValueError as ex:
    #     return str(ex)
    # return str(strategy) + str(domain) + str(system_response) + str(historical_data)
    return response1

def checkInputParameters(input_json):
    if "agent_input" in input_json:
        if "strategy" in input_json["agent_input"]:
            try:
                strategy = input_json["agent_input"]["strategy"]
            except ValueError:
                logger.error("Unable to parse IRI.")
                return "Unable to interpret strategy ('%s') as an IRI." % input_json["agent_input"]["strategy"]
        if "domain" in input_json["agent_input"]:
            try:
                domain = input_json["agent_input"]["domain"]
            except ValueError:
                logger.error("Unable to parse IRI.")
                return "Unable to interpret domain ('%s') as an IRI." % input_json["agent_input"]["domain"]
        if "systemResponse" in input_json["agent_input"]:
            try:
                system_response = input_json["agent_input"]["systemResponse"]
            except ValueError:
                logger.error("Unable to parse IRI.")
                return "Unable to interpret systemResponse ('%s') as an IRI." % input_json["agent_input"]["systemResponse"]
        if "historicalData" in input_json["agent_input"]:
            try:
                historical_data = input_json["agent_input"]["historicalData"]
            except ValueError:
                logger.error("Unable to parse IRI.")
                return "Unable to interpret historicalData ('%s') as an IRI." % input_json["agent_input"]["historicalData"]
    else:
        return "Error: Inputs are not provided in correct form."
    
    return strategy, domain, system_response, historical_data