from flask import Flask, jsonify, request
from urllib.parse import unquote

from .create_ontorxn import uploadNewExpToKG
from .kgUtils import *
from .summit_doe import *
import json
import agentlogging
from flask_apscheduler import APScheduler
import random
# Create the Flask app object
app = Flask(__name__)

# Initialise the scheduler
# scheduler = APScheduler()
# scheduler.init_app(app)
# scheduler.start()

INTERVAL_TASK_ID = 'interval-task-id'

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

@app.route('/doe/summit/suggest', methods=['GET'])
def api():
    # Check arguments (query parameters)
    logger.info("Checking arguments...")
    input_decoded = unquote(request.url[len(request.base_url)+1:])
    input_json = json.loads(input_decoded)
    strategy_instance, domain_instance, systemResponse_instances, historicalData_instance = checkInputParameters(input_json)
    new_exp = suggest(strategy_instance, domain_instance, systemResponse_instances, historicalData_instance)
    # logger.info(str(type(historical_data_instances)))
    return new_exp

def suggest(strategy_instance, domain_instance, systemResponse_instances, historicalData_instance):
    endpoint = SPARQL_QUERY_ENDPOINT

    strategy_dict = getDoEStrategy(endpoint, strategy_instance)
    designVariable_dict, systemResponse_dict, previous_results = constructHistoricalDataTable(endpoint, domain_instance, systemResponse_instances, historicalData_instance)
    historicalData_dict = {"historicalData": previous_results.drop(columns="rxnexp").astype(float)}
    
    doe_info = {**strategy_dict, **designVariable_dict, **systemResponse_dict, **historicalData_dict}

    next_exp = proposeNewExperiment(doe_info)
    exp_kg = uploadNewExpToKG(next_exp)

    logger.info(DataSet.data_to_numpy(next_exp))
    logger.info(next_exp.to_dict())
    logger.info(next_exp['ContinuousVariable_1'])
    return next_exp.to_json()
    # return DataSet.data_to_numpy(next_exp)


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