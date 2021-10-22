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
scheduler = APScheduler()
scheduler.init_app(app)
scheduler.start()

INTERVAL_TASK_ID = 'interval-task-id'

def interval_task():
    monitorDerivation()

def monitorDerivation():
    storeClient = jpsBaseLib_view.RemoteStoreClient(SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT)
    derivationClient = jpsBaseLib_view.DerivationClient(storeClient)
    list_of_derivation = jpsBaseLib_view.DerivationSparql.getDerivations(storeClient, DOEAGENT_ONTOAGENT_SERVICE)

    for d in list_of_derivation:
        if (jpsBaseLib_view.DerivationSparql.isRequested(storeClient, d)):
            agent_inputs = getDoEAgentInputs(SPARQL_QUERY_ENDPOINT, d)
            jpsBaseLib_view.DerivationSparql.markAsInProgress(storeClient, d)
            newexp_iri = setUpJob(agent_inputs)
            derivationClient.updateStatusAtJobCompletion(storeClient, newexp_iri if isinstance(newexp_iri, list) else [newexp_iri])
        elif (jpsBaseLib_view.DerivationSparql.isInProgress(storeClient, d)):
            pass
        elif (jpsBaseLib_view.DerivationSparql.isFinished(storeClient, d)):
            derivationClient.cleanUpFinishedDerivationUpdate(d)

# Initialise logger
logger = agentlogging.get_logger("dev")

scheduler.add_job(id=INTERVAL_TASK_ID, func=interval_task, trigger='interval', seconds=PERIODIC_TIMESCALE)

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
    new_exp_iri_list = setUpJob(input_decoded)
    # logger.info(str(type(historical_data_instances)))
    return '; '.join(new_exp_iri_list)

def setUpJob(input_decoded):
    input_json = json.loads(input_decoded)
    strategy_instance, domain_instance, systemResponse_instances, historicalData_instance = checkInputParameters(input_json)
    new_exp_iri_list = suggest(strategy_instance, domain_instance, systemResponse_instances, historicalData_instance)
    return new_exp_iri_list

def suggest(strategy_instance, domain_instance, systemResponse_instances, historicalData_instance):
    endpoint = SPARQL_QUERY_ENDPOINT

    strategy_dict = getDoEStrategy(endpoint, strategy_instance)
    designVariable_dict, systemResponse_dict, previous_results = constructHistoricalDataTable(endpoint, domain_instance, systemResponse_instances, historicalData_instance)
    historicalData_dict = {"historicalData": previous_results.drop(columns="rxnexp").astype(float)}
    first_experiment_dict = getFirstInstanceOfExperiment(endpoint, historicalData_instance)
    numOfNewExp_dict = getNumOfNewExpToGenerate(endpoint, historicalData_instance)
    
    doe_info = {**strategy_dict, **designVariable_dict, **systemResponse_dict, **historicalData_dict, **first_experiment_dict, **numOfNewExp_dict}

    next_exp = proposeNewExperiment(doe_info)
    new_exp_iri_list = uploadNewExpToKG(doe_info, next_exp)

    logger.info(DataSet.data_to_numpy(next_exp))
    logger.info(next_exp.to_dict())
    logger.info(next_exp['ContinuousVariable_1'])
    return new_exp_iri_list
    # return DataSet.data_to_numpy(next_exp)


def checkInputParameters(input_json):
    if DOEAGENT_INPUT_JSON_KAY in input_json:
        if ONTODOE_STRATEGY in input_json[DOEAGENT_INPUT_JSON_KAY]:
            try:
                strategy = input_json[DOEAGENT_INPUT_JSON_KAY][ONTODOE_STRATEGY]
            except ValueError:
                logger.error("Unable to parse IRI.")
                return "Unable to interpret strategy ('%s') as an IRI." % input_json[DOEAGENT_INPUT_JSON_KAY][ONTODOE_STRATEGY]
        if ONTODOE_DOMAIN in input_json[DOEAGENT_INPUT_JSON_KAY]:
            try:
                domain = input_json[DOEAGENT_INPUT_JSON_KAY][ONTODOE_DOMAIN]
            except ValueError:
                logger.error("Unable to parse IRI.")
                return "Unable to interpret domain ('%s') as an IRI." % input_json[DOEAGENT_INPUT_JSON_KAY][ONTODOE_DOMAIN]
        if ONTODOE_SYSTEMRESPONSE in input_json[DOEAGENT_INPUT_JSON_KAY]:
            try:
                system_response = input_json[DOEAGENT_INPUT_JSON_KAY][ONTODOE_SYSTEMRESPONSE]
            except ValueError:
                logger.error("Unable to parse IRI.")
                return "Unable to interpret systemResponse ('%s') as an IRI." % input_json[DOEAGENT_INPUT_JSON_KAY][ONTODOE_SYSTEMRESPONSE]
        if ONTODOE_HISTORICALDATA in input_json[DOEAGENT_INPUT_JSON_KAY]:
            try:
                historical_data = input_json[DOEAGENT_INPUT_JSON_KAY][ONTODOE_HISTORICALDATA]
            except ValueError:
                logger.error("Unable to parse IRI.")
                return "Unable to interpret historicalData ('%s') as an IRI." % input_json[DOEAGENT_INPUT_JSON_KAY][ONTODOE_HISTORICALDATA]
    else:
        return "Error: Inputs are not provided in correct form."
    
    return strategy, domain, system_response, historical_data