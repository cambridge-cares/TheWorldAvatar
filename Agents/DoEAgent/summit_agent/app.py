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
    """
        This is an interval task wrapped around monitorDerivation() that is to be executed periodically.
        The timescale for this can be adjusted by changing parameter "PERIODIC_TIMESCALE" in /resources/doeagent_properties.py file.
    """
    monitorDerivation()

def monitorDerivation():
    """
        This method monitors the status of the derivation that "isDerivedUsing" DoE Agent.
        
        When it detects the status is "Requested", the agent will start the job and mark the status as "InProgress".
        Once the job is finished, the agent marks the status as "Finished" and attaches the new derived IRI to it via "hasNewDerivedIRI".
        
        When it detects the status is "InProgress", the currently implementation just passes.
        
        When it detects the status is "Finished", the agent deletes the old entities, 
        reconnects the new instances (previously attached to the status via "hasNewDerivedIRI") with the original derivation, 
        cleans up all the status, and finally updates the timestamp of the derivation.
        All these processing steps at the `Finished` status are taken care of by method 
        `uk.ac.cam.cares.jps.base.derivation.DerivationClient.cleanUpFinishedDerivationUpdate(String)`.
    """
    # Initialise the derivationClient with SPARQL Query and Update endpoint
    storeClient = jpsBaseLib_view.RemoteStoreClient(SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT)
    derivationClient = jpsBaseLib_view.DerivationClient(storeClient)
    # Retrieves a list of derivation that "isDerivedUsing" DoE Agent
    list_of_derivation = derivationClient.getDerivations(DOEAGENT_ONTOAGENT_SERVICE)

    # Iterate over the list of derivation, and do different things depend on the derivation status
    for d in list_of_derivation:
        # If "Requested", retrieve inputs, marks as "InProgress", start job, update status at job completion
        if (derivationClient.isRequested(d)):
            agent_inputs = getDoEAgentInputs(SPARQL_QUERY_ENDPOINT, d)
            derivationClient.markAsInProgress(d)
            ontodoe_new_exp_iri = setUpJob(agent_inputs)
            logger.info("The newly suggested experiments are referred by: " + ontodoe_new_exp_iri)
            derivationClient.updateStatusAtJobCompletion(d, [ontodoe_new_exp_iri])
        # If "InProgress", pass
        elif (derivationClient.isInProgress(d)):
            pass
        # If "Finished", do all the clean-up steps
        elif (derivationClient.isFinished(d)):
            derivationClient.cleanUpFinishedDerivationUpdate(d)

# Initialise logger
logger = agentlogging.get_logger("dev")

# Add interval_task (monitorDerivation) to scheduler for periodical execution once the Docker image is deployed
scheduler.add_job(id=INTERVAL_TASK_ID, func=interval_task, trigger='interval', seconds=PERIODIC_TIMESCALE)

# Show an instructional message at the app root
@app.route('/')
def default():
    """
        Instructional message at the app root.
    """
    msg  = "To see the result of an API call, enter a URL of the form:<BR>"
    msg += "&nbsp&nbsp [this_url]/doe/summit/suggest?[URL_encoded_JSON]<BR><BR>"
    msg += "&nbsp&nbsp [this_url] is the host and port currently shown in the address bar<BR><BR>"
    msg += "&nbsp&nbsp [URL_encoded_JSON] is URL-encoded version of a JSON input string<BR><BR>"
    msg += "&nbsp&nbsp An example of such a JSON input string can look like:<BR><BR>"
    msg += """
           &nbsp&nbsp {<BR>
                &nbsp&nbsp&nbsp&nbsp "agent_input": {<BR>
                &nbsp&nbsp&nbsp&nbsp "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#Strategy": "https://theworldavatar.com/kb/ontodoe/DoE_1/Strategy_1",<BR>
                &nbsp&nbsp&nbsp&nbsp "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#Domain": "https://theworldavatar.com/kb/ontodoe/DoE_1/Domain_1",<BR>
                &nbsp&nbsp&nbsp&nbsp "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#SystemResponse": 
                    ["https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_1", "https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_2"],<BR>
                &nbsp&nbsp&nbsp&nbsp "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#HistoricalData": "https://theworldavatar.com/kb/ontodoe/DoE_1/HistoricalData_1"<BR>
                &nbsp&nbsp&nbsp&nbsp }<BR>
            &nbsp&nbsp }
           """
    return msg

@app.route('/doe/summit/suggest', methods=['GET'])
def api():
    """
        This is the main entry point for a DoE request in the form of an HTTP request/response.
        An example of a correct request string should look something similar to (before URL-encode):
        {
            "agent_input": {
            "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#Strategy": "https://theworldavatar.com/kb/ontodoe/DoE_1/Strategy_1",
            "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#Domain": "https://theworldavatar.com/kb/ontodoe/DoE_1/Domain_1",
            "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#SystemResponse": 
                ["https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_1", "https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_2"],
            "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#HistoricalData": "https://theworldavatar.com/kb/ontodoe/DoE_1/HistoricalData_1"
            }
        }

        Response:
            the created OntoDoE:NewExperiment IRI
    """
    # Unquote the request URL to JSON string
    input_decoded = unquote(request.url[len(request.base_url)+1:])
    # Setup the job using the decoded JSON string
    ontodoe_new_exp_iri = setUpJob(input_decoded)
    return ontodoe_new_exp_iri

@app.route('/example', methods=['GET'])
def exampleEntryPoint():
    """
        As the monitorDerivation() is set to be running periodically once the DoE agent is deployed, 
        this page should serve as the entry point for creating a working example once the developer 
        has uploaded the correct triples to the knowledge graph endpoints, 
        i.e. it creates the derivation instance based on the example data and execute asynchronous 
        derivation update automatically.

        Response:
            the created OntoDerivation:Derivation instance
    """
    # Initialise derivationClient with SPARQL Query and Update endpoints
    storeClient = jpsBaseLib_view.RemoteStoreClient(SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT)
    derivationClient = jpsBaseLib_view.DerivationClient(storeClient)
    
    # Hardcode the IRI to be used for the example
    # Developers should upload the files containing these triples to the endpoints following the instructions in the README.md
    derived = ['https://theworldavatar.com/kb/ontodoe/DoE_1/NewExperiment_1']
    agentIRI = DOEAGENT_ONTOAGENT_SERVICE
    inputs = ['https://theworldavatar.com/kb/ontodoe/DoE_1/Strategy_1', 
    'https://theworldavatar.com/kb/ontodoe/DoE_1/Domain_1', 
    'https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_1', 
    'https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_2', 
    'https://theworldavatar.com/kb/ontodoe/DoE_1/HistoricalData_1']
    
    # Create derivation instance given above information, the timestamp of this derivation is 0
    derivationIRI = derivationClient.createDerivation(derived, agentIRI, inputs)

    # Iterate over the list of inputs to add and update the timestamp
    for input in inputs:
        derivationClient.addTimeInstance(input)
        # Update timestamp is needed as the timestamp added using addTimeInstance() is 0
        derivationClient.updateTimestamp(input)
    
    # Update the derivation asynchronous, it will only mark as "Requested"
    # The actual update will be handled by monitorDerivation method periodically run by DoE agent
    derivationClient.updateDerivationAsyn(derivationIRI)
    return f'Initialised successfully, created derivation instance <{derivationIRI}>'

def setUpJob(input_decoded):
    """
        This function sets up the job given the URL-decoded input JSON string.

        Arguments:
            input_decoded - URL-decoded input JSON string
                            an example is:
                            {
                                "agent_input": {
                                "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#Strategy": "https://theworldavatar.com/kb/ontodoe/DoE_1/Strategy_1",
                                "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#Domain": "https://theworldavatar.com/kb/ontodoe/DoE_1/Domain_1",
                                "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#SystemResponse": 
                                    ["https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_1", "https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_2"],
                                "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#HistoricalData": "https://theworldavatar.com/kb/ontodoe/DoE_1/HistoricalData_1"
                                }
                            }
    """
    # Load string to JSON object (python dict)
    input_json = json.loads(input_decoded) if not isinstance(input_decoded, dict) else input_decoded
    # Check if the input is in correct format, and return grouped instances for further usage
    strategy_instance, domain_instance, systemResponse_instances, historicalData_instance = checkInputParameters(input_json)
    # Call function to suggest the new experiment and return an instance of OntoDoE:NewExperiment
    ontodoe_new_exp_iri = suggest(strategy_instance, domain_instance, systemResponse_instances, historicalData_instance)
    return ontodoe_new_exp_iri

def suggest(strategy_instance, domain_instance, systemResponse_instances, historicalData_instance):
    """
        This method suggests the new experiment given information provided for design of experiment exercise.

        Arguments:
            strategy_instance - IRI of instance of OntoDoE:Strategy
            domain_instance - IRI of instance of OntoDoE:Domain
            systemResponse_instances - IRI of instance of OntoDoE:SystemResponse
            historicalData_instance - IRI of instance of OntoDoE:HistoricalData
    """
    endpoint = SPARQL_QUERY_ENDPOINT

    # Get the OntoDoE:DesignOfExperiment instances given the inputs, i.e. all the inputs should belong to the same OntoDoE:DesignOfExperiment instance
    doe_instance_dict = getDoEInstanceIRI(endpoint, strategy_instance, domain_instance, systemResponse_instances, historicalData_instance)

    # Get the information from OntoDoE:Strategy instance
    strategy_dict = getDoEStrategy(endpoint, strategy_instance)
    
    # Get the information from the OntoDoE:Domain, OntoDoE:SystemResponse, and OntoDoE:HistoricalData instances
    designVariable_dict, systemResponse_dict, previous_results = constructHistoricalDataTable(endpoint, domain_instance, systemResponse_instances, historicalData_instance)
    
    # Drop the columus of OntoRxn:ReactionExperiment/OntoRxn:ReactionVariation IRIs previously used for joining the tables,
    # as that information is not digestible by the Summit package
    # also convert the DataFrame values from 'str' to 'float'
    historicalData_dict = {"historicalData": previous_results.drop(columns="rxnexp").astype(float)}
    
    # Get the first instance of OntoRxn:ReactionExperiment in the list of OntoRxn:ReactionExperiment pointed by OntoDoE:HistoricalData instance
    # The returned first instance is to be the basis of the created OntoRxn:ReactionVariation instances
    first_experiment_dict = getFirstInstanceOfExperiment(endpoint, historicalData_instance)
    
    # Get the number of new experiment desired to be suggested from the historical data
    numOfNewExp_dict = getNumOfNewExpToGenerate(endpoint, historicalData_instance)
    
    # An example of doe_info
    # doe_info = { \
    #             "doe_instance": "https://theworldavatar.com/kb/ontodoe/DoE_1/DoE_1"
    #             "TSEMO": {"nSpectralPoints": 30, "nGenerations": 20, "populationSize": 20}, \
    #             "continuousVariables": [{"name": "ContinuousVariable_1", "lower": 1, "upper": 10}, 
    #             {"name": "ContinuousVariable_2", "lower": 0.02, "upper": 0.2},
    #             {"name": "ContinuousVariable_3", "lower": 5, "upper": 15},
    #             {"name": "ContinuousVariable_4", "lower": 30, "upper": 70}], \
    #             "systemResponses": [{"name": "SystemResponse_1", "direction": "maximise"}, 
    #             {"name": "SystemResponse_2", "direction": "minimise"}], \
    #             "historicalData": previous_results, \
    #             "first_exp": "https://theworldavatar.com/kb/ontorxn/ReactionExperiment_1/RxnExp_1", \
    #             "numOfExp": 1}
    doe_info = {**doe_instance_dict, **strategy_dict, **designVariable_dict, **systemResponse_dict, **historicalData_dict, **first_experiment_dict, **numOfNewExp_dict}

    # Proposes the next experiment, returns with a DataSet that contains numerical values been suggested
    next_exp = proposeNewExperiment(doe_info)
    # Upload the new experiment to the knowledge graph, and create an instance of OntoDoE:NewExperiment as ontodoe_new_exp_iri
    ontodoe_new_exp_iri = uploadNewExpToKG(doe_info, next_exp)

    logger.info(f"The proposed new experiment is recorded in <{ontodoe_new_exp_iri}>.")
    return ontodoe_new_exp_iri

def checkInputParameters(input_json):
    """
        This function checks the input parameters of the HTTP request against the I/O signiture as declared in the DoE Agent OntoAgent instance.
    """
    logger.info("Checking arguments...")
    exception_string = """Error: Inputs are not provided in correct form. An example is: 
                            {
                                "agent_input": {
                                "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#Strategy": "https://theworldavatar.com/kb/ontodoe/DoE_1/Strategy_1",
                                "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#Domain": "https://theworldavatar.com/kb/ontodoe/DoE_1/Domain_1",
                                "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#SystemResponse": 
                                    ["https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_1", "https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_2"],
                                "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#HistoricalData": "https://theworldavatar.com/kb/ontodoe/DoE_1/HistoricalData_1"
                                }
                            }"""
    # If the input JSON string is missing mandatory keys, raise error with "exception_string"
    if DOEAGENT_INPUT_JSON_KAY in input_json:
        if ONTODOE_STRATEGY in input_json[DOEAGENT_INPUT_JSON_KAY]:
            try:
                strategy = input_json[DOEAGENT_INPUT_JSON_KAY][ONTODOE_STRATEGY]
            except ValueError:
                logger.error("Unable to parse IRI.")
                raise Exception("Unable to interpret strategy ('%s') as an IRI." % input_json[DOEAGENT_INPUT_JSON_KAY][ONTODOE_STRATEGY])
        else:
            raise Exception(exception_string)
        if ONTODOE_DOMAIN in input_json[DOEAGENT_INPUT_JSON_KAY]:
            try:
                domain = input_json[DOEAGENT_INPUT_JSON_KAY][ONTODOE_DOMAIN]
            except ValueError:
                logger.error("Unable to parse IRI.")
                raise Exception("Unable to interpret domain ('%s') as an IRI." % input_json[DOEAGENT_INPUT_JSON_KAY][ONTODOE_DOMAIN])
        else:
            raise Exception(exception_string)

        if ONTODOE_SYSTEMRESPONSE in input_json[DOEAGENT_INPUT_JSON_KAY]:
            try:
                system_response = input_json[DOEAGENT_INPUT_JSON_KAY][ONTODOE_SYSTEMRESPONSE]
            except ValueError:
                logger.error("Unable to parse IRI.")
                raise Exception("Unable to interpret systemResponse ('%s') as an IRI." % input_json[DOEAGENT_INPUT_JSON_KAY][ONTODOE_SYSTEMRESPONSE])
        else:
            raise Exception(exception_string)

        if ONTODOE_HISTORICALDATA in input_json[DOEAGENT_INPUT_JSON_KAY]:
            try:
                historical_data = input_json[DOEAGENT_INPUT_JSON_KAY][ONTODOE_HISTORICALDATA]
            except ValueError:
                logger.error("Unable to parse IRI.")
                raise Exception("Unable to interpret historicalData ('%s') as an IRI." % input_json[DOEAGENT_INPUT_JSON_KAY][ONTODOE_HISTORICALDATA])
        else:
            raise Exception(exception_string)

    else:
        raise Exception(exception_string)
    
    return strategy, domain, system_response, historical_data
