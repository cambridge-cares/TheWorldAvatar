from dataclasses import asdict
from pathlib import Path
from typing import List
import json
import os

from pyasyncagent import AsyncAgent
import agentlogging

from kg_operations import *
from doe_algo import *

from flask import Flask
from conf import *

# Initialise logger
logger = agentlogging.get_logger("dev")

class DoEAgent(AsyncAgent):
    def setupJob(self, agentInputs) -> List[str]:
        """
            This function sets up the job given the URL-decoded input JSON string.

            Arguments:
                agentInputs - URL-decoded input JSON string
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
        input_json = json.loads(agentInputs) if not isinstance(agentInputs, dict) else agentInputs

        # Create sparql_client
        self.sparql_client = SparqlClient(
            self.kgUrl, self.kgUrl, self.kgUser, self.kgPassword
        )
        # Check if the input is in correct format, and return OntoDoE.DesignOfExperiment instance
        doe_instance = self.collectInputsInformation(input_json)
        logger.info("Collected inputs from the knowledge graph: ")
        logger.info(json.dumps(asdict(doe_instance)))

        # Call function to suggest the new experiment and return an instance of dataclass OntoDoE.NewExperiment
        doe_instance_new_exp = suggest(doe_instance)

        # Upload the created OntoDoE:NewExperiment (including OntoRxn:ReactionVariation) triples to KG
        # Also update the triple between OntoDoE:DesignOfExperiment and OntoDoE:NewExperiment
        self.sparql_client.updateNewExperimentInKG(doe_instance, doe_instance_new_exp)

        logger.info(f"The proposed new experiment is recorded in <{doe_instance_new_exp.instance_iri}>.")
        return [doe_instance_new_exp.instance_iri]

    def collectInputsInformation(self, agent_inputs) -> DesignOfExperiment:
        """
            This function checks the input parameters of the HTTP request against the I/O signiture as declared in the DoE Agent OntoAgent instance and collects information.
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
        agent_input_key = str(jpsBaseLib_view.DerivationClient.AGENT_INPUT_KEY)
        if agent_input_key in agent_inputs:
            input_json = agent_inputs[agent_input_key]

            if ONTODOE_STRATEGY in input_json:
                try:
                    # Get the information from OntoDoE:Strategy instance
                    strategy_instance = self.sparql_client.getDoEStrategy(input_json[ONTODOE_STRATEGY])
                except ValueError:
                    logger.error("Unable to interpret strategy ('%s') as an IRI." % input_json[ONTODOE_STRATEGY])
                    raise Exception("Unable to interpret strategy ('%s') as an IRI." % input_json[ONTODOE_STRATEGY])
            else:
                logger.error('OntoDoE:Strategy instance might be missing.\n' + exception_string)
                raise Exception('OntoDoE:Strategy instance might be missing.\n' + exception_string)

            if ONTODOE_DOMAIN in input_json:
                try:
                    domain_instance = self.sparql_client.getDoEDomain(input_json[ONTODOE_DOMAIN])
                except ValueError:
                    logger.error("Unable to interpret domain ('%s') as an IRI." % input_json[ONTODOE_DOMAIN])
                    raise Exception("Unable to interpret domain ('%s') as an IRI." % input_json[ONTODOE_DOMAIN])
            else:
                logger.error('OntoDoE:Domain instance might be missing.\n' + exception_string)
                raise Exception('OntoDoE:Domain instance might be missing.\n' + exception_string)

            if ONTODOE_SYSTEMRESPONSE in input_json:
                try:
                    system_response_instance = self.sparql_client.getSystemResponses(input_json[ONTODOE_SYSTEMRESPONSE])
                except ValueError:
                    logger.error("Unable to interpret systemResponse ('%s') as an IRI." % input_json[ONTODOE_SYSTEMRESPONSE])
                    raise Exception("Unable to interpret systemResponse ('%s') as an IRI." % input_json[ONTODOE_SYSTEMRESPONSE])
            else:
                logger.error('OntoDoE:SystemResponse instances might be missing.\n' + exception_string)
                raise Exception('OntoDoE:SystemResponse instances might be missing.\n' + exception_string)

            if ONTODOE_HISTORICALDATA in input_json:
                try:
                    historical_data_instance = self.sparql_client.getDoEHistoricalData(input_json[ONTODOE_HISTORICALDATA])
                except ValueError:
                    logger.error("Unable to interpret historicalData ('%s') as an IRI." % input_json[ONTODOE_HISTORICALDATA])
                    raise Exception("Unable to interpret historicalData ('%s') as an IRI." % input_json[ONTODOE_HISTORICALDATA])
            else:
                logger.error('OntoDoE:HistoricalData instance might be missing.\n' + exception_string)
                raise Exception('OntoDoE:HistoricalData instance might be missing.\n' + exception_string)

            doe_instance = DesignOfExperiment(
                instance_iri=None, 
                usesStrategy=strategy_instance,
                hasDomain=domain_instance,
                hasSystemResponse=system_response_instance,
                utilisesHistoricalData=historical_data_instance,
                proposesNewExperiment=None) # TODO maybe also initialise NewExperiment?

            # Get the OntoDoE:DesignOfExperiment instances given the inputs, i.e. all the inputs should belong to the same OntoDoE:DesignOfExperiment instance
            doe_instance = self.sparql_client.getDoEInstanceIRI(doe_instance)
            return doe_instance

        else:
            logger.error('Agent input key (%s) might be missing.\n' % (agent_input_key) + exception_string)
            raise Exception('Agent input key (%s) might be missing.\n' % (agent_input_key) + exception_string)

def suggest(doe_instance: DesignOfExperiment) -> NewExperiment:
    """
        This method suggests the new experiment given information provided for design of experiment exercise.

        Arguments:
            doe_instance - instance of dataclass OntoDoE.DesignOfExperiment
    """

    # TODO this method calls summit doe, can be expanded in the future
    new_exp = proposeNewExperiment(doe_instance)

    return new_exp

# Show an instructional message at the DoEAgent servlet root
def default():
    """
        Instructional message at the app root.
    """
    msg  = "This is an asynchronous agent that capable of conducting Design Of Experiment (DoE).<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/133-dev-design-of-experiment/Agents/DoEAgent#readme<BR>"
    # TODO change above line to https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/Agents/DoEAgent#readme, before merging back to develop branch
    return msg

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
    config = DoEAgentConfig(str(Path(__file__).absolute().parent) + '/conf/doeagent_properties.json')

    # Initialise derivationClient with SPARQL Query and Update endpoints
    storeClient = jpsBaseLib_view.RemoteStoreClient(config.SPARQL_QUERY_ENDPOINT, config.SPARQL_UPDATE_ENDPOINT)
    derivationClient = jpsBaseLib_view.DerivationClient(storeClient, config.DERIVATION_INSTANCE_BASE_URL)

    clearAll = """DELETE {?s ?p ?o} \
               WHERE {?s ?p ?o}
               """

    sparql_client = SparqlClient(
            config.SPARQL_QUERY_ENDPOINT, config.SPARQL_UPDATE_ENDPOINT, config.KG_USERNAME, config.KG_PASSWORD
        )
    
    sparql_client.performUpdate(clearAll)

    filepath = os.getcwd() + '/test/resources/' #'/Agents/DoEAgent/summit_agent/resources/'
    for f in ['doe.ttl', 'Service__DoE.ttl', 'rxn_data.ttl']:
        with open(filepath+f, 'r') as file:
            data = file.read()
            sparql_client.performUpdate(data)

    # Hardcode the IRI to be used for the example
    # Developers should upload the files containing these triples to the endpoints following the instructions in the README.md
    derived = ['https://theworldavatar.com/kb/ontodoe/DoE_1/NewExperiment_1']
    agentIRI = config.ONTOAGENT_SERVICE
    inputs = ['https://theworldavatar.com/kb/ontodoe/DoE_1/Strategy_1', 
    'https://theworldavatar.com/kb/ontodoe/DoE_1/Domain_1', 
    'https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_1', 
    'https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_2', 
    'https://theworldavatar.com/kb/ontodoe/DoE_1/HistoricalData_1']

    # Create derivation instance given above information, the timestamp of this derivation is 0
    derivationIRI = derivationClient.createAsynDerivation(derived, agentIRI, inputs)
    # logger.info(f'Initialised successfully, created derivation instance <{derivationIRI}>')
    msg = f'Initialised successfully, created derivation instance: {derivationIRI}'

    # Iterate over the list of inputs to add and update the timestamp
    for input in inputs:
        derivationClient.addTimeInstance(input)
        # Update timestamp is needed as the timestamp added using addTimeInstance() is 0
        derivationClient.updateTimestamp(input)

    # Update the derivation asynchronous, it will only mark as "Requested"
    # The actual update will be handled by monitorDerivation method periodically run by DoE agent
    derivationClient.updateDerivationAsyn(derivationIRI)
    return msg

flask_app = Flask(__name__)

doe_agent_config = DoEAgentConfig(str(Path(__file__).absolute().parent) + '/conf/doeagent_properties.json')

app = DoEAgent(flask_app, doe_agent_config.ONTOAGENT_SERVICE, doe_agent_config.PERIODIC_TIMESCALE, doe_agent_config.DERIVATION_INSTANCE_BASE_URL, doe_agent_config.SPARQL_QUERY_ENDPOINT)
app.add_url_pattern('/', 'root', default, methods=['GET'])
app.add_url_pattern('/example', 'example', exampleEntryPoint, methods=['GET'])

if __name__ == '__main__':
    app.run()
