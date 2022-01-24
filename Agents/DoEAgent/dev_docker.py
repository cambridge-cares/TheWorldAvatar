from src.agent import *

###############################################################
### !!! Do NOT run this script before reading README.md !!! ###
###############################################################

def exampleEntryPoint():
    """
        !!! Do NOT run this script before reading README.md !!!
        As the monitorDerivations() is set to be running periodically once the DoE agent is deployed,
        this function serve as an example that creats a working case once the developer has confirmed
        there are no valuable data in the knowledge graph endpoints specified in the conf,
        i.e. this script deletes ALL existing triples and upload example triples to the endpoint, it
        then creates the derivation instance based on the example data and execute asynchronous
        derivation update automatically.
        Response:
            the created OntoDerivation:Derivation instance
    """

    config = DoEAgentConfig(str(Path(__file__).absolute().parent) + '/src/conf/doeagent_properties.json')

    clearAll = """DELETE {?s ?p ?o} \
            WHERE {?s ?p ?o}
            """
    example_sparql_client = DoESparqlClient(config.SPARQL_QUERY_ENDPOINT, config.SPARQL_UPDATE_ENDPOINT)
    example_sparql_client.performUpdate(clearAll)

    folderpath = str(Path(__file__).absolute().parent) + '/src/test/resources/'
    example_sparql_client.uploadOntology(folderpath+'doe.ttl')
    example_sparql_client.uploadOntology(folderpath+'Service__DoE.ttl')
    example_sparql_client.uploadOntology(folderpath+'rxn_data.ttl')

    # Hardcode the IRI to be used for the example
    # Developers should upload the files containing these triples to the endpoints following the instructions in the README.md
    derivation_output = ['https://www.example.com/triplestore/ontodoe/DoE_1/NewExperiment_1']
    derivation_inputs = ['https://www.example.com/triplestore/ontodoe/DoE_1/Strategy_1',
                        'https://www.example.com/triplestore/ontodoe/DoE_1/Domain_1',
                        'https://www.example.com/triplestore/ontodoe/DoE_1/SystemResponse_1',
                        'https://www.example.com/triplestore/ontodoe/DoE_1/SystemResponse_2',
                        'https://www.example.com/triplestore/ontodoe/DoE_1/HistoricalData_1']
    design_of_experiment_iri = 'https://www.example.com/triplestore/ontodoe/DoE_1/DoE_1'

    agent_app = DoEAgent(config.ONTOAGENT_SERVICE, config.PERIODIC_TIMESCALE, config.DERIVATION_INSTANCE_BASE_URL, config.SPARQL_QUERY_ENDPOINT, logger_name='prod')

    # Create derivation instance given above information, the timestamp of this derivation is 0
    derivationIRI = agent_app.derivationClient.createAsynDerivation(derivation_output, agent_app.agentIRI, derivation_inputs)
    # logger.info(f'Initialised successfully, created derivation instance <{derivationIRI}>')
    msg = f'Initialised successfully, created derivation instance: {derivationIRI}'

    # Iterate over the list of inputs to add and update the timestamp
    for input in derivation_inputs:
        agent_app.derivationClient.addTimeInstance(input)
        # Update timestamp is needed as the timestamp added using addTimeInstance() is 0
        agent_app.derivationClient.updateTimestamp(input)

    # Update the derivation asynchronous, it will only mark as "Requested"
    # The actual update will be handled by monitorDerivation method periodically run by DoE agent
    agent_app.derivationClient.updateDerivationAsyn(derivationIRI)
    return msg

if __name__ == '__main__':
    !!! Do NOT run this script before reading README.md !!! # comment out this line before running this script (make sure you have read README.md)
    msg = exampleEntryPoint()
    print(msg)
