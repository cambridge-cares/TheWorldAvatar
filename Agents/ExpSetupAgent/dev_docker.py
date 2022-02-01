from expsetupagent.agent import *

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

    config = ExpAgentConfig(str(Path(__file__).absolute().parent) + '/expsetupagent/conf/agent_properties.json')

    clearAll = """DELETE {?s ?p ?o} \
            WHERE {?s ?p ?o}
            """
    example_sparql_client = ExpSetupSparqlClient(config.SPARQL_QUERY_ENDPOINT, config.SPARQL_UPDATE_ENDPOINT)
    example_sparql_client.performUpdate(clearAll)

    folderpath = str(Path(__file__).absolute().parent) + '/expsetupagent/tests/resources/'
    example_sparql_client.uploadOntology(folderpath+'new_exp_data.xml')
    example_sparql_client.uploadOntology(folderpath+'Service__ExpSetup.ttl')
    example_sparql_client.uploadOntology(folderpath+'placeholder_settings.ttl')
    example_sparql_client.uploadOntology(folderpath+'duplicate_ontorxn.ttl')

    # Hardcode the IRI to be used for the example
    # Developers should upload the files containing these triples to the endpoints following the instructions in the README.md
    derivation_output = ['https://www.example.com/triplestore/ontolab/ExpSetup_1/EquipmentSettings_1']
    derivation_inputs = ['https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/ReactionVariation_342e55c0-23f7-4a08-8443-e958e3b0e2c9']
    # 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/ReactionVariation_d46acf42-ec48-454b-b138-1f548ce1f4ad'
    # 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/ReactionVariation_d2f7b1f4-76e2-4401-bb68-29ade1a792ec'

    agent_app = ExpSetupAgent(config.ONTOAGENT_SERVICE, config.PERIODIC_TIMESCALE, config.DERIVATION_INSTANCE_BASE_URL, config.SPARQL_QUERY_ENDPOINT, logger_name='prod')

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

    agent_app.start_monitoring_derivations()
    agent_app.run_flask_app()
    return msg

if __name__ == '__main__':
    !!! Do NOT run this script before reading README.md !!! # comment out this line before running this script (make sure you have read README.md)
    msg = exampleEntryPoint()
    # print(msg)