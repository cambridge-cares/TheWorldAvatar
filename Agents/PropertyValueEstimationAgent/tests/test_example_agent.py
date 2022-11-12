from pathlib import Path
from rdflib import Graph
from rdflib import RDF
import pytest
import time

import derivationagentpythonexample.data_model as dm

from . import conftest as cf

def test_example_triples():
    """This test checks that the example triples are correct in syntax.

    Raises:
        e: If the example triples are not valid RDF.
    """
    g = Graph()
    pathlist = Path(cf.TEST_TRIPLES_DIR).glob('*.ttl')
    for path in pathlist:
        try:
            g.parse(str(path))
        except Exception as e:
            raise e


@pytest.mark.parametrize(
    "local_agent_test",
    [
        (True), # local agent instance test
        (False), # deployed docker agent test
    ],
)
def test_monitor_derivations(
    initialise_clients, create_example_agent, local_agent_test
):
    sparql_client, derivation_client = initialise_clients

    # Initialise all triples in test_triples
    # It first DELETES ALL TRIPLES in the specified SPARQL endpoint
    # It then SPARQL update all triples stated in test_triples folder to the same endpoint 
    cf.initialise_triples(sparql_client)

    # Create agent instance, register agent in KG
    # NOTE Here we always set register_agent=True even for dockerised agent test
    # Reason for this design is that agent and blazegraph are in the same docker-compose.yml
    # However, there is no guarantee that the blazegraph will be ready when the agent is initialised within the docker container
    # Therefore, we register the agent in the KG from the host machine to ensure that the agent in docker is initialised successfully
    # In a real deployment, the agent MUST be registered in the KG when spinning up the agent container, i.e. REGISTER_AGENT=true in env file
    agent = create_example_agent(register_agent=True, random_agent_iri=local_agent_test)

    # Start the scheduler to monitor derivations if it's local agent test
    if local_agent_test:
        agent._start_monitoring_derivations()

    # Assert that there's currently no instances has rdf:type of the output signature in the KG
    assert not sparql_client.check_if_triple_exist(None, RDF.type.toPython(), dm.DERIVATION_AGENT_PYTHON_EXAMPLE_DIFFERENCE)

    # Create derivation instance for new information, the timestamp of this derivation is 0
    derivation_iri = derivation_client.createAsyncDerivationForNewInfo(agent.agentIRI, cf.DERIVATION_INPUTS)
    print(f"Initialised successfully, created asynchronous derivation instance: {derivation_iri}")

    # Query timestamp of the derivation for every 20 seconds until it's updated
    currentTimestamp_derivation = 0
    while currentTimestamp_derivation == 0:
        time.sleep(20)
        currentTimestamp_derivation = cf.get_timestamp(derivation_iri, sparql_client)

    # Query the input of the derivation instance
    min_value = sparql_client.get_min_value(cf.MINVALUE_INSTANCE_IRI)
    max_value = sparql_client.get_max_value(cf.MAXVALUE_INSTANCE_IRI)

    # Query the output of the derivation instance
    derivation_outputs = cf.get_derivation_outputs(derivation_iri, sparql_client)
    print(f"Generated derivation outputs that belongsTo the derivation instance: {derivation_outputs}")
    difference = sparql_client.get_difference(derivation_outputs[dm.DERIVATION_AGENT_PYTHON_EXAMPLE_DIFFERENCE][0])
    assert len(derivation_outputs) == 2
    assert dm.DERIVATION_AGENT_PYTHON_EXAMPLE_DIFFERENCE in derivation_outputs
    assert len(derivation_outputs[dm.DERIVATION_AGENT_PYTHON_EXAMPLE_DIFFERENCE]) == 1
    assert derivation_outputs[dm.DERIVATION_AGENT_PYTHON_EXAMPLE_DIFFERENCE][0] == difference.instance_iri
    assert dm.DERIVATION_AGENT_PYTHON_EXAMPLE_VALUE in derivation_outputs
    assert len(derivation_outputs[dm.DERIVATION_AGENT_PYTHON_EXAMPLE_VALUE]) == 1
    assert derivation_outputs[dm.DERIVATION_AGENT_PYTHON_EXAMPLE_VALUE][0] == difference.hasValue.instance_iri
    assert difference.hasValue.numVal == max_value.hasValue.numVal - min_value.hasValue.numVal
    print("All check passed.")

    # Shutdown the scheduler to clean up if it's local agent test (as the doe_agent scheduler must have started)
    if local_agent_test:
        agent.scheduler.shutdown()
