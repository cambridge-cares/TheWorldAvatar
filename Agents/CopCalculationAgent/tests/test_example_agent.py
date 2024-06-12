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


# NOTE It's possible to test multiple sets of derivation inputs with the same test function
#   by using pytest.mark.parametrize, see https://docs.pytest.org/en/latest/how-to/parametrize.html
# Or for a concrete example, see:
# https://github.com/cambridge-cares/TheWorldAvatar/blob/c9d822e76459cd7597e7e70e2f959afdf81580ca/Agents/PropertyValueEstimationAgent/tests/test_example_agent.py#L84
def test_monitor_derivations(
    initialise_clients, create_example_agent
):
    sparql_client, derivation_client = initialise_clients

    # Determine whether this is a local test or not
    # NOTE Depends on where this test is running, the blazegraph URL will be different
    # if DOCKERISED_TEST: then the blazegraph URL is the one within the docker stack, i.e. "host.docker.internal"
    # if not DOCKERISED_TEST: then the blazegraph URL is the one outside the docker stack, i.e. "localhost"
    local_agent_test = not cf.DOCKERISED_TEST

    # Initialise all triples in test_triples
    # It first DELETES ALL TRIPLES in the specified SPARQL endpoint
    # It then SPARQL update all triples stated in test_triples folder to the same endpoint 
    cf.initialise_triples(sparql_client)

    # Create agent instance, register agent in KG
    # NOTE Here we always set register_agent=True even for dockerised agent test
    # Reason for this design is that successful agent registration within the KG is required to pick up markup up derivations
    # However, agent and blazegraph are in the same docker-compose.yml, meaning there is
    #   no guarantee that the blazegraph will be ready when the agent is initialised within the docker container
    # Hence, the Dockerised agent is started without initial registration and
    #   registration is done within the test (from the host machine) to guarantee that test Blazegraph will be ready
    # The "belated" registration of the Dockerised agent can be achieved by registering "another"
    #   agent instance with the same ONTOAGENT_SERVICE_IRI
    # In a real deployment, the agent MUST be registered in the KG when spinning up the agent container, i.e. REGISTER_AGENT=true in env file
    agent = create_example_agent(register_agent=True, alter_agent_iri=local_agent_test)

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
