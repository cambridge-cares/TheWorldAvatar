import uuid

from twa.data_model import iris

pytest_plugins = ["docker_compose"]

def test_dropTimestampsOf(initialise_clients):
    sparql_client, derivation_client = initialise_clients

    # Create some IRIs
    entity_1 = "http://" + str(uuid.uuid4())
    agent = "http://" + str(uuid.uuid4())
    input_1 = "http://" + str(uuid.uuid4())
    input_2 = "http://" + str(uuid.uuid4())

    # Create a new derivation
    derivation = derivation_client.createDerivation([entity_1], agent, [input_1, input_2])

    # Check that the timestamps are not empty
    for iri in [derivation, input_1, input_2]:
        assert sparql_client.perform_query(f"ASK {{ <{iri}> <{iris.TIME_HASTIME}>/<{iris.TIME_INTIMEPOSITION}>/<{iris.TIME_NUMERICPOSITION}> ?ts }}")[0]['ASK']

    # Drop the timestamps
    derivation_client.dropTimestampsOf([input_1, input_2])
    assert sparql_client.perform_query(f"ASK {{ <{input_1}> <{iris.TIME_HASTIME}>/<{iris.TIME_INTIMEPOSITION}>/<{iris.TIME_NUMERICPOSITION}> ?ts }}")[0]['ASK'] == False
    assert sparql_client.perform_query(f"ASK {{ <{input_2}> <{iris.TIME_HASTIME}>/<{iris.TIME_INTIMEPOSITION}>/<{iris.TIME_NUMERICPOSITION}> ?ts }}")[0]['ASK'] == False
    assert sparql_client.perform_query(f"ASK {{ <{derivation}> <{iris.TIME_HASTIME}>/<{iris.TIME_INTIMEPOSITION}>/<{iris.TIME_NUMERICPOSITION}> ?ts }}")[0]['ASK']
