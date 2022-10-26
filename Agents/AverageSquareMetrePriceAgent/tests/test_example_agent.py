from pathlib import Path
from rdflib import Graph
from rdflib import RDF
import pytest
import time

import avgsqmpriceagent.datamodel as dm

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


def test_example_data_instantiation(initialise_clients):
    """This test checks that all example data gets correctly instantiated,
       including associated time series data in PostgreSQL.
    """
    # Get SPARQL client from fixture
    sparql_client, _, rdb_conn, rdb_url = initialise_clients

    ### TRIPPLE STORE ###
    # Verify that KG is empty
    assert sparql_client.getAmountOfTriples() == 0

    # Upload example test triples (ABox & TBox)
    cf.initialise_triples(sparql_client)

    # Verify instantiation of expected number of triples
    assert sparql_client.getAmountOfTriples() == (cf.TBOX_TRIPLES + cf.ABOX_TRIPLES)

    ### POSTGRESQL ###
    # Verify that Postgres database is empty
    assert cf.get_number_of_rdb_tables(rdb_conn) == 0

    # Initialise and Upload time series
    cf.initialise_timeseries(kgclient=sparql_client, rdb_url=rdb_url, 
                             rdb_user=cf.DB_USER, rdb_password=cf.DB_PASSWORD,
                             dataIRI=cf.PRICE_INDEX_INSTANCE_IRI,
                             dates=cf.DATES, values=cf.VALUES)

    # Verify that expected tables and triples are created (i.e. dbTable + 1 ts table)
    assert cf.get_number_of_rdb_tables(rdb_conn) == 2
    assert sparql_client.getAmountOfTriples() == (cf.TBOX_TRIPLES + cf.ABOX_TRIPLES + cf.TS_TRIPLES)

    # Verify correct retrieval of time series data
    dates, values = cf.retrieve_timeseries(kgclient=sparql_client, rdb_url=rdb_url, 
                             rdb_user=cf.DB_USER, rdb_password=cf.DB_PASSWORD,
                             dataIRI=cf.PRICE_INDEX_INSTANCE_IRI)
    assert dates == cf.DATES
    # Account for rounding errors
    assert pytest.approx(values, rel=1e-5) == cf.VALUES

    # Verify that dropping all tables works as expected
    cf.initialise_database(rdb_conn)
    assert cf.get_number_of_rdb_tables(rdb_conn) == 0



@pytest.mark.parametrize(
    "derivation_input_set, expected_postcode, expected_avg",
    [
        (cf.DERIVATION_INPUTS_1, cf.POSTCODE_1, cf.AVGPRICE_1),
        (cf.DERIVATION_INPUTS_2, cf.POSTCODE_2, cf.AVGPRICE_2), 
    ],
)
def test_monitor_derivations(
    initialise_clients, create_example_agent, derivation_input_set, expected_postcode, expected_avg
):

    sparql_client, derivation_client, rdb_conn, rdb_url = initialise_clients

    # Initialise all triples in test_triples + initialise time series in RDB
    # It first DELETES ALL DATA in the specified SPARQL/RDB endpoint
    # It then SPARQL updates all triples stated in test_triples folder to SPARQL endpoint +
    # Initialises PropertyPriceIndex time series and uploads test data to RDB
    cf.initialise_triples(sparql_client)
    cf.initialise_database(rdb_conn)
    cf.initialise_timeseries(kgclient=sparql_client, rdb_url=rdb_url, 
                             rdb_user=cf.DB_USER, rdb_password=cf.DB_PASSWORD,
                             dataIRI=cf.PRICE_INDEX_INSTANCE_IRI,
                             dates=cf.DATES, values=cf.VALUES)
    # Add time stamp to pure inputs
    cf.initialise_timestamps(derivation_client, derivation_input_set)
    # Verify correct number time mark up triples
    assert sparql_client.getAmountOfTriples() == (cf.TBOX_TRIPLES + cf.ABOX_TRIPLES + cf.TS_TRIPLES \
                         + cf.TIME_TRIPLES_PER_PURE_INPUT * len(derivation_input_set))

    # Create agent instance and register agent in KG
    # NOTE Successful agent registration within the KG is required to properly pick up derivations
    # -> Here we always set `register_agent=True` to guarantee that Blazegraph will be ready when
    # the agent is initialised. In a real deployment, the agent MUST be registered in the KG when 
    # spinning up the agent container, i.e. REGISTER_AGENT=true in env file
    agent = create_example_agent(register_agent=True, random_agent_iri=False)


    # Start the scheduler to monitor derivations (as this is a local agent test)
    agent._start_monitoring_derivations()

    # Assert that there's currently no instances has rdf:type of the output signature in the KG
    assert not sparql_client.check_if_triple_exist(None, RDF.type.toPython(), dm.OBE_AVERAGE_SM_PRICE)

    # Create derivation instance for new information, the timestamp of this derivation is 0
    derivation_iri = derivation_client.createAsyncDerivationForNewInfo(agent.agentIRI, derivation_input_set)
    print(f"Initialised successfully, created asynchronous derivation instance: {derivation_iri}")

    # Query timestamp of the derivation for every 20 seconds until it's updated
    currentTimestamp_derivation = 0
    while currentTimestamp_derivation == 0:
        time.sleep(10)
        currentTimestamp_derivation = cf.get_timestamp(derivation_iri, sparql_client)

    # Query the output of the derivation instance
    derivation_outputs = cf.get_derivation_outputs(derivation_iri, sparql_client)
    print(f"Generated derivation outputs that belongsTo the derivation instance: {', '.join(derivation_outputs)}")
    
    # Verify that there are 2 derivation outputs (i.e. AveragePrice and Measure IRIs)
    assert len(derivation_outputs) == 2
    assert dm.OBE_AVERAGE_SM_PRICE in derivation_outputs
    assert len(derivation_outputs[dm.OBE_AVERAGE_SM_PRICE]) == 1
    assert dm.OM_MEASURE in derivation_outputs
    assert len(derivation_outputs[dm.OM_MEASURE]) == 1
    
    # Verify the values of the derivation output
    avg_iri = derivation_outputs[dm.OBE_AVERAGE_SM_PRICE][0]
    inputs, postcode, price = cf.get_avgsqmprice_details(sparql_client, avg_iri)
    # Verify postcode
    assert len(postcode) == 1
    assert postcode[0] == expected_postcode
    # Verify price
    assert len(price) == 1
    assert price[0] == expected_avg
    # Verify inputs (i.e. derived from)
    for i in inputs:
        for j in inputs[i]:
            assert j in derivation_input_set
            derivation_input_set.remove(j)
    assert len(derivation_input_set) == 0

    print("All check passed.")

    # Shutdown the scheduler to clean up
    agent.scheduler.shutdown()
