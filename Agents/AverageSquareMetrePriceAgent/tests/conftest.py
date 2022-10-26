# This module provides all pytest fixtures and other utility functions for the
# actual integration tests

# Avoid unnecessary logging information from py4j package
import logging
logging.getLogger("py4j").setLevel(logging.INFO)

from pathlib import Path
from rdflib import Graph
from flask import Flask
import psycopg2 as pg
import pandas as pd
import requests
import pytest
import time
import uuid
import os

# Import mocked modules for all stack interactions (see `tests\__init__.py` for details)
from tests.mockutils.stack_configs_mock import QUERY_ENDPOINT, UPDATE_ENDPOINT, THRESHOLD, \
                                               DATABASE, DB_URL, DB_USER, DB_PASSWORD

from pyderivationagent.data_model.iris import ONTODERIVATION_BELONGSTO, TIME_HASTIME, \
                                              TIME_INTIMEPOSITION, TIME_NUMERICPOSITION
from pyderivationagent.conf import config_derivation_agent
from avgsqmpriceagent.kg_operations.kgclient import KGClient
from avgsqmpriceagent.kg_operations.tsclient import TSClient
from avgsqmpriceagent.datamodel.data import DATACLASS, TIME_FORMAT_SHORT
from avgsqmpriceagent.agent import AvgSqmPriceAgent


# ----------------------------------------------------------------------------------
# Constants and configuration (to be adjusted as needed)
# ----------------------------------------------------------------------------------

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
TEST_TRIPLES_DIR = os.path.join(THIS_DIR, 'test_triples')

# needs to match docker compose file
KG_SERVICE = "blazegraph_agent_test"
KG_ROUTE = "blazegraph/namespace/kb/sparql"
RDB_SERVICE = "postgres_agent_test"
RDB_ROUTE = DATABASE  # default DB (created upon creation)


# Configuration .env file
AGENT_ENV = os.path.join(THIS_DIR,'agent_test.env')
# NOTE As the agent is designed to be deployed to the stack, the triple store URLs provided
# in the agent_test.env file are just placeholders to be overwritten by a `stack_configs.py`
# mock. However, some entry is required in the .env file to avoid Exceptions from the 
# AgentConfig class (the same applies to other keywords left blank)


DERIVATION_INSTANCE_BASE_URL = config_derivation_agent(AGENT_ENV).DERIVATION_INSTANCE_BASE_URL

# IRIs of derivation's (pure) inputs
# NOTE Should be consistent with the ones in test_triples/example_abox.ttl
TEST_TRIPLES_BASE_IRI = 'https://www.example.com/kg/ontobuiltenv/'
POSTCODE_INSTANCE_IRI = TEST_TRIPLES_BASE_IRI + 'PostalCode_1'
PRICE_INDEX_INSTANCE_IRI = TEST_TRIPLES_BASE_IRI + 'PropertyPriceIndex_1'
TRANSACTION_INSTANCE_1_IRI = DERIVATION_INSTANCE_BASE_URL + 'TransactionRecord_1'
TRANSACTION_INSTANCE_2_IRI = DERIVATION_INSTANCE_BASE_URL + 'TransactionRecord_2'
TRANSACTION_INSTANCE_3_IRI = DERIVATION_INSTANCE_BASE_URL + 'TransactionRecord_3'
TRANSACTION_INSTANCE_4_IRI = DERIVATION_INSTANCE_BASE_URL + 'TransactionRecord_4'
DERIVATION_INPUTS = [POSTCODE_INSTANCE_IRI, PRICE_INDEX_INSTANCE_IRI,
                     TRANSACTION_INSTANCE_1_IRI, TRANSACTION_INSTANCE_2_IRI, 
                     TRANSACTION_INSTANCE_3_IRI, TRANSACTION_INSTANCE_4_IRI]


# ----------------------------------------------------------------------------------
#Test data
dates = pd.date_range(start='1990-01-01', freq='M', end='2022-10-01')
DATES = dates.strftime('%Y-%m').tolist()
DATES = [d+'-01' for d in DATES]
VALUES = [i*(100/len(dates)) for i in range(1, len(dates)+1)]


# ----------------------------------------------------------------------------------
# Session-scoped test fixtures
# (i.e. the fixture is destroyed at the end of the test session)
# ----------------------------------------------------------------------------------

@pytest.fixture(scope="session")
def get_blazegraph_service_url(session_scoped_container_getter):
    def _get_service_url(service_name, url_route):
        service = session_scoped_container_getter.get(service_name).network_info[0]
        service_url = f"http://localhost:{service.host_port}/{url_route}"

        # This will run only once per entire test session
        # It ensures that the requested Blazegraph Docker service is ready to accept SPARQL query/update
        service_available = False
        while not service_available:
            try:
                response = requests.head(service_url)
                if response.status_code != requests.status_codes.codes.not_found:
                    service_available = True
            except requests.exceptions.ConnectionError:
                time.sleep(3)

        return service_url
    return _get_service_url


@pytest.fixture(scope="session")
def get_postgres_service_url(session_scoped_container_getter):
    def _get_service_url(service_name, url_route):
        service = session_scoped_container_getter.get(service_name).network_info[0]
        service_url = f"jdbc:postgresql://localhost:{service.host_port}/{url_route}"

        # This will run only once per entire test session
        # It ensures that the requested PostgreSQL Docker service is ready to accept queries
        service_available = False
        while not service_available:
            try:
                conn = pg.connect(host='localhost', port=service.host_port,
                                  user=DB_USER, password=DB_PASSWORD,
                                  database=DATABASE)
                if conn.status == pg.extensions.STATUS_READY:
                    service_available = True
            except Exception:
                time.sleep(3)

        return conn, service_url
    return _get_service_url


@pytest.fixture(scope="session")
def initialise_clients(get_blazegraph_service_url, get_postgres_service_url):
    # Retrieve endpoint for triple store
    sparql_endpoint = get_blazegraph_service_url(KG_SERVICE, url_route=KG_ROUTE)

    # Retrieve endpoint for postgres
    rdb_conn, rdb_url = get_postgres_service_url(RDB_SERVICE, url_route=RDB_ROUTE)

    # Create SparqlClient for testing
    sparql_client = KGClient(sparql_endpoint, sparql_endpoint)

    # Create DerivationClient for creating derivation instances
    derivation_client = sparql_client.jpsBaseLib_view.DerivationClient(
        sparql_client.kg_client,
        DERIVATION_INSTANCE_BASE_URL
    )

    yield sparql_client, derivation_client, rdb_conn, rdb_url

    # Clear logger at the end of the test
    clear_loggers()


# ----------------------------------------------------------------------------------
# Module-scoped test fixtures
# (i.e. the fixture is destroyed during teardown of the last test in the module)
# ----------------------------------------------------------------------------------

@pytest.fixture(scope="module")
def create_example_agent():
    def _create_example_agent(
        register_agent:bool=False,
        random_agent_iri:bool=False,
    ):
        agent_config = config_derivation_agent(AGENT_ENV)
        agent = AvgSqmPriceAgent(
            register_agent=agent_config.REGISTER_AGENT if not register_agent else register_agent,
            agent_iri=agent_config.ONTOAGENT_SERVICE_IRI if not random_agent_iri else 'http://agent_' + str(uuid.uuid4()),
            time_interval=agent_config.DERIVATION_PERIODIC_TIMESCALE,
            derivation_instance_base_url=agent_config.DERIVATION_INSTANCE_BASE_URL,
            kg_url=QUERY_ENDPOINT,
            kg_update_url=UPDATE_ENDPOINT,
            # NOTE For agent endpoint, we keep this as it is for now (i.e. start with http://host.docker.internal)
            # As the agent endpoint is not accessed from outside the docker network
            agent_endpoint=agent_config.ONTOAGENT_OPERATION_HTTP_URL,
            max_thread_monitor_async_derivations=agent_config.MAX_THREAD_MONITOR_ASYNC_DERIVATIONS,
            threshold=THRESHOLD,
            app=Flask(__name__),
            logger_name='dev'
        )
        return agent
    return _create_example_agent


# ----------------------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------------------

def initialise_triples(sparql_client):
    # Delete all triples before initialising prepared triples
    sparql_client.performUpdate("""DELETE WHERE {?s ?p ?o.}""")

	# Upload all relevant example triples provided in the test_triples folder
    pathlist = Path(TEST_TRIPLES_DIR).glob('*.ttl')
    for path in pathlist:
        g = Graph()
        g.parse(str(path), format='turtle')
        sparql_client.uploadGraph(g)


def initialise_database(rdb_conn):
    # Deletes all tables in the database (before initialising prepared tables)
    with rdb_conn:
        cur=rdb_conn.cursor()
        sql_query = """
            DROP SCHEMA public CASCADE;
            CREATE SCHEMA public;
        """
        cur.execute(sql_query)


def get_number_of_rdb_tables(rdb_conn):
    # Returns total number of tables in given database
    with rdb_conn:
        cur=rdb_conn.cursor()
        sql_query = """
            SELECT table_name FROM information_schema.tables
            WHERE table_schema = 'public'
        """
        cur.execute(sql_query)
        rows = cur.fetchall()
        return len(rows)


def initialise_timeseries(kgclient, dataIRI, dates, values, rdb_url, 
                          rdb_user, rdb_password):
    # Initialise timeseries (in Blazegraph and PostgreSQL) with given dates and values
    # Initialise time series client
    ts_client = TSClient(kg_client=kgclient, rdb_url=rdb_url, rdb_user=rdb_user, 
                         rdb_password=rdb_password)
    # Create time series from test data                        
    ts = TSClient.create_timeseries(dates, [dataIRI], [values])
    
    # Initialise time series in Blazegraph and PostgreSQL
    ts_client.tsclient.initTimeSeries([dataIRI], [DATACLASS], TIME_FORMAT_SHORT,
                                      ts_client.conn)
    # Add test time series data
    ts_client.tsclient.addTimeSeriesData(ts, ts_client.conn)


def retrieve_timeseries(kgclient, dataIRI, rdb_url, rdb_user, rdb_password):
    # Initialise time series client
    ts_client = TSClient(kg_client=kgclient, rdb_url=rdb_url, rdb_user=rdb_user, 
                         rdb_password=rdb_password)
    ts = ts_client.tsclient.getTimeSeries([dataIRI], ts_client.conn)
    dates = ts.getTimes()
    dates = [d.toString() for d in dates]
    values = ts.getValues(dataIRI)
    return dates, values


def initialise_timestamps(derivation_client):
    # Add timestamp to pure inputs
    for input in DERIVATION_INPUTS:
        derivation_client.addTimeInstance(input)
        derivation_client.updateTimestamp(input)


def get_endpoint(docker_container):
    # Retrieve SPARQL endpoint for temporary testcontainer
    # endpoint acts as both Query and Update endpoint
    endpoint = 'http://' + docker_container.get_container_host_ip().replace('localnpipe', 'localhost') + ':' \
               + docker_container.get_exposed_port(9999)
    # 'kb' is default namespace in Blazegraph
    endpoint += '/blazegraph/namespace/kb/sparql'
    return endpoint


# method adopted from https://github.com/pytest-dev/pytest/issues/5502#issuecomment-647157873
def clear_loggers():
    """Remove handlers from all loggers"""
    import logging
    loggers = [logging.getLogger()] + list(logging.Logger.manager.loggerDict.values())
    for logger in loggers:
        handlers = getattr(logger, 'handlers', [])
        for handler in handlers:
            logger.removeHandler(handler)


def get_timestamp(derivation_iri: str, sparql_client):
    query_timestamp = f"""
        SELECT ?time WHERE {{
            <{derivation_iri}> <{TIME_HASTIME}>/<{TIME_INTIMEPOSITION}>/<{TIME_NUMERICPOSITION}> ?time .
        }}"""
    # the queried results must be converted to int, otherwise it will not be comparable
    return int(sparql_client.performQuery(query_timestamp)[0]['time'])


def get_derivation_outputs(derivation_iri: str, sparql_client):
    query_output = f"""SELECT ?output ?output_type
        WHERE {{
            ?output <{ONTODERIVATION_BELONGSTO}> <{derivation_iri}> .
            ?output a ?output_type .
        }}"""
    response = sparql_client.performQuery(query_output)
    if len(response) == 0:
        return None
    else:
        key = set([x['output_type'] for x in response])
        return {k: [x['output'] for x in response if x['output_type'] == k] for k in key}
