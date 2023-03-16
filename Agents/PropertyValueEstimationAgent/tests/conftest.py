# This module provides all pytest fixtures and other utility functions for the
# actual integration tests

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
from tests.mockutils.stack_configs_mock import QUERY_ENDPOINT, UPDATE_ENDPOINT, \
                                               DATABASE, DB_USER, DB_PASSWORD
from tests.mockutils.env_configs_mock import HOSTNAME

from pyderivationagent.data_model.iris import ONTODERIVATION_BELONGSTO, ONTODERIVATION_ISDERIVEDFROM, \
                                              TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION, \
                                              ONTODERIVATION_DERIVATIONASYN, ONTODERIVATION_HASSTATUS, \
                                              ONTODERIVATION_ERROR
from pyderivationagent.conf import config_derivation_agent
from pyderivationagent import PyDerivationClient

from propertyvalueestimation.datamodel.iris import *
from propertyvalueestimation.datamodel.data import DATACLASS, TIME_FORMAT_SHORT
from propertyvalueestimation.kg_operations.kgclient import KGClient
from propertyvalueestimation.kg_operations.tsclient import TSClient
from propertyvalueestimation.agent import PropertyValueEstimationAgent


# ----------------------------------------------------------------------------------
# Constants and configuration
# ----------------------------------------------------------------------------------
# Values to be adjusted as needed:
#

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
TEST_TRIPLES_DIR = os.path.join(THIS_DIR, 'test_triples')
STACK_CONFIG_FILE = os.path.join(THIS_DIR, 'mockutils/stack_configs_mock.py')

# Agent configuration .env file
AGENT_ENV = os.path.join(THIS_DIR,'agent_test.env')
# NOTE As the agent is designed to be deployed to the stack, the triple store URLs provided in the `agent_test.env` 
# file are just placeholders to be overwritten by the `stack_configs.py` mock. However, some entry is required in 
# the .env file to avoid Exceptions from the AgentConfig class (the same applies to other keywords left blank)
#
# To ensure proper mocking of the `stack_configs.py` module, please provide the 
# DATABASE, DB_USER, DB_PASSWORD environment variables in the respective files:
#   tests\mockutils\env_configs_mock.py
#   tests\mockutils\stack_configs_mock.py
# Correct endpoints for DB_URL, QUERY_ENDPOINT, UPDATE_ENDPOINT will be retrieved 
# automatically from the respective Docker services

# Provide names of respective Docker services
# NOTE These names need to match the ones given in the `docker-compose-test.yml` file
KG_SERVICE = "blazegraph_agent_test"
KG_ROUTE = "blazegraph/namespace/kb/sparql"
RDB_SERVICE = "postgres_agent_test"
RDB_ROUTE = DATABASE

# Derivation markup
DERIVATION_INSTANCE_BASE_URL = config_derivation_agent(AGENT_ENV).DERIVATION_INSTANCE_BASE_URL
# IRIs of derivation's (pure) inputs
# NOTE Should be consistent with the ones in test_triples/example_abox.ttl
TEST_TRIPLES_BASE_IRI = 'https://www.example.com/kg/ontobuiltenv/'

# PropertyPriceIndex
PRICE_INDEX_INSTANCE_IRI = TEST_TRIPLES_BASE_IRI + 'PropertyPriceIndex_1'
# AveragePricePerSqm
AVERAGE_SQM_PRICE = TEST_TRIPLES_BASE_IRI + 'AveragePricePerSqm_1'
# TransactionRecords
TRANSACTION_INSTANCE_1_IRI = TEST_TRIPLES_BASE_IRI + 'Transaction_1'
TRANSACTION_INSTANCE_2_IRI = TEST_TRIPLES_BASE_IRI + 'Transaction_2'
TRANSACTION_INSTANCE_3_IRI = TEST_TRIPLES_BASE_IRI + 'Transaction_3'
# FloorArea
FLOOR_AREA_INSTANCE_1_IRI = TEST_TRIPLES_BASE_IRI + 'Area_1'
FLOOR_AREA_INSTANCE_3_IRI = TEST_TRIPLES_BASE_IRI + 'Area_3'

# Define input sets to test
DERIVATION_INPUTS_1 = [PRICE_INDEX_INSTANCE_IRI, TRANSACTION_INSTANCE_1_IRI,
                       AVERAGE_SQM_PRICE, FLOOR_AREA_INSTANCE_1_IRI]
DERIVATION_INPUTS_2 = [PRICE_INDEX_INSTANCE_IRI, TRANSACTION_INSTANCE_1_IRI,
                       AVERAGE_SQM_PRICE]
DERIVATION_INPUTS_3 = [AVERAGE_SQM_PRICE, FLOOR_AREA_INSTANCE_3_IRI]
DERIVATION_INPUTS_4 = [TRANSACTION_INSTANCE_2_IRI, TRANSACTION_INSTANCE_3_IRI]
# Test against pre-calculated value estimates from Excel (rounded)
MARKET_VALUE_1 = 936000
MARKET_VALUE_2 = 938000
EXCEPTION_STATUS_1 = "More than one 'TransactionRecord' IRI provided"

# ----------------------------------------------------------------------------------
#  Inputs which should not be changed
#

# Property price index test data
dates = pd.date_range(start='1990-01-01', freq='M', end='2022-10-01')
VALUES = [i*(100/len(dates)) for i in range(1, len(dates)+1)]
VALUES = [i*(100/len(dates)) for i in range(1, len(dates)+1)]
DATES = dates.strftime('%Y-%m').tolist()
DATES = [d+'-01' for d in DATES]

# Expected number of triples
TBOX_TRIPLES = 25
ABOX_TRIPLES = 48
TS_TRIPLES = 4
TIME_TRIPLES_PER_PURE_INPUT = 6
DERIV_STATUS_TRIPLES = 2        # derivation status triples
AGENT_SERVICE_TRIPLES = 5       # agent service triples
DERIV_INPUT_TRIPLES = 2 + 4*3   # triples for derivation input message
DERIV_OUTPUT_TRIPLES = 5        # triples for derivation output message
MARKET_VALUE_COMPUTABLE = 6     # triples added by successful property value estimation
                                # i.e. kgclient.instantiate_property_value
MARKET_VALUE_NONCOMPUTABLE = 5  # triples added by unsuccessful property value estimation
                                # i.e. kgclient.instantiate_property_value


# ----------------------------------------------------------------------------------
# Session-scoped test fixtures
# (i.e. the fixture is destroyed at the end of the test session)
# ----------------------------------------------------------------------------------

@pytest.fixture(scope="session")
def get_blazegraph_service_url(session_scoped_container_getter):
    def _get_service_url(service_name, url_route, hostname=HOSTNAME):
        service = session_scoped_container_getter.get(service_name).network_info[0]
        service_url = f"http://{hostname}:{service.host_port}/{url_route}"
        print(f'KG endpoint: {service_url}')

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

        print('Connected to KG.')
        return service_url
    return _get_service_url


@pytest.fixture(scope="session")
def get_postgres_service_url(session_scoped_container_getter):
    def _get_service_url(service_name, url_route, hostname=HOSTNAME):
        service = session_scoped_container_getter.get(service_name).network_info[0]
        service_url = f"jdbc:postgresql://{hostname}:{service.host_port}/{url_route}"
        print(f'PostgreSQL endpoint: {service_url}')

        # This will run only once per entire test session
        # It ensures that the requested PostgreSQL Docker service is ready to accept queries
        service_available = False
        while not service_available:
            try:
                conn = pg.connect(host=hostname, port=service.host_port,
                                  user=DB_USER, password=DB_PASSWORD,
                                  database=DATABASE)
                if conn.status == pg.extensions.STATUS_READY:
                    service_available = True
            except Exception:
                time.sleep(3)

        print('Connected to PostgreSQL.')
        return service_url
    return _get_service_url


@pytest.fixture(scope="session")
def initialise_clients(get_blazegraph_service_url, get_postgres_service_url):
    # Retrieve "user-facing" endpoints for all clients/services relevant for testing
    # (i.e. invoked during testing from outside the Docker stack)
    # --> those shall be `localhost:...` even when agent is running as Docker container
    
    # Retrieve endpoint for triple store
    sparql_endpoint = get_blazegraph_service_url(KG_SERVICE, url_route=KG_ROUTE)

    # Retrieve endpoint for postgres
    rdb_url = get_postgres_service_url(RDB_SERVICE, url_route=RDB_ROUTE)

    # Create SparqlClient for testing
    sparql_client = KGClient(sparql_endpoint, sparql_endpoint)

    # Create DerivationClient for creating derivation instances
    derivation_client = PyDerivationClient(
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        query_endpoint=sparql_client.query_endpoint,
        update_endpoint=sparql_client.update_endpoint,
    )

    yield sparql_client, derivation_client, rdb_url

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
        alter_agent_iri:bool=False,
    ):
        agent_config = config_derivation_agent(AGENT_ENV)
        agent = PropertyValueEstimationAgent(
            register_agent=agent_config.REGISTER_AGENT if not register_agent else register_agent,
            agent_iri=agent_config.ONTOAGENT_SERVICE_IRI if not alter_agent_iri else \
                      agent_config.ONTOAGENT_SERVICE_IRI + '_altered',
            time_interval=agent_config.DERIVATION_PERIODIC_TIMESCALE,
            derivation_instance_base_url=agent_config.DERIVATION_INSTANCE_BASE_URL,
            # NOTE Ensure SPARQL endpoints contain'host.docker.internal' for dockerised agent tests
            # Set automatically for normal local as well as dockerised tests; however, requires
            # manual adjustment in `stack_configs_mock.py` file for debugging within Docker
            kg_url=QUERY_ENDPOINT,
            kg_update_url=UPDATE_ENDPOINT,
            agent_endpoint=agent_config.ONTOAGENT_OPERATION_HTTP_URL,
            max_thread_monitor_async_derivations=agent_config.MAX_THREAD_MONITOR_ASYNC_DERIVATIONS,
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


def initialise_database(rdb_url):
    # Deletes all tables in the database (before initialising prepared tables)
    with connect_to_rdb(rdb_url) as conn:
        cur=conn.cursor()
        sql_query = """
            DROP SCHEMA public CASCADE;
            CREATE SCHEMA public;
        """
        cur.execute(sql_query)


def get_number_of_rdb_tables(rdb_url):
    # Returns total number of tables in given database
    with connect_to_rdb(rdb_url) as conn:
        cur=conn.cursor()
        sql_query = """
            SELECT table_name FROM information_schema.tables
            WHERE table_schema = 'public'
        """
        cur.execute(sql_query)
        rows = cur.fetchall()
        return len(rows)


def connect_to_rdb(rdb_url):
        # Retrieve host and port from RDB URL assuming default format like
        # jdbc:postgresql://localhost:5432/<url_route>
        host = rdb_url.split(':')[2].replace('//', '')
        port = rdb_url.split(':')[3].split('/')[0]
        return pg.connect(host=host, port=port, database=DATABASE,
                          user=DB_USER, password=DB_PASSWORD)


def initialise_timeseries(kgclient, dataIRI, dates, values, rdb_url, 
                          rdb_user, rdb_password):
    # Initialise timeseries (in Blazegraph and PostgreSQL) with given dates and values
    # Initialise time series client
    ts_client = TSClient(kg_client=kgclient, rdb_url=rdb_url, rdb_user=rdb_user, 
                         rdb_password=rdb_password)
    # Create time series from test data                        
    ts = TSClient.create_timeseries(dates, [dataIRI], [values])
    
    with ts_client.connect() as conn:
        # Initialise time series in Blazegraph and PostgreSQL
        ts_client.tsclient.initTimeSeries([dataIRI], [DATACLASS], TIME_FORMAT_SHORT, conn)
        # Add test time series data
        ts_client.tsclient.addTimeSeriesData(ts, conn)


def retrieve_timeseries(kgclient, dataIRI, rdb_url, rdb_user, rdb_password):
    # Initialise time series client
    ts_client = TSClient(kg_client=kgclient, rdb_url=rdb_url, rdb_user=rdb_user, 
                         rdb_password=rdb_password)
    with ts_client.connect() as conn:
        ts = ts_client.tsclient.getTimeSeries([dataIRI], conn)
    dates = ts.getTimes()
    # Unwrap Java time objects
    dates = [d.toString() for d in dates]
    values = ts.getValues(dataIRI)
    return dates, values


def get_marketvalue_details(sparql_client, market_value_iri):
    # Returns details associated with Market Value instance (om:AmountOfMoney)
    query = f"""
        SELECT ?value ?unit ?input_iri ?input_type
        WHERE {{
        <{market_value_iri}> <{RDF_TYPE}> <{OM_AMOUNT_MONEY}> ; 
                             <{OM_HAS_VALUE}> ?measure ; 
                             <{ONTODERIVATION_BELONGSTO}>/<{ONTODERIVATION_ISDERIVEDFROM}> ?input_iri . 
        ?measure <{RDF_TYPE}> <{OM_MEASURE}> ; 
        OPTIONAL {{ ?measure <{OM_HAS_UNIT}>/<{OM_SYMBOL}> ?unit ; 
                             <{OM_NUM_VALUE}> ?value . }}
        ?input_iri <{RDF_TYPE}> ?input_type . 
        }}
        """
    response = sparql_client.performQuery(query)
    if len(response) == 0:
        return None
    else:
        # Derivation inputs (i.e. isDerivedFrom)
        key = set([x['input_type'] for x in response])
        inputs = {k: [x['input_iri'] for x in response if x['input_type'] == k] for k in key}
        # Market Value and monetary unit
        market_value = list(set([x.get('value') for x in response]))
        market_value = [float(x) for x in market_value if x]
        # NOTE: Fix encoding issue with pound sterling
        unit = list(set([x.get('unit') for x in response]))
        unit = [str(x).encode('ISO-8859-1').decode('utf-8') for x in unit if x]
        return inputs, market_value, unit


def get_derivation_status(sparql_client, derivation_iri):
    # Returns status comment of derivation

    query = f"""
        SELECT ?exception
        WHERE {{
        <{derivation_iri}> <{RDF_TYPE}> <{ONTODERIVATION_DERIVATIONASYN}> ; 
                           <{ONTODERIVATION_HASSTATUS}> ?status . 
        ?status <{RDF_TYPE}> <{ONTODERIVATION_ERROR}> ; 
                <{RDFS_COMMENT}> ?exception . 
        }}
        """
    response = sparql_client.performQuery(query)
    if len(response) == 0:
        return None
    else:
        response = response[0]
        return str(response['exception'])
    

def get_avgprice_and_measure(sparql_client, deriv_inputs):
    # Returns Measure instance associated with AverageSquareMetrePrice
    # as well as the numerical value of the measure

    query = f"""
        SELECT ?measure_iri ?num_value
        WHERE {{
        VALUES ?price_iri {{ <{'> <'.join(deriv_inputs)}> }} 
        ?price_iri <{RDF_TYPE}> <{OBE_AVERAGE_SM_PRICE}> ; 
                   <{OM_HAS_VALUE}> ?measure_iri . 
        ?measure_iri <{OM_NUM_VALUE}> ?num_value .
        }}
        """
    response = sparql_client.performQuery(query)
    measure = response[0].get('measure_iri')
    val = float(response[0].get('num_value'))
    return measure, val


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
