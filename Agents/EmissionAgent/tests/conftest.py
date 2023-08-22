################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) # 
# Date: 22 Aug 2023                            #
################################################

# This module provides all pytest fixtures and other utility functions for the
# actual (integration) tests

# Avoid unnecessary logging information from py4j package
import logging
logging.getLogger("py4j").setLevel(logging.INFO)

import os
import pytest
import requests
import time
import numpy as np
import pandas as pd
import psycopg2 as pg
from flask import Flask
from pathlib import Path
from rdflib import Graph
from urllib.parse import urlparse

from pyderivationagent.data_model.iris import ONTODERIVATION_ISDERIVEDFROM, \
                                              ONTODERIVATION_BELONGSTO, \
                                              ONTODERIVATION_DERIVATION

from emissionagent.datamodel.iris import *
from emissionagent.agent import EmissionAgent
from emissionagent.kgutils.kgclient import KGClient
from emissionagent.kgutils.tsclient import TSClient
from emissionagent.utils.baselib_gateway import jpsBaseLibGW

from emissionagent.utils.env_configs import DB_URL, DB_USER, DB_PASSWORD, \
                                               SPARQL_QUERY_ENDPOINT, \
                                               SPARQL_UPDATE_ENDPOINT


# ----------------------------------------------------------------------------------
# Constants and configuration
# ----------------------------------------------------------------------------------
# Values to be adjusted as needed, i.e. in line with values provided in
#    - docker-compose files
#    - example_abox.ttl

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
TEST_TRIPLES_DIR = os.path.join(THIS_DIR, 'test_triples')

# Provide names of respective Docker services
# NOTE These names need to match the ones given in the `docker-compose-testcontainers.yml` file
KG_SERVICE = "blazegraph_test"
RDB_SERVICE = "postgres_test"

# IRIs of derivation's (pure) inputs
TEST_TRIPLES_BASE_IRI = 'https://www.theworldavatar.com/test/'

# Expected number of triples
ABOX_TRIPLES = 28
TS_TRIPLES = 4
TIME_TRIPLES_PER_PURE_INPUT = 6
AGENT_SERVICE_TRIPLES = 4       # agent service triples
DERIV_INPUT_TRIPLES = 2 + 4*3   # triples for derivation input message
DERIV_OUTPUT_TRIPLES = 2 + 1*3  # triples for derivation output message
EMISSION_TRIPLES = 21           # triples for newly instantiated emission instance
# NOTE: Derivation Framework attaches belongsTo relationship to each output with rdf type
RDF_TYPES_PER_EMISSION = 7      # number of rdf type statements per emission instance 
                                # (incl. quantity, measure, ...)
OUTPUT_TYPES = 5                # Emission, Temperature, Density, MassFlow, Measure

# 
#  Values which should not require changing
#

# Derivation agent and markup base urls
DERIVATION_INSTANCE_BASE_URL = os.getenv('DERIVATION_INSTANCE_BASE_URL')

# Define time series value data type and format
jpsBaseLibView = jpsBaseLibGW.createModuleView()
DOUBLE = jpsBaseLibView.java.lang.Double.TYPE
TIME_FORMAT = "%Y-%m-%dT%H:%M:%SZ"

# Create synthetic time series data (for Prophet tests)
times = pd.date_range(start='2023-01-01T00:00:00Z', freq='H', 
                      end='2023-02-01T00:00:00Z')
TIMES = times.strftime("%Y-%m-%dT%H:%M:%SZ").tolist()
# Example time series values
VALUES_1 = [5.0 for i in range(len(times))]
VALUES_2 = [10.0 for i in range(len(times))]

# Times for which to test extraction of time series values
TIME_1 = 1673308800     # 2023-01-10T00:00:00Z
TIME_2 = 1674172800     # 2023-01-20T00:00:00Z
TIME_3 = 1677628800     # 2023-03-01T00:00:00Z (out of bounds)
VALUE_1 = 11.11
VALUE_2 = 22.22
VALUE_3 = f'SimulationTime unix value {TIME_3} not found in time series data.'
# Replace values at specific times
VALUES_1[TIMES.index('2023-01-10T00:00:00Z')] = VALUE_1
VALUES_1[TIMES.index('2023-01-20T00:00:00Z')] = VALUE_2

# ----------------------------------------------------------------------------------
#  Test Inputs
# ----------------------------------------------------------------------------------

SIMULATION_TIME_1 = TEST_TRIPLES_BASE_IRI + 'SimulationTime_1'
SIMULATION_TIME_2 = TEST_TRIPLES_BASE_IRI + 'SimulationTime_2'
SIMULATION_TIME_3 = TEST_TRIPLES_BASE_IRI + 'SimulationTime_3'
POINT_SOURCE_1 = TEST_TRIPLES_BASE_IRI + 'StaticPointSource_1'
POINT_SOURCE_2 = TEST_TRIPLES_BASE_IRI + 'StaticPointSource_2'
PROVIDED_HEAT_AMOUNT_1 = TEST_TRIPLES_BASE_IRI + 'ProvidedHeatAmount_1'
DATA_IRI_1 = TEST_TRIPLES_BASE_IRI + 'Measure_1'
PROVIDED_HEAT_AMOUNT_2 = TEST_TRIPLES_BASE_IRI + 'ProvidedHeatAmount_2'
CONSUMED_GAS_AMOUNT_1 = TEST_TRIPLES_BASE_IRI + 'ConsumedGasAmount_1'
DATA_IRI_2 = TEST_TRIPLES_BASE_IRI + 'Measure_2'
CONSUMED_GAS_AMOUNT_2 = TEST_TRIPLES_BASE_IRI + 'ConsumedGasAmount_2'
DATA_IRI_3 = TEST_TRIPLES_BASE_IRI + 'Measure_3'
CONSUMED_GAS_AMOUNT_3 = TEST_TRIPLES_BASE_IRI + 'ConsumedGasAmount_3'

# Define derivation input sets to test
DERIVATION_INPUTS_1 = [SIMULATION_TIME_1, POINT_SOURCE_1, 
                       PROVIDED_HEAT_AMOUNT_1]
DERIVATION_INPUTS_2 = []
# Expected emission outputs
EXPECTED_OUTPUTS_1 = {
    OD_NO2: {'temperature': 10.0, 'density': 10.0, 'massflow': 10.0}, 
    OD_PM2_5: {'temperature': 10.0, 'density': 10.0, 'massflow': 10.0}, 
    OD_PM10: {'temperature': 10.0, 'density': 10.0, 'massflow': 10.0}
}


# Define erroneous derivation input sets as retrieved by derivation agent
# --> correct exceptions tested as unit tests
ERRONEOUS_INPUTS_1 = {
    OD_SIMULATION_TIME: [],
    OD_STATIC_POINT_SOURCE: [],
}
ERRONEOUS_INPUTS_2 = {
    OD_SIMULATION_TIME: [SIMULATION_TIME_1, SIMULATION_TIME_2],
    OD_STATIC_POINT_SOURCE: [],
}
ERRONEOUS_INPUTS_3 = {
    OD_SIMULATION_TIME: [SIMULATION_TIME_1],
    OD_STATIC_POINT_SOURCE: [],
}
ERRONEOUS_INPUTS_4 = {
    OD_SIMULATION_TIME: [SIMULATION_TIME_1],
    OD_STATIC_POINT_SOURCE: [POINT_SOURCE_1],
}
ERRONEOUS_INPUTS_5 = {
    OD_SIMULATION_TIME: [SIMULATION_TIME_1],
    OD_STATIC_POINT_SOURCE: [POINT_SOURCE_1],
    OHN_PROVIDED_HEAT_AMOUNT: [PROVIDED_HEAT_AMOUNT_1],
    OHN_CONSUMED_GAS_AMOUNT: [CONSUMED_GAS_AMOUNT_1],
}
ERRONEOUS_INPUTS_6 = {
    OD_SIMULATION_TIME: [SIMULATION_TIME_1],
    OD_STATIC_POINT_SOURCE: [POINT_SOURCE_1],
    OHN_PROVIDED_HEAT_AMOUNT: [PROVIDED_HEAT_AMOUNT_1, PROVIDED_HEAT_AMOUNT_2],
}

# Expected error messages
ERRONEOUS_MSG_1 = "No 'SimulationTime' IRI provided"
ERRONEOUS_MSG_2 = "More than one 'SimulationTime' IRI provided"
ERRONEOUS_MSG_3 = "No 'StaticPointSource' IRI provided"
ERRONEOUS_MSG_4 = "Neither 'ProvidedHeatAmount' nor 'ConsumedGasAmount' instances provided"
ERRONEOUS_MSG_5 = "Both 'ProvidedHeatAmount' and 'ConsumedGasAmount' instances provided"
ERRONEOUS_MSG_6 = "More than one 'ProvidedHeatAmount' instance provided"


# ----------------------------------------------------------------------------------
# Session-scoped test fixtures
# (i.e. the fixture is destroyed at the end of the test session)
# ----------------------------------------------------------------------------------

@pytest.fixture(scope="session")
def get_blazegraph_service_url(session_scoped_container_getter):
    def _get_service_url(service_name, hostname, url_route):
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
    def _get_service_url(service_name, hostname, database_name):
        service = session_scoped_container_getter.get(service_name).network_info[0]
        service_url = f"jdbc:postgresql://{hostname}:{service.host_port}/{database_name}"
        print(f'PostgreSQL endpoint: {service_url}')

        # This will run only once per entire test session
        # It ensures that the requested PostgreSQL Docker service is ready to accept queries
        service_available = False
        while not service_available:
            try:
                conn = pg.connect(host=hostname, port=service.host_port,
                                  user=DB_USER, password=DB_PASSWORD,
                                  database=database_name)
                if conn.status == pg.extensions.STATUS_READY:
                    service_available = True
            except Exception:
                time.sleep(3)

        print('Connected to PostgreSQL.')
        return service_url
    return _get_service_url


@pytest.fixture(scope="session")
def initialise_clients(get_blazegraph_service_url, get_postgres_service_url):
    # Retrieve "user-facing" endpoints for all dockerised testing services
    
    # Retrieve endpoint for triple store
    bg = urlparse(SPARQL_QUERY_ENDPOINT)
    host = bg.hostname
    path = bg.path[1:]
    sparql_endpoint = get_blazegraph_service_url(KG_SERVICE, hostname=host,
                                                 url_route=path)
    # Create SparqlClient for testing
    kg_client = KGClient(sparql_endpoint, sparql_endpoint)
    
    # Retrieve endpoint for postgres
    db = DB_URL[DB_URL.rfind('/')+1:]
    rdb_url = get_postgres_service_url(RDB_SERVICE, hostname=host, 
                                       database_name=db)
    # Create TimeSeriesClient for testing
    ts_client = TSClient(kg_client=kg_client, rdb_url=rdb_url, 
                         rdb_user=DB_USER, rdb_password=DB_PASSWORD)
    
    # Create DerivationClient for creating derivation instances
    derivation_client = kg_client.jpsBaseLib_view.DerivationClient(
        kg_client.kg_client,
        DERIVATION_INSTANCE_BASE_URL
    )

    yield kg_client, ts_client, derivation_client, rdb_url

    # Clear logger at the end of the test
    clear_loggers()


# ----------------------------------------------------------------------------------
# Module-scoped test fixtures
# (i.e. the fixture is destroyed during teardown of the last test in the module)
# ----------------------------------------------------------------------------------

@pytest.fixture(scope="module")
def create_example_agent():
    def _create_example_agent(
            register_agent:bool=True,
            ontoagent_service_iri=None,
            ontoagent_http_url=None
            ):
        agent = EmissionAgent(
            register_agent=register_agent,
            agent_iri=ontoagent_service_iri if ontoagent_service_iri else 
                      os.getenv('ONTOAGENT_SERVICE_IRI'),
            agent_endpoint=ontoagent_http_url if ontoagent_http_url else
                           os.getenv('ONTOAGENT_OPERATION_HTTP_URL'),
            # Settings which don't really matter for dockerised testing (as settings of
            # already running dockerised agents will be used once this one gets registered)
            # NOTE: only relevant to allow for unit testing with "fake" agent
            time_interval=int(os.getenv('DERIVATION_PERIODIC_TIMESCALE')),
            derivation_instance_base_url=os.getenv('DERIVATION_INSTANCE_BASE_URL'),
            kg_url=SPARQL_QUERY_ENDPOINT,
            kg_update_url=SPARQL_UPDATE_ENDPOINT,
            app=Flask(__name__),
            logger_name='dev'
        )
        return agent
    return _create_example_agent


# ----------------------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------------------

def get_derivation_inputs_outputs(derivation_iri: str, sparql_client):
    query_output = f"""SELECT ?output ?output_type ?input ?input_type
        WHERE {{
            <{derivation_iri}> <{ONTODERIVATION_ISDERIVEDFROM}> ?input .
            ?input a ?input_type .
            ?output <{ONTODERIVATION_BELONGSTO}> <{derivation_iri}> .
            ?output a ?output_type .
        }}"""
    response = sparql_client.performQuery(query_output)
    if len(response) == 0:
        return None
    else:
        # Derivation inputs (i.e. isDerivedFrom)
        key = set([x['input_type'] for x in response])
        inputs = {k: set([x['input'] for x in response if x['input_type'] == k]) for k in key}
        # Derivation outputs (i.e. belongsTo)
        key = set([x['output_type'] for x in response])
        outputs = {k: set([x['output'] for x in response if x['output_type'] == k]) for k in key}
    
    return inputs, outputs


# def update_derivation_interval(derivation_iri: str, interval_iri: str, sparql_client):
#     # Replace the derivation's forecast interval with a new interval
#     update = f"""
#         DELETE {{ 
#             ?deriv_iri <{ONTODERIVATION_ISDERIVEDFROM}> ?interval . 
#         }} INSERT {{
#             ?deriv_iri  <{ONTODERIVATION_ISDERIVEDFROM}> <{interval_iri}> . 
#         }} WHERE {{
#             VALUES ?deriv_iri {{ <{derivation_iri}> }}
#             ?deriv_iri <{ONTODERIVATION_ISDERIVEDFROM}> ?interval .
#             ?interval <{RDF_TYPE}> <{TIME_INTERVAL}> .
#         }}
#         """
#     sparql_client.performUpdate(update)


def initialise_triples(sparql_client):
    # Delete all triples before initialising prepared triples
    sparql_client.performUpdate("""DELETE WHERE {?s ?p ?o.}""")

	# Upload all relevant example triples provided in the test_triples folder
    pathlist = Path(TEST_TRIPLES_DIR).glob('*.ttl')
    for path in pathlist:
        g = Graph()
        g.parse(str(path), format='turtle')
        sparql_client.uploadGraph(g)


def clear_database(rdb_url):
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
        db = rdb_url[rdb_url.rfind('/')+1:]
        return pg.connect(host=host, port=port, database=db,
                          user=DB_USER, password=DB_PASSWORD)


# method adopted from https://github.com/pytest-dev/pytest/issues/5502#issuecomment-647157873
def clear_loggers():
    """Remove handlers from all loggers"""
    import logging
    loggers = [logging.getLogger()] + list(logging.Logger.manager.loggerDict.values())
    for logger in loggers:
        handlers = getattr(logger, 'handlers', [])
        for handler in handlers:
            logger.removeHandler(handler)
