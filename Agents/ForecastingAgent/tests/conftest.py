################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) # 
# Date: 25 Jul 2023                            #
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
import uuid
import numpy as np
import pandas as pd
import psycopg2 as pg
from flask import Flask
from pathlib import Path
from rdflib import Graph
from urllib.parse import urlparse

from forecastingagent.datamodel.iris import *
from forecastingagent.agent import ForecastingAgent
from forecastingagent.agent.forcasting_config import *
from forecastingagent.utils.tools import *
from forecastingagent.kgutils.kgclient import KGClient
from forecastingagent.kgutils.tsclient import TSClient

from forecastingagent.utils.env_configs import DB_URL, DB_USER, DB_PASSWORD, \
                                               SPARQL_QUERY_ENDPOINT, \
                                               SPARQL_UPDATE_ENDPOINT, \
                                               OVERWRITE_FORECAST


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
TBOX_TRIPLES = 7
ABOX_TRIPLES = 36
TS_TRIPLES = 4
TIME_TRIPLES_PER_PURE_INPUT = 6
AGENT_SERVICE_TRIPLES = 4       # agent service triples
DERIV_INPUT_TRIPLES = 2 + 6*3   # triples for derivation input message
DERIV_OUTPUT_TRIPLES = 2 + 1*3  # triples for derivation output message


# 
#  Values which should not require changing
#

# Derivation agent and markup base urls
DERIVATION_INSTANCE_BASE_URL = os.getenv('DERIVATION_INSTANCE_BASE_URL')
AGENT_BASE_URL = os.getenv('ONTOAGENT_OPERATION_HTTP_URL')
AGENT_BASE_URL = AGENT_BASE_URL[:AGENT_BASE_URL.rfind('/')+1]

# Create synthetic time series data
times = pd.date_range(start='2019-10-01T00:00:00Z', freq='H', 
                      end='2020-02-01T00:00:00Z')
TIMES = times.strftime("%Y-%m-%dT%H:%M:%SZ").tolist()
# Linearly increasing time series
VALUES_1 = [round(i*(100/len(times)),5) for i in range(1, len(times)+1)]    # original
VALUES_2 = VALUES_1.copy()
VALUES_2[100] = VALUES_2[100]*2     # slightly distorted copy
# Constant value time series
VALUES_3 = [1 for i in range(1, len(times)+1)]


# ----------------------------------------------------------------------------------
#  Test Inputs
# ----------------------------------------------------------------------------------

IRI_TO_FORECAST_1 = TEST_TRIPLES_BASE_IRI + 'HeatDemand_1'      #om:Quantity
IRI_TO_FORECAST_2 = TEST_TRIPLES_BASE_IRI + 'HeatDemand_2'      #om:Quantity
IRI_TO_FORECAST_3 = TEST_TRIPLES_BASE_IRI + 'Availability_1'    #owl:Thing
IRI_TO_FORECAST_4 = TEST_TRIPLES_BASE_IRI + 'Availability_2'    #owl:Thing
FORECASTING_MODEL_1 = TEST_TRIPLES_BASE_IRI + 'ForecastingModel_1'
FC_INTERVAL_1 = TEST_TRIPLES_BASE_IRI + 'OptimisationInterval_1'
FC_FREQUENCY_1 = TEST_TRIPLES_BASE_IRI + 'Frequency_1'
HIST_DURATION_1 = TEST_TRIPLES_BASE_IRI + 'Duration_1'
HIST_DURATION_2 = TEST_TRIPLES_BASE_IRI + 'Duration_2'

# Define derivation input sets to test
# Forecast 1 : 
#  - Prophet
#   - Jan 01 2020 00:00:00 UTC - Jan 02 2020 00:00:00 UTC
DERIVATION_INPUTS_1 = [IRI_TO_FORECAST_1, FORECASTING_MODEL_1, 
                       FC_INTERVAL_1, FC_FREQUENCY_1, HIST_DURATION_1]
DERIVATION_INPUTS_2 = [IRI_TO_FORECAST_2, FORECASTING_MODEL_1,
                       FC_INTERVAL_1, FC_FREQUENCY_1, HIST_DURATION_1]

# Define erroneous derivation input sets as retrieved by derivation agent
# --> correct exceptions tested as unit tests
# NOTE: As om:Quantity and ts:ForecastingModel are also owl:Things, multiple
#       owl:Things will be detected by the derivation agent
ERRONEOUS_FORECAST_INPUTS_1 = {
    OM_QUANTITY: [IRI_TO_FORECAST_1],
    OWL_THING: [IRI_TO_FORECAST_1],
    TS_FREQUENCY: [FC_FREQUENCY_1],
    TIME_INTERVAL: [FC_INTERVAL_1],
    TIME_DURATION: [HIST_DURATION_1]
}
ERRONEOUS_FORECAST_INPUTS_2 = {
    OM_QUANTITY: [IRI_TO_FORECAST_1],
    OWL_THING: [IRI_TO_FORECAST_1, FORECASTING_MODEL_1],
    TS_FORECASTINGMODEL: [FORECASTING_MODEL_1],
    TS_FREQUENCY: [FC_FREQUENCY_1],
    TIME_INTERVAL: [FC_INTERVAL_1],
    TIME_DURATION: [HIST_DURATION_1, HIST_DURATION_2]
}
ERRONEOUS_FORECAST_INPUTS_3 = {
    OWL_THING: [FORECASTING_MODEL_1],
    TS_FORECASTINGMODEL: [FORECASTING_MODEL_1],
    TS_FREQUENCY: [FC_FREQUENCY_1],
    TIME_INTERVAL: [FC_INTERVAL_1],
    TIME_DURATION: [HIST_DURATION_1]
}
ERRONEOUS_FORECAST_INPUTS_4 = {
    OM_QUANTITY: [IRI_TO_FORECAST_1, IRI_TO_FORECAST_2],
    OWL_THING: [FORECASTING_MODEL_1],
    TS_FORECASTINGMODEL: [FORECASTING_MODEL_1],
    TS_FREQUENCY: [FC_FREQUENCY_1],
    TIME_INTERVAL: [FC_INTERVAL_1],
    TIME_DURATION: [HIST_DURATION_1]
}
ERRONEOUS_FORECAST_INPUTS_5 = {
    OWL_THING: [IRI_TO_FORECAST_3, IRI_TO_FORECAST_4, FORECASTING_MODEL_1],
    TS_FORECASTINGMODEL: [FORECASTING_MODEL_1],
    TS_FREQUENCY: [FC_FREQUENCY_1],
    TIME_INTERVAL: [FC_INTERVAL_1],
    TIME_DURATION: [HIST_DURATION_1]
}

# Define skeleton for correct forecast error evaluation request
ERROR_REQUEST = {'query': {
        'tsIRI_target': None,
        'tsIRI_fc' : None } }
# Define erroneous HTTP requests for forecast error evaluation
ERRONEOUS_ERROR_REQUEST_1 = {'quer': {}}
ERRONEOUS_ERROR_REQUEST_2 = {'query': {
        'tsIRI_target': 'https://www.theworldavatar.com/kg/...' } }
ERRONEOUS_ERROR_REQUEST_3 = {'query': {
        'tsIRI_target': 'https://www.theworldavatar.com/kg/ontotimeseries/Timeseries_1',
        'tsIRI_fc' : 'https://www.theworldavatar.com/kg/ontotimeseries/Timeseries_2' } }

# Expected error messages
ERRONEOUS_FORECAST_MSG_1 = "No 'ForecastingModel' IRI provided"
ERRONEOUS_FORECAST_MSG_2 = "More than one 'Duration' IRI provided"
ERRONEOUS_FORECAST_MSG_3 = "Neither 'om:Quantity' nor 'owl:Thing' IRI provided to forecast"
ERRONEOUS_FORECAST_MSG_4 = "More than one 'om:Quantity' IRI provided to forecast"
ERRONEOUS_FORECAST_MSG_5 = "No unique 'owl:Thing' IRI provided to forecast"
ERRONEOUS_ERROR_MSG_1 = "No 'query' node provided in HTTP request"
ERRONEOUS_ERROR_MSG_2 = "Unable to extract time series IRIs to evaluate"
ERRONEOUS_ERROR_MSG_3 = "No dataIRI found for tsIRI"


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
    def _create_example_agent(random_agent_iri:bool=False, register_agent:bool=True):
        agent = ForecastingAgent(
            register_agent=register_agent,
            agent_iri=os.getenv('ONTOAGENT_SERVICE_IRI') if not random_agent_iri else 'http://agent_' + str(uuid.uuid4()),
            time_interval=int(os.getenv('DERIVATION_PERIODIC_TIMESCALE')),
            derivation_instance_base_url=os.getenv('DERIVATION_INSTANCE_BASE_URL'),
            kg_url=SPARQL_QUERY_ENDPOINT,
            kg_update_url=SPARQL_UPDATE_ENDPOINT,
            agent_endpoint=os.getenv('ONTOAGENT_OPERATION_HTTP_URL'),
            overwrite_fc=OVERWRITE_FORECAST,
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


# def initialise_prophet(kgClient, tsClient, rdb_url):
#     """
#     Initialise Blazegrph and PostgreSQL with test data for Prophet test
#     """

#     # Clear Blazegraph and PostgreSQL
#     clear_database(rdb_url)
#     clear_triplestore(kgClient)

#     # Verify that Triplestore and RDB are empty
#     assert get_number_of_rdb_tables(rdb_url) == 0
#     assert get_number_of_triples(kgClient) == 0

#     # Initialise SPARQL update
#     update = ''

#     # Generate test data
#     test_data = pd.DataFrame({
#         'timestamp': TIMESTAMPS,
#         'generatedHeat': np.random.rand(PERIODS)})

#     update += get_properties_for_subj(subj=GENERATED_HEAT_DATAIRI, 
#                                       verb_obj={RDF_TYPE: OM_MEASURE,
#                                                 OM_HASUNIT: OM_MEGAWATTHOUR
#                                             })
#     update += get_properties_for_subj(subj=GENERATED_HEAT_IRI, 
#                                       verb_obj={RDF_TYPE: OHN_GENERATEDHEATAMOUNT,
#                                                 OM_HASVALUE: GENERATED_HEAT_DATAIRI
#                                             })
#     # Convert to proper format
#     update = add_insert_data(update)

#     # Execute query
#     kgClient.performUpdate(update)

#     # Initialise time series and add data
#     tsClient.init_ts(GENERATED_HEAT_DATAIRI, test_data['timestamp'], test_data['generatedHeat'], 
#                      ts_type=DOUBLE, time_format=TIME_FORMAT_TS)


# def initialise_tft(kgClient, tsClient, rdb_url):
#     """
#     Initialise Blazegrph and PostgreSQL with test data for TFT test
#     """

#     # Clear Blazegraph and PostgreSQL
#     clear_database(rdb_url)
#     clear_triplestore(kgClient)

#     # Verify that Triplestore and RDB are empty
#     assert get_number_of_rdb_tables(rdb_url) == 0
#     assert get_number_of_triples(kgClient) == 0

#     # Initialise SPARQL update
#     update = ''

#     # Generate test data
#     test_data = pd.DataFrame({
#         'timestamp': TIMESTAMPS,
#         'heatDemand': np.random.rand(PERIODS),
#         'airTemp': np.random.rand(PERIODS),
#         'isHoliday': np.random.randint(0, 2, PERIODS)})

#     update += get_properties_for_subj(subj=HEAT_DEMAND_DATA_IRI, 
#                                       verb_obj={RDF_TYPE: OM_MEASURE,
#                                                 OM_HASUNIT: OM_MEGAWATTHOUR
#                                             })
#     update += get_properties_for_subj(subj=HEAT_DEMAND_IRI, 
#                                       verb_obj={RDF_TYPE: OHN_HEATDEMAND,
#                                                 OM_HASVALUE: HEAT_DEMAND_DATA_IRI
#                                             })

#     # Initialise time series and add data
#     tsClient.init_ts(HEAT_DEMAND_DATA_IRI, test_data['timestamp'], test_data['heatDemand'],  
#                      ts_type=DOUBLE, time_format=TIME_FORMAT_TS)

#     # Instantiate covariates
#     # air temperature
#     airTemp_dataIRI = KB + "AirTemperature_" + str(uuid.uuid4())
#     update += get_properties_for_subj(subj=airTemp_dataIRI, 
#                                       verb_obj={RDF_TYPE: ONTOEMS_AIRTEMPERATURE,
#                                                 OM_HASVALUE: airTemp_dataIRI
#                                             })
#     tsClient.init_ts(airTemp_dataIRI, test_data['timestamp'], test_data['airTemp'],
#                      ts_type=DOUBLE, time_format=TIME_FORMAT_TS)

#     # is holiday
#     isHoliday_dataIRI = KB + "IsHoliday_" + str(uuid.uuid4())
#     update += get_properties_for_subj(subj=isHoliday_dataIRI, 
#                                       verb_obj={RDF_TYPE: OHN_ISPUBLICHOLIDAY,
#                                                 OM_HASVALUE: isHoliday_dataIRI
#                                             })
#     tsClient.init_ts(isHoliday_dataIRI, test_data['timestamp'], test_data['isHoliday'],  
#                      ts_type=DOUBLE, time_format=TIME_FORMAT_TS)

#     # Execute SPARQL update
#     kgClient.performUpdate(add_insert_data(update))
