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
from pathlib import Path
from rdflib import Graph
from urllib.parse import urlparse

from forecastingagent.agent import ForecastingAgent
from forecastingagent.datamodel.iris import *
from forecastingagent.datamodel.data_mapping import *
from forecastingagent.utils.tools import *
from forecastingagent.kgutils.kgclient import KGClient
from forecastingagent.kgutils.tsclient import TSClient

from forecastingagent.utils.env_configs import DB_URL, DB_USER, DB_PASSWORD, \
                                               SPARQL_QUERY_ENDPOINT


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

# Derivation markup
DERIVATION_INSTANCE_BASE_URL = os.getenv('DERIVATION_INSTANCE_BASE_URL')
# IRIs of derivation's (pure) inputs
TEST_TRIPLES_BASE_IRI = 'https://www.theworldavatar.com/test/'

# Forecast 1
# Start: Wed Jan 01 2020 00:00:00 GMT+0000
# End:   Thu Jan 02 2020 00:00:00 GMT+0000
IRI_TO_FORECAST_1 = TEST_TRIPLES_BASE_IRI + 'TestHeatDemand_1'
FORECASTING_MODEL_1 = TEST_TRIPLES_BASE_IRI + 'ForecastingModel_1'
FC_INTERVAL_1 = TEST_TRIPLES_BASE_IRI + 'OptimisationInterval_1'
FC_FREQUENCY_1 = TEST_TRIPLES_BASE_IRI + 'Frequency_1'
HIST_DURATION_1 = TEST_TRIPLES_BASE_IRI + 'Duration_1'
DERIVATION_INPUTS_1 = [IRI_TO_FORECAST_1, FORECASTING_MODEL_1, 
                       FC_INTERVAL_1, FC_FREQUENCY_1, HIST_DURATION_1]


# ----------------------------------------------------------------------------------
#  Inputs which should not be changed
#
# Create synthetic linearly increasing time series data
times = pd.date_range(start='2019-10-01T00:00:00Z', freq='H', 
                      end='2020-02-01T00:00:00Z')
VALUES = [i*(100/len(times)) for i in range(1, len(times)+1)]
TIMES = times.strftime("%Y-%m-%dT%H:%M:%SZ").tolist()

# Expected number of triples
INITIAL_TRIPLES = 26
TS_TRIPLES = 4
TIME_TRIPLES_PER_PURE_INPUT = 6
DERIV_STATUS_TRIPLES = 2        # derivation status triples
AGENT_SERVICE_TRIPLES = 5       # agent service triples
DERIV_INPUT_TRIPLES = 2 + 3*3   # triples for derivation input message
DERIV_OUTPUT_TRIPLES = 5        # triples for derivation output message
DERIV_OUTPUT_COMPUATBLE = 6     # triples instantiated for successful avg price calculation
                                # i.e. kgclient.instantiate_average_price


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

# @pytest.fixture(scope="module")
# def create_example_agent():
#     def _create_example_agent(random_agent_iri:bool=False):
#         agent = ForecastingAgent(
#             register_agent=os.getenv('REGISTER_AGENT'),
#             agent_iri=os.getenv('ONTOAGENT_SERVICE_IRI') if not random_agent_iri else 'http://agent_' + str(uuid.uuid4()),
#             time_interval=os.getenv('DERIVATION_PERIODIC_TIMESCALE'),
#             derivation_instance_base_url=os.getenv('DERIVATION_INSTANCE_BASE_URL'),
#             kg_url=SPARQL_QUERY_ENDPOINT,
#             kg_update_url=SPARQL_UPDATE_ENDPOINT,
#             agent_endpoint=os.getenv('ONTOAGENT_OPERATION_HTTP_URL'),
#             threshold=THRESHOLD,
#             app=Flask(__name__),
#             logger_name='dev'
#         )
#         return agent
#     return _create_example_agent


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




#-----------------------------------------------------------------------------
### TEST INPUTS

# # test prophet
# query1 = {"query": {
#           "forecast_start_date": FORECAST_START_PROPHET,
#           "iri": GENERATED_HEAT_IRI,
#           "data_length": 168,
#           "horizon": 3,
#           "use_model_configuration": "DEFAULT"
#         }}
# expected1 = {'fc_model': {'train_again': True, 'name': 'prophet', 'scale_data': False, 'input_length': query1['query']['data_length']},
#              'data_length': query1['query']['data_length'],
#              'model_configuration_name': query1['query']['use_model_configuration'],
#              'iri': query1['query']['iri'],
#              'horizon': query1['query']['horizon'],
#              'forecast_start_date': FORECAST_START_PROPHET,
#              'model_input_interval': ['Thu, 15 Aug 2019 01:00:00 GMT', 'Thu, 22 Aug 2019 00:00:00 GMT'],
#              'model_output_interval': ['Thu, 22 Aug 2019 01:00:00 GMT', 'Thu, 22 Aug 2019 03:00:00 GMT'],
#              'unit': OM_MEGAWATTHOUR,
#             }

# query2 = {"query": {
#           "iri": GENERATED_HEAT_IRI,
#           "data_length": 168,
#           "horizon": 3
#         }}
# expected2 = {'fc_model': {'train_again': True, 'name': 'prophet', 'scale_data': False, 'input_length': query2['query']['data_length']},
#              'data_length': query2['query']['data_length'],
#              'model_configuration_name': 'DEFAULT',
#              'iri': query2['query']['iri'],
#              'horizon': query2['query']['horizon'],
#              'forecast_start_date': FORECAST_START_PROPHET,
#              'model_input_interval': ['Thu, 15 Aug 2019 01:00:00 GMT', 'Thu, 22 Aug 2019 00:00:00 GMT'],
#              'model_output_interval': ['Thu, 22 Aug 2019 01:00:00 GMT', 'Thu, 22 Aug 2019 03:00:00 GMT'],
#              'unit': OM_MEGAWATTHOUR,
#              'time_format': TIME_FORMAT_TS
#             }

# # test prophet error
# query_error1 = {"query": {
#                 "iri": GENERATED_HEAT_IRI
#                 }}
# expected_error1 = '"horizon" (how many steps to forecast) must be provided.'

# query_error2 = {"query": {
#                 "horizon": 3
#                 }}
# expected_error2 = '"iri" must be provided.'

# query_error3 = {}
# expected_error3 = 'No JSON "query" object could be identified.'

# query_error4 = {"query": {
#                 "iri": 'blablabla',
#                 "data_length": 168,
#                 "horizon": 3
#                 }}
# expected_error4 = 'No time series data could be retrieved for the given IRI: blablabla'

# query_error5 = {"query": {
#                 "iri": GENERATED_HEAT_IRI,
#                 "data_length": 168,
#                 "horizon": 3,
#                 "use_model_configuration": "blablabla"
#                 }}
# expected_error5 = 'No model configuration found for the given key: blablabla'

# query_error6 = {"query": {
#                 "iri": GENERATED_HEAT_IRI,
#                 "data_length": 168,
#                 "horizon": 24,
#                 "forecast_start_date": "2049-08-15T01:00:00Z"
#                 }}
# #NOTE There seem to be issues in finding test results using parameterized fixtures
# # which contain forward slashes (details: https://github.com/microsoft/vscode-python/issues/17079)
# # Hence, only check for beginning of error message
# expected_error6 = 'no values for dataIRI '

# query_error7 = {"query": {
#                 "iri": GENERATED_HEAT_IRI + 'blablabla',
#                 "data_length": 168,
#                 "horizon": 24,
#                 "forecast_start_date": "2049-08-15T01:00:00Z"
#                 }}
# expected_error7 = 'Could not get time series for '

# # test temporal fusion transformer (tft)
# query3 = {"query": {
#     "forecast_start_date": FORECAST_START_TFT,
#     "iri": HEAT_DEMAND_IRI,
#     "data_length": 168,
#     "horizon": 24,
#     "use_model_configuration": "TFT_HEAT_SUPPLY"
# }}
# expected3 = {'fc_model': {'train_again': False, 'name': 'tft', 'scale_data': True, 'input_length': query3['query']['data_length'],
#                           'model_path_ckpt_link': 'https://www.dropbox.com/s/fxt3iztbimvm47s/best.ckpt?dl=1',
#                           'model_path_pth_link': 'https://www.dropbox.com/s/ntg8lgvh01x09wr/_model.pth.tar?dl=1'},
#              'data_length': query3['query']['data_length'],
#              'model_configuration_name': query3['query']['use_model_configuration'],
#              'iri': query3['query']['iri'],
#              'horizon': query3['query']['horizon'],
#              'forecast_start_date': query3['query']['forecast_start_date'],
#              'model_input_interval': ['Thu, 08 Aug 2019 00:00:00 GMT', 'Wed, 14 Aug 2019 23:00:00 GMT'],
#              'model_output_interval': ['Thu, 15 Aug 2019 00:00:00 GMT', 'Thu, 15 Aug 2019 23:00:00 GMT'],
#              'unit': OM_MEGAWATTHOUR,
#              }

# # test tft error
# query_error8 = {"query": {
#                 "iri": HEAT_DEMAND_IRI,
#                 "use_model_configuration": "TFT_HEAT_SUPPLY",
#                 "data_length": 168,
#                 "horizon": 24
#                 }}
# expected_error8 = 'Not enough covariates for complete future horizon. Covariates end at '

# query_error9 = {"query": {
#                 "iri": HEAT_DEMAND_IRI,
#                 "forecast_start_date": FORECAST_START_TFT,
#                 "use_model_configuration": "TFT_HEAT_SUPPLY",
#                 "data_length": 168,
#                 "horizon": 3
#                 }}
# expected_error9 = 'Specify a horizon bigger than the output_chunk_length of your model'

# query_error10 = {"query": {
#                 "iri": HEAT_DEMAND_IRI + 'blablabla',
#                 "forecast_start_date": FORECAST_START_TFT,
#                 "use_model_configuration": "TFT_HEAT_SUPPLY",
#                 "data_length": 168,
#                 "horizon": 3
#                 }}
# expected_error10 = 'Could not get time series for '

# # test HTTP connection config error
# query_error11 = {"query": {
#                 "forecast_start_date": FORECAST_START_PROPHET,
#                 "iri": GENERATED_HEAT_IRI,
#                 "data_length": 168,
#                 "horizon": 3,
#                 "use_model_configuration": "DEFAULT",
#                 "db_url": "renamed"
#                 }}
# expected_error11 = 'Could not get time series for iri'
# query_error12 = {"query": {
#                 "forecast_start_date": FORECAST_START_PROPHET,
#                 "iri": GENERATED_HEAT_IRI,
#                 "data_length": 168,
#                 "horizon": 3,
#                 "use_model_configuration": "DEFAULT",
#                 "query_endpoint": "renamed"
#                 }}
# expected_error12 = 'SPARQL query not successful.'
