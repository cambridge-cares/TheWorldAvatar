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
import numpy as np
import pandas as pd
import psycopg2 as pg
import matplotlib.pyplot as plt
from flask import Flask
from pathlib import Path
from rdflib import Graph
from urllib.parse import urlparse

from forecastingagent.datamodel.iris import *
from forecastingagent.agent import ForecastingAgent
from forecastingagent.agent.forcasting_config import *
from forecastingagent.kgutils.kgclient import KGClient
from forecastingagent.kgutils.tsclient import TSClient

from forecastingagent.utils.env_configs import DB_URL, DB_USER, DB_PASSWORD, \
                                               SPARQL_QUERY_ENDPOINT, \
                                               SPARQL_UPDATE_ENDPOINT, \
                                               ROUNDING


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

# Derivation agent IRIs
AGENT_w_OVERWRITING_IRI = 'https://www.theworldavatar.com/resource/agents/Service__ForecastingAgent_wOverwriting/Service'
AGENT_w_OVERWRITING_URL = 'http://host.docker.internal:5001/ForecastingAgent'
AGENT_wo_OVERWRITING_IRI = 'https://www.theworldavatar.com/resource/agents/Service__ForecastingAgent_woOverwriting/Service'
AGENT_wo_OVERWRITING_URL = 'http://host.docker.internal:5002/ForecastingAgent'

# IRIs of derivation's (pure) inputs
TEST_TRIPLES_BASE_IRI = 'https://www.theworldavatar.com/test/'

# Expected number of triples
TBOX_TRIPLES = 7
ABOX_TRIPLES = 73
TS_TRIPLES = 4
TIME_TRIPLES_PER_PURE_INPUT = 6
AGENT_SERVICE_TRIPLES = 4       # agent service triples
DERIV_INPUT_TRIPLES = 2 + 6*3   # triples for derivation input message
DERIV_OUTPUT_TRIPLES = 2 + 1*3  # triples for derivation output message
FORECAST_TRIPLES = 35           # triples for newly instantiated forecast (w/o unit)
UNIT_TRIPLES = 1                # triples to assign unit to forecast
# Forecast interval time bounds in unix seconds
T_1 = 1577836800
T_2 = 1577923200
T_3 = 1578009600
# Forecast data history (in hours)
DURATION_1 = 336
DURATION_2 = 8760
# Input_chunk_length of pre-trained TFT model
DURATION_3 = 168


# 
#  Values which should not require changing
#

# Derivation agent and markup base urls
DERIVATION_INSTANCE_BASE_URL = os.getenv('DERIVATION_INSTANCE_BASE_URL')

# Create synthetic time series data (for Prophet tests)
times = pd.date_range(start='2018-12-01T00:00:00Z', freq='H', 
                      end='2020-02-01T00:00:00Z')
TIMES = times.strftime("%Y-%m-%dT%H:%M:%SZ").tolist()
# Linearly increasing time series
VALUES_1 = [round(i*(1000/len(times)),5) for i in range(1, len(times)+1)]    # original
VALUES_2 = VALUES_1.copy()
VALUES_2[500] = VALUES_2[500]*2     # slightly distorted copy
# Constant value time series
VALUES_3 = [1 for i in range(1, len(times)+1)]
# Link to historical district heating data (to allow for more meaningful testing)
DH_DATA = 'https://www.dropbox.com/s/qqosbkg38fv6s93/dh_timeseries_data_2020.csv?dl=1'


# ----------------------------------------------------------------------------------
#  Test Inputs
# ----------------------------------------------------------------------------------

IRI_TO_FORECAST_1 = TEST_TRIPLES_BASE_IRI + 'HeatDemand_1'      #om:Quantity
ASSOCIATED_DATAIRI_1 = TEST_TRIPLES_BASE_IRI + 'Measure_1'      #associated om:Measure
# Not associated om:Measure, i.e., associated with same TimeSeries but not Quantity
NOT_ASSOCIATED_DATAIRI_2 = TEST_TRIPLES_BASE_IRI + 'Measure_2'
IRI_TO_FORECAST_2 = TEST_TRIPLES_BASE_IRI + 'HeatDemand_2'      #om:Quantity
IRI_TO_FORECAST_3 = TEST_TRIPLES_BASE_IRI + 'Availability_1'    #owl:Thing
IRI_TO_FORECAST_4 = TEST_TRIPLES_BASE_IRI + 'Availability_2'    #owl:Thing
FORECASTING_MODEL_1 = TEST_TRIPLES_BASE_IRI + 'ForecastingModel_1'
FORECASTING_MODEL_2 = TEST_TRIPLES_BASE_IRI + 'ForecastingModel_2'
FORECASTING_MODEL_3 = TEST_TRIPLES_BASE_IRI + 'ForecastingModel_3'
FORECASTING_MODEL_4 = TEST_TRIPLES_BASE_IRI + 'ForecastingModel_4'
FORECASTING_MODEL_5 = TEST_TRIPLES_BASE_IRI + 'ForecastingModel_5'
FORECASTING_MODEL_6 = TEST_TRIPLES_BASE_IRI + 'ForecastingModel_6'
FC_INTERVAL_1 = TEST_TRIPLES_BASE_IRI + 'OptimisationInterval_1'
FC_INTERVAL_2 = TEST_TRIPLES_BASE_IRI + 'OptimisationInterval_2'
FC_FREQUENCY_1 = TEST_TRIPLES_BASE_IRI + 'Frequency_1'
HIST_DURATION_1 = TEST_TRIPLES_BASE_IRI + 'Duration_1'
HIST_DURATION_2 = TEST_TRIPLES_BASE_IRI + 'Duration_2'
AIRTEMPERATURE_1 = TEST_TRIPLES_BASE_IRI + 'AirTemperature_1'
ISHOLIDAY_1 = TEST_TRIPLES_BASE_IRI + 'IsHoliday_1'
PROMOTION_1 = TEST_TRIPLES_BASE_IRI + 'Promotion_1'
SPECIALEVENT_1 = TEST_TRIPLES_BASE_IRI + 'SpecialEvent_1'

# Define derivation input sets to test
# Prophet test cases
TEST_CASE_1 = 'Propeht_OM_Quantity_with_Measure_with_Unit__overwriting'
TEST_CASE_2 = 'Propeht_OM_Quantity_with_Measure_with_Unit__no_overwriting'
TEST_CASE_3 = 'Propeht_OM_Quantity_without_Measure_with_Unit__overwriting'
TEST_CASE_4 = 'Propeht_OM_Quantity_without_Measure_with_Unit__no_overwriting'
TEST_CASE_5 = 'Propeht_OM_Quantity_without_Measure_without_Unit__overwriting'
TEST_CASE_6 = 'Propeht_OM_Quantity_without_Measure_without_Unit__no_overwriting'
TEST_CASE_7 = 'Propeht_OWL_Thing_without_Measure_without_Unit__overwriting'
TEST_CASE_8 = 'Propeht_OWL_Thing_without_Measure_without_Unit__no_overwriting'
DERIVATION_INPUTS_1 = [IRI_TO_FORECAST_1, FORECASTING_MODEL_1, 
                       FC_INTERVAL_1, FC_FREQUENCY_1, HIST_DURATION_1]
DERIVATION_INPUTS_2 = [IRI_TO_FORECAST_2, FORECASTING_MODEL_1,
                       FC_INTERVAL_1, FC_FREQUENCY_1, HIST_DURATION_1]
DERIVATION_INPUTS_3 = [IRI_TO_FORECAST_1, FORECASTING_MODEL_1, 
                       FC_INTERVAL_1, FC_FREQUENCY_1, HIST_DURATION_2]
DERIVATION_INPUTS_4 = [IRI_TO_FORECAST_3, FORECASTING_MODEL_1,
                       FC_INTERVAL_1, FC_FREQUENCY_1, HIST_DURATION_2]
# TFT test cases (require long data history/duration to meaningfully scale data)
TEST_CASE_9 = 'TFT_OM_Quantity_with_Measure_with_Unit__overwriting'
TEST_CASE_10 = 'TFT_OM_Quantity_with_Measure_with_Unit__no_overwriting'
TEST_CASE_11 = 'TFT_OM_Quantity_without_Measure_with_Unit__overwriting'
TEST_CASE_12 = 'TFT_OM_Quantity_without_Measure_with_Unit__no_overwriting'
# Prophet with covariates test cases
TEST_CASE_13 = 'Prophet_2_Covariates_OM_Quantity_without_scaling_without_Measure_without_Unit__overwriting'
TEST_CASE_14 = 'Prophet_2_Covariates_OM_Quantity_with_scaling_without_Measure_without_Unit__overwriting'
TEST_CASE_15 = 'Prophet_1_Covariates_OM_Quantity_without_scaling_without_Measure_without_Unit__overwriting'
TEST_CASE_16 = 'Prophet_1_Covariates_OM_Quantity_with_scaling_without_Measure_without_Unit__overwriting'
TEST_CASE_17 = 'Prophet_2_Covariates_OM_Quantity_without_scaling_without_Measure_without_Unit__overwriting_comparison'
# Test cases for multiple dataIRIs being linked to same time series IRI (i.e., different columns)
TEST_CASE_18 = 'Prophet_OM_Quantity_with_multiple_dataIRIs_associated_with_same_tsIRI'
TEST_CASE_19 = 'Prophet_OWL_Thing_with_multiple_dataIRIs_associated_with_same_tsIRI'
DERIVATION_INPUTS_5 = [IRI_TO_FORECAST_1, FORECASTING_MODEL_2,
                       FC_INTERVAL_1, FC_FREQUENCY_1, HIST_DURATION_2]
DERIVATION_INPUTS_6 = [IRI_TO_FORECAST_2, FORECASTING_MODEL_2,
                       FC_INTERVAL_1, FC_FREQUENCY_1, HIST_DURATION_2]
DERIVATION_INPUTS_7 = [IRI_TO_FORECAST_1, FORECASTING_MODEL_3,
                       FC_INTERVAL_1, FC_FREQUENCY_1, HIST_DURATION_2]
DERIVATION_INPUTS_8 = [IRI_TO_FORECAST_1, FORECASTING_MODEL_4,
                       FC_INTERVAL_1, FC_FREQUENCY_1, HIST_DURATION_2]
DERIVATION_INPUTS_9 = [IRI_TO_FORECAST_1, FORECASTING_MODEL_5,
                       FC_INTERVAL_1, FC_FREQUENCY_1, HIST_DURATION_2]
DERIVATION_INPUTS_10 = [IRI_TO_FORECAST_1, FORECASTING_MODEL_6,
                       FC_INTERVAL_1, FC_FREQUENCY_1, HIST_DURATION_2]
COVARIATES_1 = [AIRTEMPERATURE_1, ISHOLIDAY_1]
COVARIATES_2 = [PROMOTION_1, SPECIALEVENT_1]
COVARIATES_3 = [PROMOTION_1]

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
        'dataIRI_target': None,
        'dataIRI_fc' : None } }
# Define erroneous HTTP requests for forecast error evaluation
ERRONEOUS_ERROR_REQUEST_1 = {'quer': {}}
ERRONEOUS_ERROR_REQUEST_2 = {'query': {
        'dataIRI_target': 'https://www.theworldavatar.com/kg/...' } }
ERRONEOUS_ERROR_REQUEST_3 = {'query': {
        'dataIRI_target': 'https://www.theworldavatar.com/kg/ontotimeseries/Timeseries_1',
        'dataIRI_fc' : 'https://www.theworldavatar.com/kg/ontotimeseries/Timeseries_2' } }

# Expected error messages
ERRONEOUS_FORECAST_MSG_1 = "No 'ForecastingModel' IRI provided"
ERRONEOUS_FORECAST_MSG_2 = "More than one 'Duration' IRI provided"
ERRONEOUS_FORECAST_MSG_3 = "Neither 'om:Quantity' nor 'owl:Thing' IRI provided to forecast"
ERRONEOUS_FORECAST_MSG_4 = "More than one 'om:Quantity' IRI provided to forecast"
ERRONEOUS_FORECAST_MSG_5 = "No unique 'owl:Thing' IRI provided to forecast"
ERRONEOUS_ERROR_MSG_1 = "No 'query' node provided in HTTP request"
ERRONEOUS_ERROR_MSG_2 = "Unable to extract time series data IRIs to evaluate"
ERRONEOUS_ERROR_MSG_3 = "Unable to retrieve time series data"


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
            ontoagent_http_url=None,
            overwrite_forecast:bool=True
            ):
        agent = ForecastingAgent(
            register_agent=register_agent,
            agent_iri=ontoagent_service_iri if ontoagent_service_iri else 
                      os.getenv('ONTOAGENT_SERVICE_IRI'),
            agent_endpoint=ontoagent_http_url if ontoagent_http_url else
                           os.getenv('ONTOAGENT_OPERATION_HTTP_URL'),
            overwrite_fc=overwrite_forecast,
            # Settings which don't really matter for dockerised testing (as settings of
            # already running dockerised agents will be used once this one gets registered)
            # NOTE: only relevant to allow for unit testing with "fake" agent
            time_interval=int(os.getenv('DERIVATION_PERIODIC_TIMESCALE')),
            derivation_instance_base_url=os.getenv('DERIVATION_INSTANCE_BASE_URL'),
            kg_url=SPARQL_QUERY_ENDPOINT,
            kg_update_url=SPARQL_UPDATE_ENDPOINT,
            round_fc=ROUNDING,
            app=Flask(__name__),
            logger_name='dev'
        )
        return agent
    return _create_example_agent


# ----------------------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------------------

def generate_bounded_random_walk(initial_value, num_values, step_size_mean, 
                                 step_size_std, value_min, value_max):
    random_walk = [initial_value]
    for _ in range(1, num_values):
        step = np.random.normal(step_size_mean, step_size_std)
        next_value = random_walk[-1] + step
        next_value = max(value_min, min(value_max, next_value))  # Apply value constraints
        random_walk.append(round(next_value,2))
    return random_walk


def assess_forecast_error(data_iri, forecast_iri, ts_client, agent_url, 
                          name='test'):
    """
    Assess the error of a derived forecast and plot both time series
    NOTE: Plots are saved to the 'test_plots' folder in 'tests' volume as 
          Docker test container does not have GUIs
    """

    # 1) Create forecast plot for visual inspection
    # Retrieve time series data and create a DataFrame
    ts1 = ts_client.retrieve_timeseries(data_iri)
    ts2 = ts_client.retrieve_timeseries(forecast_iri)
    df1 = pd.DataFrame({'timestamp': ts1[0], 'actual data': ts1[1]})
    df2 = pd.DataFrame({'timestamp': ts2[0], 'forecast': ts2[1]})
    # Convert 'timestamp' column to a datetime data type
    df1['timestamp'] = pd.to_datetime(df1['timestamp'])
    df1.set_index('timestamp', inplace=True)
    df2['timestamp'] = pd.to_datetime(df2['timestamp'])
    df2.set_index('timestamp', inplace=True)
    # Merge DataFrames (while keeping all historical data) and slice to relevant period
    df = pd.merge(df1, df2, on='timestamp', how='outer')
    offset1 = pd.DateOffset(days=3)
    offset2 = pd.DateOffset(days=1)
    valid_indices = (df.index - offset2 <= df2.index.max()) & (df.index + offset1 >= df2.index.min())
    valid_entries = df[valid_indices]

    # Create new figure, plot and save to volume
    ax = valid_entries.plot()
    ax.set_xlabel('Timestamp')
    ax.set_ylabel('Values')
    ax.set_title(name)
    ax.grid(which='minor', axis='both')
    fp = '/app/tests/test_plots/' + name + '.png'
    plt.savefig(fp)

    # 2) Calculate forecast errors
    http_request = ERROR_REQUEST.copy()
    http_request['query']['dataIRI_target'] = data_iri
    http_request['query']['dataIRI_fc'] = forecast_iri
    # Create HTTP request to evaluate forecast errors
    headers = {'Content-Type': 'application/json'}
    agent_base_url = agent_url[:agent_url.rfind('/')+1]
    url = agent_base_url + '/evaluate_errors'
    response = requests.post(url, json=http_request, headers=headers)

    return response.json()


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


def update_derivation_interval(derivation_iri: str, interval_iri: str, sparql_client):
    # Replace the derivation's forecast interval with a new interval
    update = f"""
        DELETE {{ 
            ?deriv_iri <{ONTODERIVATION_ISDERIVEDFROM}> ?interval . 
        }} INSERT {{
            ?deriv_iri  <{ONTODERIVATION_ISDERIVEDFROM}> <{interval_iri}> . 
        }} WHERE {{
            VALUES ?deriv_iri {{ <{derivation_iri}> }}
            ?deriv_iri <{ONTODERIVATION_ISDERIVEDFROM}> ?interval .
            ?interval <{RDF_TYPE}> <{TIME_INTERVAL}> .
        }}
        """
    sparql_client.performUpdate(update)


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
