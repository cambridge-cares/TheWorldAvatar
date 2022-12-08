################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) # 
# Date: 08 Dec 2022                            #
################################################

# This module provides all pytest fixtures and other utility functions for the
# actual (integration) tests

import pytest
import requests
import time
import uuid
import numpy as np
import pandas as pd
import psycopg2 as pg

from forecasting.datamodel.iris import *
from forecasting.datamodel.data_mapping import *
from forecasting.flaskapp import create_app
from forecasting.utils.tools import *
from forecasting.kgutils.kgclient import KGClient
from forecasting.kgutils.tsclient import TSClient, init_ts

from forecasting.utils.env_configs import DB_USER, DB_PASSWORD


# ----------------------------------------------------------------------------------
# Constants and configuration
# ----------------------------------------------------------------------------------
# Values to be adjusted as needed:
#

# Provide names of respective Docker services
# NOTE These names need to match the ones given in the `docker-compose-test.yml` file
HOSTNAME = "localhost"
KG_SERVICE = "blazegraph_test"
KG_ROUTE = "blazegraph/namespace/kb/sparql"
RDB_SERVICE = "postgres_test"
DATABASE = 'postgres'

# Links to pre-trained Darts TFT model
DARTS_MODEL_OBJECT = "https://www.dropbox.com/s/fxt3iztbimvm47s/best.ckpt?dl=1"
DARTS_CHECKPOINTS = "https://www.dropbox.com/s/ntg8lgvh01x09wr/_model.pth.tar?dl=1"

# Length of test time series to generate (i.e. number of time steps)
PERIODS = 400


# ----------------------------------------------------------------------------------
#  Inputs which should not be changed
#
GENERATED_HEAT_IRI = KB + "GeneratedHeat_" + str(uuid.uuid4())
GENERATED_HEAT_DATAIRI = KB + 'Measure_' + str(uuid.uuid4())
TIMESTAMPS = pd.date_range(start='2019-08-05T09:00:00Z', periods=PERIODS,
                           freq='H').strftime(TIME_FORMAT)
FORECAST_START = (pd.to_datetime(TIMESTAMPS[-1]) + pd.Timedelta('1 hour')).strftime(TIME_FORMAT)


QUERY1 = {"query": {
          "forecast_start_date": FORECAST_START,
          "iri": GENERATED_HEAT_IRI,
          "data_length": 168,
          "horizon": 3,
          "use_model_configuration": "DEFAULT"
        }}
EXPECTED1 = {'fc_model': {'train_again': True, 'name': 'prophet', 'scale_data': False, 'input_length': QUERY1['query']['data_length']},
             'data_length': QUERY1['query']['data_length'],
             'model_configuration_name': QUERY1['query']['use_model_configuration'],
             'iri': QUERY1['query']['iri'],
             'horizon': QUERY1['query']['horizon'],
             'forecast_start_date': FORECAST_START,
             'model_input_interval': ['Thu, 15 Aug 2019 01:00:00 GMT', 'Thu, 22 Aug 2019 00:00:00 GMT'],
             'model_output_interval': ['Thu, 22 Aug 2019 01:00:00 GMT', 'Thu, 22 Aug 2019 03:00:00 GMT'],
             'unit': OM_MEGAWATTHOUR,
            }

QUERY2 = {"query": {
          "iri": GENERATED_HEAT_IRI,
          "data_length": 168,
          "horizon": 3
        }}
EXPECTED2 = {'fc_model': {'train_again': True, 'name': 'prophet', 'scale_data': False, 'input_length': QUERY2['query']['data_length']},
             'data_length': QUERY2['query']['data_length'],
             'model_configuration_name': 'DEFAULT',
             'iri': QUERY2['query']['iri'],
             'horizon': QUERY2['query']['horizon'],
             'forecast_start_date': FORECAST_START,
             'model_input_interval': ['Thu, 15 Aug 2019 01:00:00 GMT', 'Thu, 22 Aug 2019 00:00:00 GMT'],
             'model_output_interval': ['Thu, 22 Aug 2019 01:00:00 GMT', 'Thu, 22 Aug 2019 03:00:00 GMT'],
             'unit': OM_MEGAWATTHOUR,
             'time_format': TIME_FORMAT_TS
            }

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
    # Retrieve "user-facing" endpoints for all dockerised testing services
    
    # Retrieve endpoint for triple store
    sparql_endpoint = get_blazegraph_service_url(KG_SERVICE, url_route=KG_ROUTE)

    # Retrieve endpoint for postgres
    rdb_url = get_postgres_service_url(RDB_SERVICE, url_route=DATABASE)

    # Create SparqlClient for testing
    kg_client = KGClient(sparql_endpoint, sparql_endpoint)

    # Create TimeSeriesClient for testing
    ts_client = TSClient(kg_client=kg_client, rdb_url=rdb_url, 
                         rdb_user=DB_USER, rdb_password=DB_PASSWORD)

    yield kg_client, ts_client, rdb_url

    # Clear logger at the end of the test
    clear_loggers()


# ----------------------------------------------------------------------------------
# Module-scoped test fixtures
# (i.e. the fixture is destroyed during teardown of the last test in the module)
# ----------------------------------------------------------------------------------

# Create Flask App
@pytest.fixture(scope='module')
def test_client():
    flask_app = create_app()
    # Create a test client using the Flask application configured for testing
    with flask_app.test_client() as testing_client:
        # Establish an application context
        with flask_app.app_context():
            # This is where the testing happens!
            yield testing_client  


# ----------------------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------------------


def initialise_prophet(kgClient, tsClient, rdb_url):
    """
    Initialise Blazegrph and PostgreSQL with test data for Prophet test
    """

    # Clear Blazegraph and PostgreSQL
    clear_database(rdb_url)
    clear_triplestore(kgClient)

    # Verify that Triplestore and RDB are empty
    assert get_number_of_rdb_tables(rdb_url) == 0
    assert get_number_of_triples(kgClient) == 0

    # Initialise SPARQL update
    update = ''

    test_data = pd.DataFrame({
        'timestamp': TIMESTAMPS,
        'generatedHeat': np.random.rand(PERIODS)})

    update += get_properties_for_subj(subj=GENERATED_HEAT_DATAIRI, 
                                      verb_obj={RDF_TYPE: OM_MEASURE,
                                                OM_HASUNIT: OM_MEGAWATTHOUR
                                            })
    update += get_properties_for_subj(subj=GENERATED_HEAT_IRI, 
                                      verb_obj={RDF_TYPE: OHN_GENERATEDHEATAMOUNT,
                                                OM_HASVALUE: GENERATED_HEAT_DATAIRI
                                            })
    # Convert to proper format
    update = add_insert_data(update)

    # Execute query
    kgClient.performUpdate(update)

    # Initialise time series and add data
    init_ts(GENERATED_HEAT_DATAIRI, test_data['timestamp'], test_data['generatedHeat'], 
            tsClient, ts_type=DOUBLE, time_format=TIME_FORMAT_TS)


def clear_triplestore(kgClient):
    # Delete all triples
    query_delete = """
                    DELETE WHERE {?s ?p ?o}
                    """
    kgClient.performUpdate(query_delete)


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


def get_number_of_triples(kgClient):
    # Count all triples
    query = """
            SELECT (count(*) as ?count)
            WHERE { ?s ?p ?o }
            """
    res = kgClient.performQuery(query)
    return int(res[0]['count'])

def connect_to_rdb(rdb_url):
        # Retrieve host and port from RDB URL assuming default format like
        # jdbc:postgresql://localhost:5432/<url_route>
        host = rdb_url.split(':')[2].replace('//', '')
        port = rdb_url.split(':')[3].split('/')[0]
        return pg.connect(host=host, port=port, database=DATABASE,
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
