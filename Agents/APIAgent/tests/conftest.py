# This module provides all pytest fixtures and other utility functions for the
# actual (integration) tests

# Avoid unnecessary logging information from py4j package
import logging
import threading
import multiprocessing

from pyderivationagent import PyDerivationClient

logging.getLogger("py4j").setLevel(logging.INFO)

import os
import pytest
import requests
import time
import psycopg2 as pg
from flask import Flask
from pathlib import Path
from rdflib import Graph
from urllib.parse import urlparse

from data_classes.iris import *
from data_classes.ts_data_classes import TSTR_FORMATS
from api_agent.api_agent import APIAgent
from confs.apiagent_conf import *
from pyderivationagent.kg_operations import PySparqlClient
from werkzeug.serving import make_server

# ----------------------------------------------------------------------------------
# Constants and configuration
# ----------------------------------------------------------------------------------

PROJECT_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
TEST_TRIPLES_DIR = os.path.join(PROJECT_DIR, 'test_triples')
ENV_FILE = os.path.join(PROJECT_DIR, 'agent.env.test')
# Provide names of respective Docker services
# NOTE These names need to match the ones given in the `docker-compose-testcontainers.yml` file
KG_SERVICE = "blazegraph_test"
RDB_SERVICE = "postgres_test"

# Derivation agent IRIs
AGENT_IRI = 'http://www.example.com/resource/agents/Service__APIAgent/Service'
AGENT_URL = 'http://localhost:5000/api/'
# IRIs of derivation's (pure) inputs
TEST_TRIPLES_BASE_IRI = 'https://www.theworldavatar.com/kg/test/'
TIME_FORMAT = TSTR_FORMATS['Instant']

agent_config = config_api_agent(ENV_FILE)
DB_USER = agent_config.DB_USER
DB_PW = agent_config.DB_PW
SPARQL_QUERY_ENDPOINT = agent_config.SPARQL_QUERY_ENDPOINT
SPARQL_UPDATE_ENDPOINT = agent_config.SPARQL_UPDATE_ENDPOINT
DB_URL = agent_config.DB_URL
DERIVATION_INSTANCE_BASE_URL = agent_config.DERIVATION_INSTANCE_BASE_URL
# 
#  Values which should not require changing
#


# ----------------------------------------------------------------------------------
#  Test Inputs
# ----------------------------------------------------------------------------------
ABOX_TRIPLES = 61
AGENT_SERVICE_TRIPLES = 14
TIME_TRIPLES_PER_PURE_INPUT = 6
CREATED_TS_TRIPLES = 5
TEST_INPUT1 = [TEST_TRIPLES_BASE_IRI + 'TSMap_dwellingunit']
TEST_INPUT2 = [TEST_TRIPLES_BASE_IRI + 'TSMap_pvcapacity']
TEST_INPUT3 = [TEST_TRIPLES_BASE_IRI + 'TSMap_temperature']
TEST_OUTPUT1 = (
['2000-01-01T00:00:00Z', '2001-01-01T00:00:00Z', '2002-01-01T00:00:00Z', '2003-01-01T00:00:00Z', '2004-01-01T00:00:00Z',
 '2005-01-01T00:00:00Z', '2006-01-01T00:00:00Z', '2007-01-01T00:00:00Z', '2008-01-01T00:00:00Z', '2009-01-01T00:00:00Z',
 '2010-01-01T00:00:00Z', '2011-01-01T00:00:00Z', '2012-01-01T00:00:00Z', '2013-01-01T00:00:00Z', '2014-01-01T00:00:00Z',
 '2015-01-01T00:00:00Z', '2016-01-01T00:00:00Z', '2017-01-01T00:00:00Z', '2018-01-01T00:00:00Z', '2019-01-01T00:00:00Z',
 '2020-01-01T00:00:00Z', '2021-01-01T00:00:00Z', '2022-01-01T00:00:00Z', '2023-01-01T00:00:00Z'],
[1033961.0, 1060956.0, 1080728.0, 1093351.0, 1106876.0, 1119892.0, 1125784.0, 1128485.0, 1139561.0, 1147452.0,
 1164906.0, 1187110.0, 1216217.0, 1231914.0, 1271109.0, 1322898.0, 1370319.0, 1421302.0, 1460968.0, 1477627.0,
 1490946.0, 1507505.0, 1526540.0, 1563641.0])
TEST_OUTPUT2 = (
['2008-01-01T00:00:00Z', '2009-01-01T00:00:00Z', '2010-01-01T00:00:00Z', '2011-01-01T00:00:00Z', '2012-01-01T00:00:00Z',
 '2013-01-01T00:00:00Z', '2014-01-01T00:00:00Z', '2015-01-01T00:00:00Z', '2016-01-01T00:00:00Z', '2017-01-01T00:00:00Z',
 '2018-01-01T00:00:00Z', '2019-01-01T00:00:00Z', '2020-01-01T00:00:00Z', '2021-01-01T00:00:00Z', '2022-01-01T00:00:00Z',
 '2023-01-01T00:00:00Z'],
[0.36192499999999994, 1.9408750000000003, 3.7974600000000005, 5.9231084, 9.9738944, 15.5148234, 33.1401714,
 59.613699399999994, 125.79220339999999, 151.08575439999998, 209.82879940000004, 352.6970454, 431.1686204,
 631.7530194000001, 822.0080454, 1005.6916454000001])
TEST_OUTPUT3 = (
['1982-01-01T00:00:00Z', '1982-02-01T00:00:00Z', '1982-03-01T00:00:00Z', '1982-04-01T00:00:00Z', '1982-05-01T00:00:00Z']
, [25.9, 27.1, 27.2, 27.0, 28.0])


# ----------------------------------------------------------------------------------
# Session-scoped test fixtures
# (i.e. the fixture is destroyed at the end of the test session)
# ----------------------------------------------------------------------------------
class ServerThread(threading.Thread):

    def __init__(self, app):
        threading.Thread.__init__(self)
        self.server = make_server('127.0.0.1', 5000, app)
        self.ctx = app.app_context()
        self.ctx.push()

    def run(self):
        self.server.serve_forever()

    def shutdown(self):
        self.server.shutdown()


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
                                  user=DB_USER, password=DB_PW,
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
    print(bg)
    sparql_endpoint = get_blazegraph_service_url(KG_SERVICE, hostname=host,
                                                 url_route=path)
    # Create SparqlClient for testing
    kg_client = PySparqlClient(sparql_endpoint, sparql_endpoint)

    # Retrieve endpoint for postgres
    db = DB_URL[DB_URL.rfind('/') + 1:]
    rdb_url = get_postgres_service_url(RDB_SERVICE, hostname=host,
                                       database_name=db)

    # Create DerivationClient for creating derivation instances
    derivation_client = PyDerivationClient(TEST_TRIPLES_BASE_IRI, sparql_endpoint, sparql_endpoint)

    yield kg_client, derivation_client, rdb_url

    # Clear logger at the end of the test
    clear_loggers()


# ----------------------------------------------------------------------------------
# Module-scoped test fixtures
# (i.e. the fixture is destroyed during teardown of the last test in the module)
# ----------------------------------------------------------------------------------

@pytest.fixture
def create_example_agent():
    def _create_example_agent(
            ontoagent_service_iri=None,
            ontoagent_http_url=None
    ):
        print('creating test api agent')
        app1 = Flask(__name__)
        agent = APIAgent(
            db_url=agent_config.DB_URL,
            db_user=agent_config.DB_USER,
            db_pw=agent_config.DB_PW,
            agent_iri=ontoagent_service_iri,
            time_interval=agent_config.DERIVATION_PERIODIC_TIMESCALE,
            derivation_instance_base_url=agent_config.DERIVATION_INSTANCE_BASE_URL,
            kg_url=agent_config.SPARQL_QUERY_ENDPOINT,
            kg_update_url=agent_config.SPARQL_QUERY_ENDPOINT,
            kg_user=agent_config.KG_USERNAME,
            kg_password=agent_config.KG_PASSWORD,
            agent_endpoint=ontoagent_http_url,
            register_agent=agent_config.REGISTER_AGENT,
            app=app1
        )
        print('agent created')
        app = agent.app
        agent.add_url_pattern('/', 'root', default, methods=['GET'])  # Add root web to be introduction page
        print(app)
        agent.start_all_periodical_job()
        thread = ServerThread(app)
        thread.start()
        return agent, thread

    return _create_example_agent


def default():
    """
        Instructional message at the app root.
    """
    msg = "This is a derivation agent that serve as an example of pyderivationagent package.<BR>"
    msg += "For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/DerivationAgentPythonExample#readme<BR>"
    return msg


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
        cur = conn.cursor()
        sql_query = """
            DROP SCHEMA public CASCADE;
            CREATE SCHEMA public;
        """
        cur.execute(sql_query)


def get_number_of_rdb_tables(rdb_url):
    # Returns total number of tables in given database
    with connect_to_rdb(rdb_url) as conn:
        cur = conn.cursor()
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
    db = rdb_url[rdb_url.rfind('/') + 1:]
    return pg.connect(host=host, port=port, database=db,
                      user=DB_USER, password=DB_PW)


# method adopted from https://github.com/pytest-dev/pytest/issues/5502#issuecomment-647157873
def clear_loggers():
    """Remove handlers from all loggers"""
    import logging
    loggers = [logging.getLogger()] + list(logging.Logger.manager.loggerDict.values())
    for logger in loggers:
        handlers = getattr(logger, 'handlers', [])
        for handler in handlers:
            logger.removeHandler(handler)
