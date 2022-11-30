# This module provides all pytest fixtures and other utility functions for the
# actual integration tests

from pathlib import Path
from rdflib import Graph
from flask import Flask
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

from floodassessment.datamodel.iris import *
from floodassessment.kg_operations.kgclient import KGClient
from floodassessment.agent import FloodAssessmentAgent


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

# Derivation markup
DERIVATION_INSTANCE_BASE_URL = config_derivation_agent(AGENT_ENV).DERIVATION_INSTANCE_BASE_URL
# IRIs of derivation's (pure) inputs
# NOTE Should be consistent with the ones in test_triples/example_abox.ttl
TEST_TRIPLES_BASE_IRI = 'https://www.example.com/kg/ontoflood/'

# Buildings
BUILDING_1 = TEST_TRIPLES_BASE_IRI + 'Building_1'
BUILDING_2 = TEST_TRIPLES_BASE_IRI + 'Building_2'
BUILDING_3 = TEST_TRIPLES_BASE_IRI + 'Building_3'
BUILDING_4 = TEST_TRIPLES_BASE_IRI + 'Building_4'
BUILDINGS = [BUILDING_1, BUILDING_2, BUILDING_3, BUILDING_4]
# Property market value estimations
MARKET_VALUE_1 = TEST_TRIPLES_BASE_IRI + 'MarketValue_1'
MARKET_VALUE_2 = TEST_TRIPLES_BASE_IRI + 'MarketValue_2'
MARKET_VALUE_3 = TEST_TRIPLES_BASE_IRI + 'MarketValue_3'
MARKET_VALUES = [MARKET_VALUE_1, MARKET_VALUE_2, MARKET_VALUE_3]
# Flood warnings
FLOOD_WARNING_1 = TEST_TRIPLES_BASE_IRI + 'FloodWarning_1'
FLOOD_WARNING_2 = TEST_TRIPLES_BASE_IRI + 'FloodWarning_2'
FLOOD_WARNINGS = [FLOOD_WARNING_1, FLOOD_WARNING_2]


# # PropertyPriceIndex
# PRICE_INDEX_INSTANCE_IRI = TEST_TRIPLES_BASE_IRI + 'PropertyPriceIndex_1'
# # AveragePricePerSqm
# AVERAGE_SQM_PRICE = TEST_TRIPLES_BASE_IRI + 'AveragePricePerSqm_1'
# # TransactionRecords
# TRANSACTION_INSTANCE_1_IRI = TEST_TRIPLES_BASE_IRI + 'Transaction_1'
# TRANSACTION_INSTANCE_2_IRI = TEST_TRIPLES_BASE_IRI + 'Transaction_2'
# TRANSACTION_INSTANCE_3_IRI = TEST_TRIPLES_BASE_IRI + 'Transaction_3'
# # FloorArea
# FLOOR_AREA_INSTANCE_1_IRI = TEST_TRIPLES_BASE_IRI + 'Area_1'
# FLOOR_AREA_INSTANCE_3_IRI = TEST_TRIPLES_BASE_IRI + 'Area_3'

# # Define input sets to test
# DERIVATION_INPUTS_1 = [PRICE_INDEX_INSTANCE_IRI, TRANSACTION_INSTANCE_1_IRI,
#                        AVERAGE_SQM_PRICE, FLOOR_AREA_INSTANCE_1_IRI]
# DERIVATION_INPUTS_2 = [PRICE_INDEX_INSTANCE_IRI, TRANSACTION_INSTANCE_1_IRI,
#                        AVERAGE_SQM_PRICE]
# DERIVATION_INPUTS_3 = [AVERAGE_SQM_PRICE, FLOOR_AREA_INSTANCE_3_IRI]
# DERIVATION_INPUTS_4 = [TRANSACTION_INSTANCE_2_IRI, TRANSACTION_INSTANCE_3_IRI]
# # Test against pre-calculated value estimates from Excel (rounded)
MARKET_VALUE_1 = 1000000
# MARKET_VALUE_2 = 938000
# EXCEPTION_STATUS_1 = "More than one 'TransactionRecord' IRI provided"

# ----------------------------------------------------------------------------------
#  Inputs which should not be changed
#

# Expected number of triples
TBOX_TRIPLES = 25
ABOX_TRIPLES = 26
# TIME_TRIPLES_PER_PURE_INPUT = 6
# DERIV_STATUS_TRIPLES = 2        # derivation status triples
# AGENT_SERVICE_TRIPLES = 5       # agent service triples
# DERIV_INPUT_TRIPLES = 2 + 4*3   # triples for derivation input message
# DERIV_OUTPUT_TRIPLES = 5        # triples for derivation output message
# MARKET_VALUE_TRIPLES = 7        # triples added by `instantiate_property_value`


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
def initialise_clients(get_blazegraph_service_url):
    # Retrieve "user-facing" endpoints for all clients/services relevant for testing
    # (i.e. invoked during testing from outside the Docker stack)
    # --> those shall be `localhost:...` even when agent is running as Docker container
    
    # Retrieve endpoint for triple store
    sparql_endpoint = get_blazegraph_service_url(KG_SERVICE, url_route=KG_ROUTE)

    # Create SparqlClient for testing
    sparql_client = KGClient(sparql_endpoint, sparql_endpoint)

    # Create DerivationClient for creating derivation instances
    derivation_client = PyDerivationClient(
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        query_endpoint=sparql_client.query_endpoint,
        update_endpoint=sparql_client.update_endpoint,
    )

    yield sparql_client, derivation_client

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
        agent = FloodAssessmentAgent(
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


# def get_marketvalue_details(sparql_client, market_value_iri):
#     # Returns details associated with Market Value instance (om:AmountOfMoney)
#     query = f"""
#         SELECT ?value ?unit ?input_iri ?input_type
#         WHERE {{
#         <{market_value_iri}> <{RDF_TYPE}> <{OM_AMOUNT_MONEY}> ; 
#                              <{OM_HAS_VALUE}> ?measure ; 
#                              <{ONTODERIVATION_BELONGSTO}>/<{ONTODERIVATION_ISDERIVEDFROM}> ?input_iri . 
#         ?measure <{RDF_TYPE}> <{OM_MEASURE}> ; 
#                  <{OM_HAS_UNIT}>/<{OM_SYMBOL}> ?unit ; 
#                  <{OM_NUM_VALUE}> ?value . 
#         ?input_iri <{RDF_TYPE}> ?input_type . 
#         }}
#         """
#     response = sparql_client.performQuery(query)
#     if len(response) == 0:
#         return None
#     else:
#         # Derivation inputs (i.e. isDerivedFrom)
#         key = set([x['input_type'] for x in response])
#         inputs = {k: [x['input_iri'] for x in response if x['input_type'] == k] for k in key}
#         # Market Value and monetary unit
#         market_value = list(set([float(x['value']) for x in response]))
#         # NOTE: Fix encoding issue with pound sterling
#         unit = list(set([str(x['unit']).encode('ISO-8859-1').decode('utf-8') for x in response]))
#         return inputs, market_value, unit


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
