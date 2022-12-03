# This module provides all pytest fixtures and other utility functions for the
# actual integration tests

from pathlib import Path
from rdflib import Graph
from flask import Flask
import requests
import pytest
import time
import os

# Import mocked modules for all stack interactions (see `tests\__init__.py` for details)
from tests.mockutils.stack_configs_mock import QUERY_ENDPOINT, UPDATE_ENDPOINT
from tests.mockutils.env_configs_mock import HOSTNAME

from pyderivationagent.data_model.iris import ONTODERIVATION_BELONGSTO, ONTODERIVATION_ISDERIVEDFROM, \
                                              TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION, \
                                              ONTODERIVATION_DERIVATIONASYN, ONTODERIVATION_HASSTATUS, \
                                              ONTODERIVATION_ERROR
from pyderivationagent.conf import config_derivation_agent
from pyderivationagent import PyDerivationClient

from floodassessment.datamodel.iris import *
from floodassessment.agent import FloodAssessmentAgent
from floodassessment.kg_operations.kgclient import KGClient


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
# DATABASE, NAMESPACE, DB_USER, DB_PASSWORD environment variables in the respective files:
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

# Define input sets to test
DERIVATION_INPUTS_1 = [FLOOD_WARNING_1,
                       BUILDING_1, BUILDING_2, BUILDING_3, BUILDING_4,
                       MARKET_VALUE_1, MARKET_VALUE_2, MARKET_VALUE_3]
DERIVATION_INPUTS_2 = [FLOOD_WARNING_2,
                       BUILDING_1, BUILDING_2, BUILDING_3, BUILDING_4,
                       MARKET_VALUE_1, MARKET_VALUE_2, MARKET_VALUE_3]
DERIVATION_INPUTS_3 = [FLOOD_WARNING_2,
                       BUILDING_1, BUILDING_2,
                       MARKET_VALUE_1, MARKET_VALUE_2]
DERIVATION_INPUTS_4 = [FLOOD_WARNING_2,
                       BUILDING_1, BUILDING_2,
                       MARKET_VALUE_1, MARKET_VALUE_2, MARKET_VALUE_3]
DERIVATION_INPUTS_5 = [FLOOD_WARNING_2]

# Define expected output sets
DERIVATION_OUTPUTS_1 = [FLOOD_FLOOD, FLOOD_IMPACT, FLOOD_POPULATION, FLOOD_BUILDINGS,
                        OM_AMOUNT_MONEY, OM_AMOUNT_MONEY, OM_MEASURE, OM_MEASURE]
DERIVATION_OUTPUTS_2 = [FLOOD_FLOOD, FLOOD_IMPACT, FLOOD_BUILDINGS,
                        OM_AMOUNT_MONEY, OM_AMOUNT_MONEY, OM_MEASURE, OM_MEASURE]
DERIVATION_OUTPUTS_5 = [FLOOD_FLOOD, FLOOD_IMPACT, FLOOD_BUILDINGS]

# Test against pre-calculated value estimates from Excel (rounded)
# (Number of buildings at risk, value of building at risk, people at risk)
FLOOD_ASSESSMENT_1 = (0, 0, 0)          # Inactive flood alert
FLOOD_ASSESSMENT_2 = (4, 1000000, None) # Active flood alert with buildings at risk
FLOOD_ASSESSMENT_3 = (2, 700000, None)  # Active flood alert with buildings at risk
FLOOD_ASSESSMENT_5 = (0, None, None)    # Active flood alert without buildings at risk

EXCEPTION_STATUS_1 = "More value estimations than buildings provided"


# ----------------------------------------------------------------------------------
#  Inputs which should not be changed
#

# Expected number of triples
TBOX_TRIPLES = 25
ABOX_TRIPLES = 26
TIME_TRIPLES_PER_PURE_INPUT = 6
#NOTE: derivation status triples get removed again after successful derivation execution
DERIV_STATUS_TRIPLES = 2         # derivation status triples
AGENT_SERVICE_TRIPLES = 5        # agent service triples
DERIV_INPUT_TRIPLES = 2 + 3*3    # triples for derivation input message & parts:
                                 # 2 triples for message, 3 triples per input concept
DERIV_OUTPUT_TRIPLES = 2 + 3*3   # triples for derivation output message & parts:
                                 # 2 triples for message, 3 triples per output concept
# Flood assessment / derivation output triples
FLOOD_ASSESSMENT_GENERAL = 5+2     # general flood assessment markup (i.e., newly instantiated
                                   # FloodIRI, warnsAbout, number of affected buildings)
                                   # + 2 "belongs to" triples
FLOOD_ASSESSMENT_POPULATION = 3+1  # In case affected population is given +
                                   # + 1 "belongs to" triple
FLOOD_ASSESSMENT_BUILDINGS = 14+5  # In case value of affected buildings is given
                                   # + 5 "belongs to" triples
FLOOD_ASSESSMENT_DESCRIPTION = 1   # In case description is given

# Define expected number of derivation triples
DERIVATION_TRIPLES_1 = FLOOD_ASSESSMENT_GENERAL + FLOOD_ASSESSMENT_POPULATION + \
                       FLOOD_ASSESSMENT_BUILDINGS - DERIV_STATUS_TRIPLES
DERIVATION_TRIPLES_2 = FLOOD_ASSESSMENT_GENERAL + FLOOD_ASSESSMENT_BUILDINGS + \
                       FLOOD_ASSESSMENT_DESCRIPTION - DERIV_STATUS_TRIPLES
DERIVATION_TRIPLES_5 = FLOOD_ASSESSMENT_GENERAL + FLOOD_ASSESSMENT_DESCRIPTION - \
                       DERIV_STATUS_TRIPLES + 3 # triples to create Impact IRI incl. "belongsTo"


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


def get_flood_assessment_details(sparql_client, derivation_iri):
    # Returns details associated with Derivation instance
    query = f"""
        SELECT ?input_iri ?input_type ?population ?bldgs ?bldg_value ?bldg_unit
               ?impact_value ?impact_unit
        WHERE {{
            <{derivation_iri}> <{ONTODERIVATION_ISDERIVEDFROM}> ?input_iri . 
            ?input_iri <{RDF_TYPE}> ?input_type . 
            ?buildings_iri <{RDF_TYPE}> <{FLOOD_BUILDINGS}> ; 
                           <{ONTODERIVATION_BELONGSTO}> <{derivation_iri}> ; 
                           <{FLOOD_HAS_TOTAL_COUNT}> ?bldgs . 
            OPTIONAL {{
                ?buildings_iri <{FLOOD_HAS_TOTAL_MONETARY_VALUE}> ?building_value . 
                ?building_value <{OM_HAS_VALUE}> ?building_measure . 
                ?building_measure <{RDF_TYPE}> <{OM_MEASURE}> ; 
                                  <{OM_HAS_UNIT}>/<{OM_SYMBOL}> ?bldg_unit ; 
                                  <{OM_NUM_VALUE}> ?bldg_value . 
            }}
            OPTIONAL {{
                ?population_iri <{RDF_TYPE}> <{FLOOD_POPULATION}> ; 
                                <{ONTODERIVATION_BELONGSTO}> <{derivation_iri}> ; 
                                <{FLOOD_HAS_TOTAL_COUNT}> ?population . 
            }}
            OPTIONAL {{ 
                ?impact <{RDF_TYPE}> <{FLOOD_IMPACT}> ; 
                        <{ONTODERIVATION_BELONGSTO}> <{derivation_iri}> ; 
                        <{FLOOD_HAS_MONETARY_VALUE}> ?imp_value . 
                ?imp_value <{OM_HAS_VALUE}> ?imp_measure . 
                ?imp_measure <{RDF_TYPE}> <{OM_MEASURE}> ; 
                             <{OM_HAS_UNIT}>/<{OM_SYMBOL}> ?impact_unit ; 
                             <{OM_NUM_VALUE}> ?impact_value . 
            }}
        }}
        """
    response = sparql_client.performQuery(query)
    if len(response) == 0:
        return None
    else:
        # Derivation inputs (i.e. isDerivedFrom)
        key = set([x['input_type'] for x in response])
        inputs = {k: [x['input_iri'] for x in response if x['input_type'] == k] for k in key}
        # Extract impact, population and building details
        impacts = {'population': None, 
                   'bldgs': None,
                   'bldg_value': None,
                   'bldg_unit': None,
                   'impact_value': None,
                   'impact_unit': None
                   }
        for imp in impacts:
            # Extract value if present in response
            if 'unit' in imp:
                try:
                    impacts[imp] = list(set([str(x[imp]).encode('ISO-8859-1').decode('utf-8') for x in response]))
                except:
                    pass
            else:
                try:
                    impacts[imp] = list(set([float(x[imp]) for x in response]))
                except:
                    pass

        return inputs, impacts


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
