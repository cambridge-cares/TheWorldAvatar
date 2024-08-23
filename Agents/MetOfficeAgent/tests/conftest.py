################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 04 Apr 2022                            #
################################################

# This module provides fixtures and helper functions for various tests

# Avoid unnecessary logging information from py4j package
import logging
logging.getLogger("py4j").setLevel(logging.INFO)

import os
import json
import time
import requests
import pytest
import psycopg2 as pg
from pathlib import Path
from testcontainers.core.container import DockerContainer

from agent.flaskapp import create_app
from agent.kgutils.kgclient import KGClient
from agent.utils.stack_configs import QUERY_ENDPOINT, UPDATE_ENDPOINT, \
                                      DB_URL, DB_USER, DB_PASSWORD


# ----------------------------------------------------------------------------------
# Fixtures
# ----------------------------------------------------------------------------------

@pytest.fixture()
def initialise_triple_store():
    # Define temporary Docker container based on empty Blazegraph image from CMCL registry
    blazegraph = DockerContainer('docker.cmclinnovations.com/blazegraph:1.0.0-SNAPSHOT')
    blazegraph.with_exposed_ports(8080)    
    # Clear logger at the end of the test
    clear_loggers()    
    yield blazegraph


@pytest.fixture()
def create_testing_agent():
    app = create_app({'TESTING': True})
    with app.test_client() as client:
        yield client


@pytest.fixture()
def clear_triple_store():
    # Delete all triples from triple store (to ensure that tests are independent)
    kg_client = KGClient(QUERY_ENDPOINT, UPDATE_ENDPOINT)
    query_string = \
        """
        DELETE WHERE {?s ?p ?o}
        """
    kg_client.performUpdate(query_string)



@pytest.fixture()
def clear_database():
    # Delete all tables from database (to ensure that tests are independent)
    # 1) Ensure access to database
    service_available = False
    while not service_available:
        try:
            # Retrieve host, port, and database from RDB URL
            host = DB_URL.split(':')[2].replace('//', '')
            port = DB_URL.split(':')[3].split('/')[0]
            database = DB_URL.split('/')[-1]
            conn = pg.connect(host=host, port=port, database=database,
                              user=DB_USER, password=DB_PASSWORD)
            if conn.status == pg.extensions.STATUS_READY:
                service_available = True
        except Exception:
            time.sleep(3)
    
    # 2) Drop all tables
    with conn:
        cur=conn.cursor()
        sql_query = """
            DROP SCHEMA public CASCADE;
            CREATE SCHEMA public;
        """
        cur.execute(sql_query)


# ----------------------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------------------

def get_sparql_endpoint(docker_container):
    # Retrieve SPARQL endpoint for temporary testcontainer
    # (endpoint acts as both Query and Update endpoint)
    endpoint = 'http://' + docker_container.get_container_host_ip().replace('localnpipe', 'localhost') + ':' \
               + docker_container.get_exposed_port(8080)
    # 'kb' is default namespace in Blazegraph
    endpoint += '/blazegraph/namespace/kb/sparql'
    
    # Ensures that the requested Blazegraph Docker service is ready to accept SPARQL query/update
    service_available = False
    while not service_available:
        try:
            response = requests.head(endpoint)
            if response.status_code != requests.status_codes.codes.not_found:
                service_available = True
        except requests.exceptions.ConnectionError:
            time.sleep(3)
    
    return endpoint


def get_number_of_triples(query_endpoint=QUERY_ENDPOINT):
    # Construct KG client with correct query
    kg_client = KGClient(query_endpoint, query_endpoint)
    query_string = \
        """
        SELECT (count(*) as ?count)
        WHERE {?s ?p ?o}
        """
    # Execute query
    results = kg_client.performQuery(query=query_string)
    # Extract results
    res = int(results[0]['count'])

    return res


def read_station_data():
    fp = os.path.join(Path(__file__).parent, "data", "station_data.json")
    with open(fp, 'r') as file:
        stations = json.load(file)

    return stations


def read_readings_locations():
    # Files with location data for observations and forecasts
    fp1 = os.path.join(Path(__file__).parent, "data", "observation_locs.json")
    fp2 = os.path.join(Path(__file__).parent, "data", "forecast_locs.json")
    files = [fp1, fp2]
    data = []
    for f in files:
        with open(f, 'r') as file:
            readings = json.load(file)
            data.append(readings)

    return data


def read_readings_timeseries():
    # Observation readings as retrieved from DataPoint API
    fp1 = os.path.join(Path(__file__).parent, "data", "observation_readings.json")
    # Forecast readings as retrieved from DataPoint; however for station 25
    # 1) "AirTemperature"" has been removed completely for all time steps
    # 2) "RelativeHumidity"" has been removed completely except for 1st time step
    # 3) "WindSpeed" has been set to missing (-99) for time steps 4 & 5
    # 4) "Visibility" has been set to missing (-99) for time step 5
    fp2 = os.path.join(Path(__file__).parent, "data", "forecast_readings1.json")
    # Forecast readings as retrieved from DataPoint; however for station 25
    # 1) "WindSpeed" and "Visibility" again set to missing for very same time steps
    #    (otherwise previously missing data would get filled now)
    # 2) "RelativeHumidity"" still removed completely except for 1st time step
    # 3) "AirTemperature" now included in forecast readings
    fp3 = os.path.join(Path(__file__).parent, "data", "forecast_readings2.json")
    files = [fp1, fp2, fp3]
    data = []
    for f in files:
        with open(f, 'r') as file:
            readings = json.load(file)
            data.append(readings)

    return data


def create_blazegraph_namespace(endpoint):
    """
        Creates new Blazegraph namespace with name as specified in 'endpoint'
        and geospatial querying enabled
    """

    # Extract Blazegraph REST API url from SPARQL endpoint
    url = endpoint[:endpoint.find('namespace') + len('namespace')]

    # Extract name for new namespace from SPARQL endpoint
    ns = endpoint[endpoint.find('namespace') + len('namespace') + 1:]
    ns = ns[:ns.find('/')]

    # Define POST request header and payload
    header = {'Content-type': 'text/plain'}

    payload = 'com.bigdata.rdf.store.AbstractTripleStore.textIndex=false\r\n' \
              'com.bigdata.rdf.store.AbstractTripleStore.axiomsClass=com.bigdata.rdf.axioms.NoAxioms\r\n' \
              'com.bigdata.rdf.sail.isolatableIndices=false\r\n' \
              'com.bigdata.rdf.sail.truthMaintenance=false\r\n' \
              'com.bigdata.rdf.store.AbstractTripleStore.justify=false\r\n' \
              'com.bigdata.namespace.{}.spo.com.bigdata.btree.BTree.branchingFactor=1024\r\n' \
              'com.bigdata.rdf.sail.namespace={}\r\n' \
              'com.bigdata.rdf.store.AbstractTripleStore.quads=false\r\n' \
              'com.bigdata.namespace.{}.lex.com.bigdata.btree.BTree.branchingFactor=400\r\n' \
              'com.bigdata.rdf.store.AbstractTripleStore.geoSpatial=true\r\n' \
              'com.bigdata.rdf.store.AbstractTripleStore.statementIdentifiers=false'.format(ns, ns, ns)

    # Post the request
    response = requests.post(url, payload, headers=header)

    if response.status_code == 201:
        print('New namespace \"{}\" successfully created.\n'.format(ns))
    elif response.status_code == 409:
        print('Namespace \"{}\" already exists\n'.format(ns))
    else:
        print('Request status code: {}\n'.format(response.status_code))


# method adopted from https://github.com/pytest-dev/pytest/issues/5502#issuecomment-647157873
def clear_loggers():
    """Remove handlers from all loggers"""
    import logging
    loggers = [logging.getLogger()] + list(logging.Logger.manager.loggerDict.values())
    for logger in loggers:
        handlers = getattr(logger, 'handlers', [])
        for handler in handlers:
            logger.removeHandler(handler)
