###############################################
# Authors: Markus Hofmeister (mh807cam.ac.uk) #    
# Date: 04 Apr 2022                           #
###############################################

# This module provides helper functions for various tests

import os
import json
import requests
from pathlib import Path

from metoffice.kgutils.kgclient import KGClient


def get_sparql_endpoint(docker_container):
    # Retrieve SPARQL endpoint for temporary testcontainer
    # endpoint acts as both Query and Update endpoint
    endpoint = 'http://' + docker_container.get_container_host_ip().replace('localnpipe', 'localhost') + ':' \
               + docker_container.get_exposed_port(9999)
    # 'kb' is default namespace in Blazegraph
    endpoint += '/blazegraph/namespace/test_kb/sparql'
    return endpoint


def get_number_of_triples(query_endpoint):
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
    # Files fp1 is as retrieved from DataPoint API
    fp1 = os.path.join(Path(__file__).parent, "data", "observation_readings.json")
    # File fp2 is as retrieved from DataPoint; however for station 25
    # 1) 1 reported quantity has been removed completely for all time steps
    # 2) 1 reported quantity has been removed except for 1 time step
    fp2 = os.path.join(Path(__file__).parent, "data", "forecast_readings1.json")
    # File fp3 is basically a copy of fp2 as initially retrieved
    # however a few missing data entry (-99) are included for station 25
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
