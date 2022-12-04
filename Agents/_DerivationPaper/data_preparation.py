# This script provides functionality to prepare and instantiate all data required
# for the minimum viable product of the King's Lynn use case for the derivation paper

import os
import requests
import pandas as pd
from pathlib import Path

from iris import *
from configs import SPARQL_QUERY_ENDPOINT, SPARQL_UPDATE_ENDPOINT
from property_price_index import initialise_ukhpi

from pyderivationagent.kg_operations import PySparqlClient

# REQUIRED FILES
# Specify name of n-triples file with consolidated property data (or consolidated property data with labels)
triples_file = '20221203_consolidated_and_labeled_properties.nt'

# Specify name of building location csv to be extracted from KG
bldg_loc = 'building_locations.csv'
# Specify name of csv with affected properties (determined using QGIS)
affected = 'affected_property_iris.csv'

# Specify label to be attached to all properties within flood polygon
label = 'affected'


def create_blazegraph_namespace(endpoint):
    """
    Creates Blazegraph namespace with name as specified in SPARQL update endpoint
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
              'com.bigdata.rdf.store.AbstractTripleStore.quads=False\r\n' \
              'com.bigdata.namespace.{}.lex.com.bigdata.btree.BTree.branchingFactor=400\r\n' \
              'com.bigdata.rdf.store.AbstractTripleStore.geoSpatial=False\r\n' \
              'com.bigdata.rdf.store.AbstractTripleStore.statementIdentifiers=false'.format(ns, ns, ns)

    # Post the request
    response = requests.post(url, payload, headers=header)

    if response.status_code == 201:
        print('New namespace \"{}\" successfully created.\n'.format(ns))
    elif response.status_code == 409:
        print('Namespace \"{}\" already exists\n'.format(ns))
    else:
        print('Request status code: {}\n'.format(response.status_code))


def insert_pound_sterling_symbols():
    # Return query to instantiate pound sterling symbols
    query = f"""
    INSERT DATA {{
        <{UOM_GBP_M2}> <{OM_SYMBOL}> \"{GBP_PER_SM}\"^^<{XSD_STRING}> . 
        <{OM_GBP}> <{OM_SYMBOL}> \"{GBP_SYMBOL}\"^^<{XSD_STRING}> . 
        }}
    """
    return query


def extract_property_locations(kg_client, output_file):
    """
    Extracts all property locations from the KG and writes them to a csv file
    """
    # Retrieve query
    query_file = os.path.join(Path(__file__).parent, 'resources', 'property_location.sparql')
    with open(query_file, 'r') as f:
        query = f.read()
    
    # Execute query and retrieve results
    results = kg_client.performQuery(query)
    df = pd.DataFrame(results)
    # TODO encountered below warning when running this script as main
    # FutureWarning: In a future version of pandas all arguments of StringMethods.split except for the argument 'pat' will be keyword-only.
    df[['latitude', 'longitude']] = df['location'].str.split('#', 1, expand=True)

    # Write results to csv file
    df.drop(columns=['location']).to_csv(output_file, index=False)


def attach_labels(kg_client, label, input_csv):
    """
    Extracts all property locations from the KG and writes them to a csv file
    """
    # Extract IRIs from csv file
    with open(input_csv, 'r') as f:
        iris = f.read()
    iris = iris.split('\n')
    iris = iris[1:-1]
    
    # Construct INSERT query
    query = [f'<{iri}> <{RDFS_LABEL}> \"{label}\"^^<{XSD_STRING}>' for iri in iris]
    query = 'INSERT DATA {{ {} }}'.format(' . '.join(query))

    # Execute query and retrieve results
    kg_client.performUpdate(query)


if __name__ == '__main__':

    # 1) Instantiate consolidated triples file
    # Ensure Blazegraph namespace exists
    create_blazegraph_namespace(SPARQL_UPDATE_ENDPOINT)

    # Initialise KG client
    kg_client = PySparqlClient(query_endpoint=SPARQL_QUERY_ENDPOINT,
                               update_endpoint=SPARQL_UPDATE_ENDPOINT)

    # Upload n-triples file (use `uploadOntolgy` method of pyderivation sparql_client 
    # directly which ensures file is converted to Java filetype)
    triples = os.path.join(Path(__file__).parent, 'data', triples_file)
    kg_client.uploadOntology(triples)

    # Instantiate pound sterling symbols
    query = insert_pound_sterling_symbols()
    kg_client.performUpdate(query)

    # 2) Extract building (point) locations from KG
    bldg_locations = os.path.join(Path(__file__).parent, 'data', bldg_loc)
    extract_property_locations(kg_client, bldg_locations)

    # 3) Attach rdfs:label to affected properties
    affected_bldg = os.path.join(Path(__file__).parent, 'data', affected)
    attach_labels(kg_client, label=label, input_csv=affected_bldg)

    # 4) Initialise Property Price Index
    #NOTE: This initialises the property price index; however, no pure input time
    #      stamp is attached. This will only be done with derivation markup 
    initialise_ukhpi(kg_client)
