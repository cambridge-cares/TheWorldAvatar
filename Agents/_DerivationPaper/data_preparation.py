# This script provides functionality to prepare and instantiate all data required
# for the minimum viable product of the King's Lynn use case for the derivation paper

import os
import requests
from pathlib import Path

from configs import *

from pyderivationagent.kg_operations import PySparqlClient

# Define name of n-triples file with consolidated property data (to be further amended)
triples_file = 'consolidated_properties.nt'



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


if __name__ == '__main__':

    # Create filepath
    fp = os.path.join(Path(__file__).parent, 'input_data', triples_file)

    # Ensure Blazegraph namespace exists
    create_blazegraph_namespace(SPARQL_UPDATE_ENDPOINT)

    # Initialise KG client
    kg_client = PySparqlClient(query_endpoint=SPARQL_QUERY_ENDPOINT,
                               update_endpoint=SPARQL_UPDATE_ENDPOINT)

    # Upload n-triples file (use `uploadOntolgy` method of pyderivation sparql_client 
    # directly which ensures file is converted to Java filetype)
    kg_client.uploadOntology(fp)