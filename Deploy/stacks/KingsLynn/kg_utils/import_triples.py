################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 21 Sep 2022                            #
################################################

# This module uploads all triples from an '.nt' file to
# an online Blazegraph SPARQL endpoint 


import os
from pathlib import Path
from SPARQLWrapper import SPARQLWrapper
from rdflib import Graph


def upload_all_triples(endpoint, filepath):

    # Read serialized triples
    with open(filepath, 'r') as f:
        triples = f.read()
    # Create INSERT DATA update
    insert_data = f'INSERT DATA {{ {triples} }}'

    # Initialise SPARQL wrapper
    sparql = SPARQLWrapper(endpoint)
    sparql.setMethod('POST')
    sparql.setQuery(insert_data)

    # Upload data
    results = sparql.query()

    if results.response.code == 200:
        print('Triple upload successfully finished!')
    else:
        raise Exception('Triple upload unsuccessful.')


if __name__ == '__main__':

    # Specify SPARQL query endpoint
    # NOTE: Endpoint needs to be available, i.e. manually created beforehand
    endpoint = "http://128.199.197.40:3838/blazegraph/namespace/test/sparql"
    # Output file for triples (relative path)
    fp = r'outputs\triples.nt'

    # Get all Triples and serialise as turtle
    file_name = os.path.join(Path(__file__).parent.parent, fp)
    upload_all_triples(endpoint, file_name)
