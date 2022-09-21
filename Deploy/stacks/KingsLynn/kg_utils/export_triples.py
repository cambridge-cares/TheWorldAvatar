################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 21 Sep 2022                            #
################################################

# This module extracts all triples from an online Blazegraph SPARQL endpoint 
# and saves them as .ttl file

import os
from pathlib import Path
from SPARQLWrapper import SPARQLWrapper, JSON, RDFXML
from rdflib import Graph


def get_all_triples(endpoint, filepath):

    sparql = SPARQLWrapper(endpoint)

    # Run Describe queries for all IRIs
    queryString = 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o } '
    sparql.setQuery(queryString)
    results = sparql.queryAndConvert()
    
    results.serialize(filepath)


if __name__ == '__main__':

    # Specify SPARQL query endpoint
    endpoint = "http://128.199.197.40:3838/blazegraph/namespace/buildings/sparql"
    # Output file for triples (relative path)
    fp = r'outputs\triples.ttl'

    # Get all Triples and serialise as turtle
    file_name = os.path.join(Path(__file__).parent.parent, fp)
    get_all_triples(endpoint, file_name)



