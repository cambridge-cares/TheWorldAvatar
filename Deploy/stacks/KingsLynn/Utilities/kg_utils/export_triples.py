################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 21 Sep 2022                            #
################################################

# This module extracts all triples from an online Blazegraph SPARQL endpoint 
# and saves them as .nt file

import os
from pathlib import Path
from SPARQLWrapper import SPARQLWrapper


def get_all_triples(endpoint, filepath):

    sparql = SPARQLWrapper(endpoint)

    # Run CONSTRUCT queries for all IRIs
    queryString = 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o } '
    sparql.setQuery(queryString)
    results = sparql.queryAndConvert()
    
    # For available format see: https://rdflib.readthedocs.io/en/stable/intro_to_parsing.html
    # turtle: format='ttl'
    # n-triples: format='ntriples'
    results.serialize(filepath, format='ntriples')


if __name__ == '__main__':

    # Specify SPARQL query endpoint
    endpoint = "http://165.232.172.16:3838/blazegraph/namespace/placeholder/sparql"
    # Output file for triples (relative path)
    fp = r'..\data\outputs\placeholder.nt'

    # Get all Triples and serialise as turtle
    file_name = os.path.join(Path(__file__).parent, fp)
    get_all_triples(endpoint, file_name)
