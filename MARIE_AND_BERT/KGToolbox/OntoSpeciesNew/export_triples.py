################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 21 Sep 2022                            #
################################################

# This module extracts all triples from an online Blazegraph SPARQL endpoint 
# and saves them as .nt file

import os
from pathlib import Path
from SPARQLWrapper import SPARQLWrapper

from Marie.Util.location import DATA_DIR


def get_all_triples(endpoint, filepath):
    print("Query Started")
    sparql = SPARQLWrapper(endpoint)
    # Run Describe queries for all IRIs
    queryString = 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o } '
    sparql.setQuery(queryString)
    results = sparql.queryAndConvert()
    print("Query Done")
    # For available format see: https://rdflib.readthedocs.io/en/stable/intro_to_parsing.html
    # turtle: format='ttl'
    # n-triples: format='ntriples'
    results.serialize(filepath, format='ntriples')
    print("Exported file")


if __name__ == '__main__':
    # Specify SPARQL query endpoint
    ontology = "CrossGraph/ontospecies_new"
    endpoint = "http://www.theworldavatar.com/blazegraph/namespace/copy_ontospecies_marie"
    file_name = "ontospecies.nt" # freeze ontospecies 30,000 species version

    # Get all Triples and serialise as turtle
    full_path = os.path.join(DATA_DIR, ontology, file_name)
    get_all_triples(endpoint, full_path)
