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

    # Run Describe queries for all IRIs
    queryString = 'CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o } '
    sparql.setQuery(queryString)
    results = sparql.queryAndConvert()
    
    results.serialize(filepath, format='nt')


if __name__ == '__main__':

    # Specify SPARQL query endpoint
    #endpoint = "http://128.199.197.40:3838/blazegraph/namespace/buildings_new/sparql"
    endpoint = "http://165.232.172.16:3838/blazegraph/namespace/buildings_test/sparql"
    # Output file for triples (relative path)
    #fp = r'..\data\outputs\hmtest_without_sales.nt'
    #fp = r'..\data\outputs\hmtest_with_sales.nt'
    fp = r'..\data\outputs\data_uploader_export.nt'

    # Get all Triples and serialise as turtle
    file_name = os.path.join(Path(__file__).parent, fp)
    get_all_triples(endpoint, file_name)
