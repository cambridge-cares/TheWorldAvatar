################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 10 Feb 2023                           #
################################################

# This module extracts all quads from an online Blazegraph SPARQL endpoint 
# and saves them as .nq file

import os
import time
from pathlib import Path
from rdflib import ConjunctiveGraph, URIRef
from SPARQLWrapper import SPARQLWrapper, JSON


def get_all_quads(endpoint, filepath):
    # SPARQL CONSTRUCT doesn't seem to work with quads; hence, each named
    # graph is extracted separately and then merged into one graph
    # https://docs.aws.amazon.com/neptune/latest/userguide/migrating-from-blazegraph.html

    # Define batch size for SPARQL queries (to avoid potential timeout, memory, etc. issues)
    batch = 1000000
    
    # Initialise 2 SPARQLWrapper objects due to different return formats
    sparql1 = SPARQLWrapper(endpoint)
    sparql2 = SPARQLWrapper(endpoint)

    # Initialise empty results graph
    entire_namespace = ConjunctiveGraph()

    # 1) Getting all named graphs
    sparql1.setReturnFormat(JSON)
    queryString = 'SELECT DISTINCT ?g WHERE { GRAPH ?g { ?s ?p ?o } } '
    sparql1.setQuery(queryString)
    results = sparql1.queryAndConvert()
    graphs= [g['g']['value'] for g in results['results']['bindings']]

    for g in graphs:
        print(f'\nExporting named graph: {g}')
        # 2.1) Get number of triples in named graph
        queryString = f'SELECT (COUNT(*) AS ?count) WHERE {{ GRAPH <{g}> {{ ?s ?p ?o }} }} '
        sparql1.setQuery(queryString)
        results = sparql1.queryAndConvert()
        n_triples = int(results['results']['bindings'][0]['count']['value'])
        print(f'  Number of quads: {n_triples:8d}')
        for n in range(n_triples//batch + 1):
            print(f'  Batch {n+1:3d}/{n_triples//batch + 1:3d}')
            print(f'    Exporting quads...')
            pending = True
            t = time.time()
            offset = n*batch
            while pending:
                try:
                    # 2.2) Run CONSTRUCT query for all triples in named graph (in batches)
                    queryString = f'CONSTRUCT {{ ?s ?p ?o . }} WHERE {{ GRAPH <{g}> {{ ?s ?p ?o }} }} ORDER BY ?s ?p ?o LIMIT {batch} OFFSET {offset}'
                    sparql2.setQuery(queryString)
                    subgraph = sparql2.queryAndConvert()
                    pending = False
                except Exception as ex:
                    # http.client.IncompleteRead Exceptions have been observed previously
                    # This typically occurs when the connection to the server is lost or if 
                    # the server is unable to send the complete response --> retry until successful
                    print('    Error: ', ex)
                    print('    Retrying...')
            elapsed = round(time.time() - t, 2)
            print(f'    Done {elapsed} s')            

            # Get all quads from subgraph (context will be blank graph identifier)
            # and convert quad tuples to list (as tuples are immutable)
            print(f'    Adding quads to export graph...')
            t = time.time()
            quads = [list(q) for q in subgraph.quads()]
            # Update context/named graph info
            context = URIRef(g)
            for q in quads:
                q[3] = context
            quads = [tuple(q) for q in quads]
            # Add quads from named graph to entire namespace
            entire_namespace.addN(quads)
            elapsed = round(time.time() - t, 2)
            print(f'    Done {elapsed} s')
    
    # For available format see: https://rdflib.readthedocs.io/en/stable/intro_to_parsing.html
    entire_namespace.serialize(filepath, format='nquads')


if __name__ == '__main__':

    # Specify SPARQL query endpoint
    endpoint = "http://165.232.172.16:3838/blazegraph/namespace/ocgml/sparql"
    # Output file for triples (relative path)
    fp = r'..\data\outputs\ocgml.nq'

    # Get all Triples and serialise as turtle
    file_name = os.path.join(Path(__file__).parent, fp)
    get_all_quads(endpoint, file_name)
