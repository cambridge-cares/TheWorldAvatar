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
    # Initialise SPARQL wrapper
    sparql = SPARQLWrapper(endpoint)
    sparql.setMethod('POST')

    # Read serialized triples
    with open(filepath, 'r') as f:
        triples = f.readlines()

    # Split data in chunks to avoid memory issues (i.e. number of triples per upload)
    # NOTE: SPARQLWrapper SPARQL update quite size limited, i.e. having issues
    #       with chunk sizes > 2500 triples
    n = 2500
    chunks = [triples[i:i + n] for i in range(0, len(triples), n)]

    i = 1
    for c in chunks:
        print(f'Uploading batch {i:>3}/{len(chunks):>3}.')
        i += 1

        insert_triples = ' '.join(c)
        # Create INSERT DATA update
        insert_data = f'INSERT DATA {{ {insert_triples} }}'
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
    # endpoint = "http://128.199.197.40:3838/blazegraph/namespace/buildings_new/sparql"
    endpoint = "http://www.theworldavatar.com/blazegraph/namespace/copy_ontospecies_marie"
    # Input file for triples (relative path)
    # fp = r'..\data\inputs\data_mehal.nq'
    fp = r'c:\\Users\\LPAS01\\Desktop\\GitHub\\tools\\data\\pubchem.nt'

    # Get all Triples and serialise as turtle
    file_name = os.path.join(Path(__file__).parent, fp)
    upload_all_triples(endpoint, file_name)
