################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 19 Oct 2022                            #
################################################

import os
from pathlib import Path
from rdflib import Graph
from rdflib.compare import to_isomorphic, graph_diff


def rdf_diff(old, new):
    """
        Compares provided rdf files and creates two new rdf files with all differences:
        'only_in_file1' and 'only_in_file2'

        ONLY WORKS FOR TRIPLES, NOT QUADS
    """

    # Construct both triple graphs from rdf files
    graph1 = Graph()
    graph1.parse(old)
    graph2 = Graph()
    graph2.parse(new)

    # Make isomorphic
    iso1 = to_isomorphic(graph1)
    iso2 = to_isomorphic(graph2)

    # Diff both graphs
    # As graph_diff uses graph canonization this can be very time consuming!
    # Hence, a simpler, but potentially less accurate diff of the non-canonizated graphs is used
    #in_both, in_first, in_second = graph_diff(iso1, iso2)
    in_both = iso2 * iso1
    in_first = iso1 - iso2
    in_second = iso2 - iso1

    # Write differences to files
    fp1 = os.path.join(old[:old.rfind('\\')], 'common_triples.ttl')
    fp2 = os.path.join(old[:old.rfind('\\')], 'only_in_old.ttl.ttl')
    fp3 = os.path.join(old[:old.rfind('\\')], 'only_in_new.ttl.ttl')
    in_both.serialize(fp1, format="ttl")
    in_first.serialize(fp2, format="ttl")
    in_second.serialize(fp3, format="ttl")



if __name__ == '__main__':

    # Specify file paths (relative path)
    fp1 = r'..\data\outputs\hmtest_with_sales.nt'
    fp2 = r'..\data\outputs\data_uploader_export.nt'

    old_triples = os.path.join(Path(__file__).parent, fp1)
    new_triples = os.path.join(Path(__file__).parent, fp2)
    rdf_diff(old_triples, new_triples)
