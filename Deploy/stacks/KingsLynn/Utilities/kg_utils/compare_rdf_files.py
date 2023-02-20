################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 19 Oct 2022                            #
################################################

import dictdiffer
import os
import re
from collections import defaultdict
from pathlib import Path
from rdflib import Graph
from rdflib.compare import to_isomorphic, graph_diff


def rdf_diff(old, new):
    """
        Compares provided rdf files and creates two new rdf files with all differences:
        'only_in_file1' and 'only_in_file2'

        ONLY WORKS FOR TRIPLES, NOT QUADS
    """

    def _align_uuid(rdf_file, replace_with='123'):
        # Define UUID regex
        uuid_regex = r'[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}'
        with open(rdf_file, 'r') as file:
            contents = file.read()
        contents = re.sub(uuid_regex, replace_with, contents)
        # Adjust rdf file name
        to_keep = rdf_file[:rdf_file.rfind('.')]
        adj = rdf_file[rdf_file.rfind('.'):]
        adj = '_adj' + adj
        rdf_adj = to_keep + adj
        with open(rdf_adj, 'w') as file:
            file.write(contents)
        return rdf_adj

    # Align UUIDs in both files
    old_adj = _align_uuid(old)
    new_adj = _align_uuid(new)

    # Construct both triple graphs from rdf files
    graph1 = Graph()
    graph1.parse(old_adj)
    graph2 = Graph()
    graph2.parse(new_adj)

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
    fp2 = os.path.join(old[:old.rfind('\\')], 'only_in_old.ttl')
    fp3 = os.path.join(old[:old.rfind('\\')], 'only_in_new.ttl')
    in_both.serialize(fp1, format="ttl")
    in_first.serialize(fp2, format="ttl")
    in_second.serialize(fp3, format="ttl")


def get_unique_predicates_with_counts(nt_file_path):
    # Load the .nt file into a graph
    g = Graph()
    g.parse(nt_file_path, format="nt")

    # Execute SPARQL query on the graph
    result = g.query("""
        SELECT ?s ?p ?o
        WHERE {
            ?s ?p ?o .
        }
    """)
    # Get all predicates
    res = [str(row.p) for row in result]
    # Create a dictionary with unique predicates and counts
    d = create_dict_with_counts(res)
    return d


def create_dict_with_counts(lst):
    count_dict = defaultdict(int)
    for element in lst:
        count_dict[element] += 1
    return dict(count_dict)


if __name__ == '__main__':

    # Specify input file paths (relative path)
    fp1 = r'..\data\outputs\run1.nt'
    fp2 = r'..\data\outputs\run2.nt'
    # Specify output file paths
    fp3 = r'..\data\outputs\predicates.txt'

    # Create actual file paths
    old_triples = os.path.join(Path(__file__).parent, fp1)
    new_triples = os.path.join(Path(__file__).parent, fp2)

    #
    # 1) Compare instantiated predicates
    #
    # NOTE: Done before IRI alignment to avoid distortion of counts for each predicate
    pred_h = get_unique_predicates_with_counts(old_triples)
    pred_m = get_unique_predicates_with_counts(new_triples)
    with open(os.path.join(Path(__file__).parent, fp3), 'w') as f:
        for diff in list(dictdiffer.diff(pred_h, pred_m)):
            print(diff, file=f)

    #
    # 2) Compare instantiated triples
    #
    rdf_diff(old_triples, new_triples)
