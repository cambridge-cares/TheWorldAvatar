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


# Define UUID regex
UUID_REGEX = r'[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}'

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
    fp2 = os.path.join(old[:old.rfind('\\')], 'only_in_old.ttl')
    fp3 = os.path.join(old[:old.rfind('\\')], 'only_in_new.ttl')
    in_both.serialize(fp1, format="ttl")
    in_first.serialize(fp2, format="ttl")
    in_second.serialize(fp3, format="ttl")


def get_unique_predicates_with_counts(nt_file_path, include_subjects=False):
    # Load the .nt file into a graph
    g = Graph()
    g.parse(nt_file_path, format="nt")

    if include_subjects:
        query = """
        SELECT distinct ?s ?p ?o ?type
        WHERE {
            ?s ?p ?o . 
            OPTIONAL { ?s a ?type . }
        }
    """
    else:
        query = """
            SELECT distinct ?s ?p ?o
            WHERE {
                ?s ?p ?o . 
            }
        """

    # Execute SPARQL query on the graph
    result = g.query(query)
    if include_subjects:
        res = []
        for row in result:
            # Get all predicates and subjects
            if row.type:
                subj_pred = str(row.type) + '_' + str(row.p)
            else:
                subj = str(row.s)
                subj = re.sub(UUID_REGEX, '', subj)
                subj_pred = subj + '_' + str(row.p)
            res.append(subj_pred)
    else:
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


def align_uuids(rdf_file, replace_with='123'):

    with open(rdf_file, 'r') as file:
        contents = file.read()
    contents = re.sub(UUID_REGEX, replace_with, contents)
    # Adjust rdf file name
    to_keep = rdf_file[:rdf_file.rfind('.')]
    adj = rdf_file[rdf_file.rfind('.'):]
    adj = '_adj' + adj
    rdf_adj = to_keep + adj
    with open(rdf_adj, 'w') as file:
        file.write(contents)
    return rdf_adj


if __name__ == '__main__':

    # Specify input file paths (relative path)
    fp1 = r'..\data\outputs\clean_slate.nt'
    fp2 = r'..\data\outputs\no_warnings.nt'
    # Specify output file paths
    fp3 = r'..\data\outputs\predicates.txt'
    fp4 = r'..\data\outputs\subject_predicate_pairs.txt'

    # Create actual file paths
    triples1 = os.path.join(Path(__file__).parent, fp1)
    triples2 = os.path.join(Path(__file__).parent, fp2)

    #
    # 1) Compare instantiated predicates
    #
    # NOTE: Done before IRI alignment to avoid distortion of counts for each predicate
    pred_1 = get_unique_predicates_with_counts(triples1)
    pred_2 = get_unique_predicates_with_counts(triples2)
    with open(os.path.join(Path(__file__).parent, fp3), 'w') as f:
        for diff in list(dictdiffer.diff(pred_1, pred_2)):
            print(diff, file=f)

    #
    # 2) Compare instantiated subject-predicates pairs
    #
    sub_pred1 = get_unique_predicates_with_counts(triples1, include_subjects=True)
    sub_pred2 = get_unique_predicates_with_counts(triples2, include_subjects=True)
    with open(os.path.join(Path(__file__).parent, fp4), 'w') as f:
        for diff in list(dictdiffer.diff(sub_pred1, sub_pred2)):
            print(diff, file=f)
    # Reformat file
    with open(os.path.join(Path(__file__).parent, fp4), 'r') as f:
        contents = f.read()
    with open(os.path.join(Path(__file__).parent, fp4), 'w') as f:
        f.writelines(contents.replace('), (', '),\n('))

    # Align UUIDs in both files
    triples1_adj = align_uuids(triples1, replace_with='')
    triples2_adj = align_uuids(triples2, replace_with='')

    #
    # 3) Compare instantiated triples
    #
    rdf_diff(triples1_adj, triples2_adj)
