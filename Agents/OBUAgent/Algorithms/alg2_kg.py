import csv
from rdflib import Graph, URIRef
import Algorithms.instantiation_kg as inst

def expand_precursor_space_lfr(sparql_client, expanded_output_file, expand_all=True):
    """This function swaps the cores for binding sites that are involved in the same linkage formation reaction
    and share the same outer coordination number.
    """
    # Query the combination of binding sites and cores that form the new precursor space from linkage formation reactions
    result_list = sparql_client.performQuery(
        f"""
        prefix ocof: <https://www.theworldavatar.com/kg/ontocofs/>
        prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        select distinct ?bs ?core ?bs_label ?core_label
        where {{
            ?bs {'(' if expand_all else ''}^ocof:hasBindingSite/ocof:hasBindingSite{')+' if expand_all else ''} ?connected_bs.
            ?bs ocof:outerCoordinationNumber ?bs_ocn.
            ?connected_bs ocof:outerCoordinationNumber ?connected_bs_ocn.
            filter (?bs_ocn = ?connected_bs_ocn)
            ?connected_bs ^ocof:compatibleWith ?core.
            filter not exists {{?core ocof:compatibleWith ?bs}}
            ?bs rdfs:label ?bs_label.
            ?core rdfs:label ?core_label.
        }}
        order by ?bs_label ?core_label
        """
    )

    # Write the updated precursor space to the knowledge graph
    # An additional copy is saved to output file for easier manual inspection
    with open(expanded_output_file, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['BS', 'CORE'])

        g = Graph()
        for res in result_list:
            writer.writerow([res['bs_label'], res['core_label']])
            g = inst.create_precursor(g, URIRef(res['bs']), URIRef(res['core']))
        sparql_client.uploadGraph(g)
