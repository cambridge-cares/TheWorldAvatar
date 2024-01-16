import csv
from rdflib import Graph, URIRef
import Algorithms.instantiation_kg as inst

# Function to find common cores between sets of cores grouped based on binding sites and update the precursor space
def expand_precursor_space(sparql_client, expanded_output_file, expand_all=True):
    # Query the combination of binding sites and cores that form the new precursor space
    result_list = sparql_client.performQuery(
        f"""
        prefix ocof: <https://www.theworldavatar.com/kg/ontocofs/>
        prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        select distinct ?bs ?core ?bs_label ?core_label
        where {{
            ?any_bs a ocof:BindingSite.
            ?any_bs ^ocof:compatibleWith ?core.
            ?any_bs {'(' if expand_all else ''}^ocof:compatibleWith/ocof:compatibleWith{')+' if expand_all else ''} ?bs.
            FILTER NOT EXISTS{{?core ocof:compatibleWith ?bs .}}
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
