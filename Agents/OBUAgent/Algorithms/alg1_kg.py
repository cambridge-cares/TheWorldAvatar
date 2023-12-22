import csv
from copy import deepcopy
from rdflib import Graph, URIRef
import Algorithms.instantiation as instantiation

# Function to find common cores between sets of cores grouped based on binding sites and update the precursor space
def expand_precursor_space(sparql_client, expanded_output_file):
    # Query the combination of binding sites and cores
    # The complete query should look something like below
    # However, only part of it is executed as SPARQL query due to performance issues related to property paths
    # """
    # prefix ocof: <https://www.theworldavatar.com/kg/ontocofs/>
    # select distinct ?bs ?core
    # where {
    #   ?bs a ocof:BindingSite.
    #   ?bs ^ocof:compatibleWith ?core.
    #   ?core (ocof:compatibleWith/^ocof:compatibleWith)+ ?co.
    #   FILTER NOT EXISTS{?co ocof:compatibleWith ?bs .}
    # }
    # """
    result_list = sparql_client.performQuery(
        """
        prefix ocof: <https://www.theworldavatar.com/kg/ontocofs/>
        select distinct ?bs ?core
        where {
            ?bs a ocof:BindingSite.
            ?bs ^ocof:compatibleWith ?core.
        }
        """
    )

    # Loop through the result list to construct dictionary
    result_dict = {bs_core['bs']: set() for bs_core in result_list}
    for bs_core in result_list:
        result_dict[bs_core['bs']].update(set([bs_core['core']]))
    existing_precursors_copy = deepcopy(result_dict)
    expanded_dict = {} # Prepare the dict to store expanded precursor space

    # Loop through each outer dictionary in the result list
    for outer_bs in result_dict:
        # Loop through each inner dictionary in the result list
        for inner_bs in result_dict:
            # NOTE the inner loop should contain all the remaining cores, but leaving out the same index as the outer loop
            if inner_bs != outer_bs:
                # If there are common cores, update the outer and inner core sets
                if result_dict[outer_bs].intersection(result_dict[inner_bs]):
                    result_dict[outer_bs].update(result_dict[inner_bs])
                    result_dict[inner_bs].update(result_dict[outer_bs])

    # Convert set to list
    expanded_dict = {k: list(v) for k, v in result_dict.items()}

    # Write the updated precursor space to the knowledge graph
    # An additional copy is saved to output file for easier manual inspection
    with open(expanded_output_file, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['BS', 'CORE'])

        g = Graph()
        for bs, cores in expanded_dict.items():
            for c in cores:
                writer.writerow([bs, c])
                if bs not in existing_precursors_copy:
                    g = instantiation.create_precursor(g, URIRef(bs), URIRef(c))
                    existing_precursors_copy[bs] = set([c])
                else:
                    if c not in existing_precursors_copy[bs]:
                        g = instantiation.create_precursor(g, URIRef(bs), URIRef(c))
                        existing_precursors_copy[bs].update([c])
        sparql_client.uploadGraph(g)
