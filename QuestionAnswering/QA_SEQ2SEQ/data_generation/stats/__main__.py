from argparse import ArgumentParser
import json

import networkx as nx
from locate_then_ask.query_graph import QueryGraph
from stats.aggregate import count_aggregate
from stats.functions import count_functions
from stats.solution_modifiers import count_solution_modifiers

from utils.json import as_enum
from .schema_items import count_schema_items

if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("input", type=str)
    args = parser.parse_args()

    with open(args.input, "r") as f:
        data = json.load(f, object_hook=as_enum)

    query_graphs = [QueryGraph(nx.node_link_graph(datum["query"]["graph"])) for datum in data]

    stats_schema = count_schema_items(query_graphs)
    stats_agg = count_aggregate(query_graphs)
    stats_func = count_functions(query_graphs)
    stats_solnmod = count_solution_modifiers(query_graphs)

    stats = dict(
        num=len(query_graphs),
        schema=stats_schema,
        aggregate=stats_agg,
        func=stats_func,
        soln_mod=stats_solnmod,
    )

    filename = args.input.rsplit(".", maxsplit=1)[0]
    with open(filename + "_stats.json", "w") as f:
        json.dump(stats, f, indent=4)
