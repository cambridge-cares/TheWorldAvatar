import argparse
import json
import random
from typing import Dict, List

import networkx as nx
from combine_then_split import sanitize

from utils.json import EnumEncoder, as_enum


def reground(species: Dict[str, List[str]], datum: dict):
    query_graph = nx.node_link_graph(datum["query"]["graph"])
    label2regrounded = dict()

    for node, attr in query_graph.nodes(data=True):
        if attr.get("rdf_type") == "os:Species" and attr.get("template_node"):
            node = query_graph.nodes[node]
            node["label_regrounded"] = random.choice(species)
            label2regrounded[node["label"]] = node["label_regrounded"]

    question = random.choice([datum["verbalization"]] + datum["paraphrases"])
    sparql = datum["query"]["sparql"]

    for label, regrounded in label2regrounded.items():
        question = question.replace("[{label}]".format(label=label), "[{label}]".format(label=regrounded), 1)
        sparql = sparql.replace('"{label}"'.format(label=label), '"{label}"'.format(label=regrounded), 1)

    datum["query"]["graph"] = nx.node_link_data(query_graph)
    datum["question"] = sanitize(question)
    datum["query"]["sparql"] = sparql

    return datum


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--input_file", type=str)
    parser.add_argument("--output_file", type=str)
    parser.add_argument("--entities_for_regrounding", type=str)
    args = parser.parse_args()

    with open(args.input_file, "r") as f:
        data = json.load(f, object_hook=as_enum)

    with open(args.entities_for_regrounding, "r") as f:
        entities_for_regrounding = [x.strip() for x in f.readlines()]
    entities_for_regrounding = [x for x in entities_for_regrounding if x]

    data = [
        reground(
            species=entities_for_regrounding, 
            datum=datum
        ) 
        for datum in data
    ]

    with open(args.output_file, "w") as f:
        json.dump(data, f, indent=4, cls=EnumEncoder)
