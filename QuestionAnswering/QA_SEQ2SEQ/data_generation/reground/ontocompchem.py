import argparse
import json
import random
from typing import Dict, List

import networkx as nx
from combine_then_split import sanitize

from utils.json import EnumEncoder, as_enum


def reground(entities_for_regrounding: Dict[str, List[str]], datum: dict):
    query_graph = nx.node_link_graph(datum["query"]["graph"])

    label2regrounded = dict()    
    
    if "LevelOfTheoryLabel" in query_graph.nodes() and not query_graph.nodes["LevelOfTheoryLabel"].get("question_node"):
        node = query_graph.nodes["LevelOfTheoryLabelFunc"]
        lots = node["operand"]
        lots_regrounded = random.sample(entities_for_regrounding["LevelOfTheory"], k=len(lots))
        node["operand_regrounded"] = lots_regrounded
        label2regrounded.update({label:regrounded for label, regrounded in zip(lots, lots_regrounded)})

    if "BasisSetLabel" in query_graph.nodes() and not query_graph.nodes["BasisSetLabel"].get("question_node"):
        node = query_graph.nodes["BasisSetLabelFunc"]
        bss = node["operand"]
        bss_regrounded = random.sample(entities_for_regrounding["BasisSet"], k=len(bss))
        node["operand_regrounded"] = bss_regrounded
        label2regrounded.update({label:regrounded for label, regrounded in zip(bss, bss_regrounded)})

    if "Species" in query_graph.nodes():
        node = query_graph.nodes["Species"]
        species_regrounded = random.choice(entities_for_regrounding["Species"])
        node["label_regrounded"] = species_regrounded
        label2regrounded[node["label"]] = species_regrounded

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
        entities_for_regrounding = json.load(f)
    
    data = [
        reground(
            entities_for_regrounding=entities_for_regrounding, 
            datum=datum
        ) 
        for datum in data
    ]

    with open(args.output_file, "w") as f:
        json.dump(data, f, indent=4, cls=EnumEncoder)
