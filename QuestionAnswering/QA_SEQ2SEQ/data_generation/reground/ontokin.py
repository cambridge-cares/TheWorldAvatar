import argparse
import json
import os
import random
from typing import Dict, List

import networkx as nx
from tqdm import tqdm
from combine_then_split import sanitize
from constants.fs import ROOTDIR
from locate_then_ask.kg_client import KgClient

from utils.json import EnumEncoder, as_enum

PATH_TO_ENTITIES_FOR_REGROUNDING = os.path.join(
    ROOTDIR, "data", "entities_for_regrounding", "ontokin.json"
)


def query_species_for_regrounding(kg_client: KgClient):
    query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT DISTINCT ?label (COUNT(*) AS ?deg) WHERE {
  ?x a okin:Species ; okin:belongsToPhase/a okin:GasPhase ; rdfs:label ?label .
  { ?x ?p ?o } UNION { ?s ?p ?x }
}
GROUP BY ?label
ORDER BY DESC(?deg) 
LIMIT 100"""
    response_bindings = kg_client.query(query)["results"]["bindings"]
    return [x["label"]["value"] for x in response_bindings]


def query_eqn_for_regrounding(kg_client: KgClient):
    query = """PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>

SELECT DISTINCT ?eqn (COUNT(*) AS ?deg) WHERE {
  ?x a/rdfs:subClassOf* okin:GasPhaseReaction ; okin:hasEquation ?eqn .
  { ?x ?p ?o } UNION { ?s ?p ?x }
}
GROUP BY ?eqn
ORDER BY DESC(?deg) 
LIMIT 100"""
    response_bindings = kg_client.query(query)["results"]["bindings"]
    eqns =  [x["eqn"]["value"] for x in response_bindings]
    eqns = [x.replace("[=]", random.choice(["<=>", "="])) for x in eqns]
    return eqns


def query_doi_for_regrounding():
    with open(
        os.path.join(ROOTDIR, "data", "entities_for_regrounding", "doi.txt"), "r"
    ) as f:
        data = [x.strip() for x in f.readlines()]
    return [x for x in data if x]


def query_entities_for_regrounding(kg_endpoint: str):
    client = KgClient(kg_endpoint)
    data = dict(
        Species=query_species_for_regrounding(client),
        Equations=query_eqn_for_regrounding(client),
        DOIs=query_doi_for_regrounding(),
    )
    with open(PATH_TO_ENTITIES_FOR_REGROUNDING, "w") as f:
        json.dump(data, f, indent=4)


def reground(entities_for_regrounding: Dict[str, List[str]], datum: dict):
    query_graph = nx.node_link_graph(datum["query"]["graph"])
    label2regrounded = dict()

    for node, attr in query_graph.nodes(data=True):
        if not attr.get("template_node"):
            continue

        if attr.get("rdf_type") == "os:Species":
            label_regrounded = random.choice(entities_for_regrounding["Species"])
        elif any(
            pred == "okin:hasEquation"
            for _, _, pred in query_graph.in_edges(node, data="label")
        ):
            label_regrounded = random.choice(entities_for_regrounding["Equations"])
        elif any(
            pred == "okin:hasProvenance/op:hasDOI"
            for _, _, pred in query_graph.in_edges(node, data="label")
        ):
            label_regrounded = random.choice(entities_for_regrounding["DOIs"])
        else:
            label_regrounded = None

        if label_regrounded is not None:
            node = query_graph.nodes[node]
            node["label_regrounded"] = label_regrounded
            label2regrounded[node["label"]] = label_regrounded

    if len(label2regrounded) == 0:
        return datum
    
    assert "[" in datum["verbalization"] and ']' in datum["verbalization"]
    sampling_frame = [datum["verbalization"]] + [x for x in datum["paraphrases"] if '[' in x and ']' in x]

    question = random.choice(sampling_frame)
    sparql = datum["query"]["sparql"]

    for label, regrounded in label2regrounded.items():
        question = question.replace(
            "[{label}]".format(label=label), "[{label}]".format(label=regrounded), 1
        )
        sparql = sparql.replace(
            '"{label}"'.format(label=label), '"{label}"'.format(label=regrounded), 1
        )

    datum["query"]["graph"] = nx.node_link_data(query_graph)
    datum["question"] = sanitize(question)
    datum["query"]["sparql"] = sparql

    return datum


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("input_file", type=str)
    parser.add_argument("--kg_endpoint", type=str, required=True)
    args = parser.parse_args()

    with open(args.input_file, "r") as f:
        data = json.load(f, object_hook=as_enum)

    if not os.path.exists(PATH_TO_ENTITIES_FOR_REGROUNDING):
        query_entities_for_regrounding(args.kg_endpoint)
    with open(PATH_TO_ENTITIES_FOR_REGROUNDING, "r") as f:
        entities_for_regrounding = json.load(f)

    data_out = []
    for datum in tqdm(data):
        data_out.append(reground(entities_for_regrounding, datum=datum))

    output_file = "{file}_regrounded.json".format(
        file=args.input_file.rsplit(".", maxsplit=1)[0]
    )
    with open(output_file, "w") as f:
        json.dump(data, f, indent=4, cls=EnumEncoder)
