import argparse
import ast
import json
import random

import pandas as pd
import networkx as nx

from reground.ontocompchem import OCCRegrounder
from reground.ontokin import OKRegrounder
from reground.reground import IdentityRegrounder
from utils.json import as_enum


def replace_entity_tags(text: str):
    text = text.replace("<entity>", "[")
    text = text.replace("</entity>", "]")
    return text


def remove_brackets(text: str):
    # TODO: remove brackets based on labels in query graph
    if text.startswith("["):
        text = text[1:]

    text = text.replace(" [", " ")
    text = text.replace("] ", " ")
    text = text.replace("([", "(")
    text = text.replace("])", ")")

    ptr = 0
    while ptr < len(text):
        ptr = text.find("]", ptr)
        if ptr < 0:
            break
        if ptr == len(text) - 1:
            text = text[:-1]
            break
        if text[ptr + 1] in ".!/;,:?'":
            text = text[:ptr] + text[ptr + 1 :]
        ptr += 2

    return text


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--filepath_examples", type=str)
    parser.add_argument("--filepath_paraphrases", type=str)
    parser.add_argument("--domain", type=str)
    args = parser.parse_args()

    df = pd.read_csv(args.filepath_paraphrases, index_col="id")
    df["paraphrases"] = (
        df["paraphrases"]
        .apply(ast.literal_eval)
        .apply(lambda lst: [replace_entity_tags(x) for x in lst])
    )

    with open(args.filepath_examples, "r") as f:
        data = json.load(f, object_hook=as_enum)

    # if args.domain == "ontokin":
    #     regrounder = OKRegrounder()
    # elif args.domain == "ontocompchem":
    #     regrounder = OCCRegrounder()
    # elif args.domain in ["ontospecies", "ontobuiltenv"]:
    #     regrounder = IdentityRegrounder()
    # else:
    #     raise ValueError("Unexpected domain: " + args.domain)
    regrounder = IdentityRegrounder()

    out = []
    for datum in data:
        query_graph = nx.node_link_graph(datum["query"]["graph"])
        query_sparql = str(datum["query"]["sparql"])
        parapharses = df.loc[datum["id"]]["paraphrases"]
        random.shuffle(parapharses)

        paraphrases_for_regrounding = parapharses[:2]
        parapharses_notfor_regrounding = parapharses[2:]

        pairs = regrounder.reground(
            query_graph=query_graph,
            query_sparql=query_sparql,
            paraphrases=paraphrases_for_regrounding,
        )
        pairs.append((query_sparql, datum["verbalization"]))
        pairs.extend([(query_sparql, p) for p in parapharses_notfor_regrounding])

        out.extend(
            [
                dict(
                    id="{id}_{num}".format(id=datum["id"], num=i),
                    domain=args.domain,
                    question=remove_brackets(nlq),
                    query=dict(sparql=sparql),
                )
                for i, (sparql, nlq) in enumerate(pairs)
            ]
        )

    filename = args.filepath_examples.rsplit(".", maxsplit=1)[0]
    with open(filename + "_regrounded.json", "w") as f:
        json.dump(out, f, indent=4)
