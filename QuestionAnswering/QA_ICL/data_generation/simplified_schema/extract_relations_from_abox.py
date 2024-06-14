from argparse import ArgumentParser
import json
import os

from SPARQLWrapper import JSON, SPARQLWrapper


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("endpoint", help="Paths to SPARQL endpoint")
    parser.add_argument(
        "--path2headtypes",
        help="Path to txt file containing \n separated list of types of subject entities to retrieve properties for",
        required=True,
    )
    parser.add_argument("--out", help="Path to output dir", required=True)
    args = parser.parse_args()

    sparql_client = SPARQLWrapper(args.endpoint)
    sparql_client.setReturnFormat(JSON)

    with open(args.path2headtypes, "r") as f:
        head_types = f.read().split()

    relations = []
    for ht in head_types:
        query = """SELECT DISTINCT ?p ?ot WHERE {{
    ?s a <{head_type}> .
    ?s ?p ?o .
    ?o a ?ot .
}}""".format(
            head_type=ht
        )

        sparql_client.setQuery(query)
        res = sparql_client.queryAndConvert()

        relations.extend(
            (ht, binding["p"]["value"], binding["ot"]["value"])
            for binding in res["results"]["bindings"]
        )

    relations = [{"s": s, "p": p, "o": o} for s, p, o in relations]

    os.makedirs(args.out, exist_ok=True)

    with open(os.path.join(args.out, "relations.json"), "w") as f:
        json.dump(relations, f, indent=4)

