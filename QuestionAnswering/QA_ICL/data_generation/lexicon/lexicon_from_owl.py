from argparse import ArgumentParser
import json
import os

from openai import OpenAI
import rdflib


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("filename", help="Path to OWL file")
    parser.add_argument(
        "--base_class",
        required=True,
        help="IRI of base class of classes to retrieve labels for",
    )
    parser.add_argument("--out", required=True, help="Path to output JSON file")
    args = parser.parse_args()

    g = rdflib.Graph()
    g.parse(args.filename, format="xml")

    query = """SELECT ?s ?label WHERE {{
    ?s rdfs:subClassOf* <{base_class}> .
    ?s rdfs:label ?label
}}""".format(
        base_class=args.base_class
    )

    data = [
        {"iri": s, "label": label, "surface_forms": []} for s, label in g.query(query)
    ]

    os.makedirs(os.path.dirname(args.out), exist_ok=True)

    with open(args.out, "w") as f:
        json.dump(data, f, indent=4)
