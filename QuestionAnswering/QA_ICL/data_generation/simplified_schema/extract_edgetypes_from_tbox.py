from argparse import ArgumentParser
import json
import os

import rdflib


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("filenames", nargs="+", help="Paths to owl files")
    parser.add_argument("--out", help="Path to output directory", required=True)
    args = parser.parse_args()

    g = rdflib.Graph()
    for filename in args.filenames:
        g.parse(filename, format="xml")

    query = f"""SELECT DISTINCT ?p ?label ?comment
WHERE {{
    VALUES ?t {{ owl:ObjectProperty owl:DatatypeProperty }}
    ?p a ?t .
    OPTIONAL {{
        ?p rdfs:label ?label .
    }}
    OPTIONAL {{
        ?p rdfs:comment ?comment .
    }}
}}"""
    data = [
        {k: v for k, v in {"iri": p, "label": label, "comment": comment}.items() if v}
        for p, label, comment in g.query(query)
    ]

    dirpath = os.path.dirname(args.out)
    os.makedirs(dirpath, exist_ok=True)

    with open(os.path.join(args.out), "w") as f:
        json.dump(data, f, indent=4)
