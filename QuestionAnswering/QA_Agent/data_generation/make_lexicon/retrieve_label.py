from argparse import ArgumentParser
import json
import os

from SPARQLWrapper import JSON, SPARQLWrapper



if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("--endpoint", help="SPARQL endpoint to retrieve label data")
    parser.add_argument(
        "--base_class",
        required=True,
        help="IRI of base class of classes to retrieve labels for",
    )
    parser.add_argument("--out", required=True, help="Path to output JSON file")
    args = parser.parse_args()

    sparql_client = SPARQLWrapper(args.endpoint)
    sparql_client.setReturnFormat(JSON)

    query = """SELECT ?s ?label WHERE {{
    ?s a/rdfs:subClassOf* <{base_class}> .
    ?s rdfs:label ?label
}}""".format(
        base_class=args.base_class
    )

    sparql_client.setQuery(query)
    res = sparql_client.queryAndConvert()

    data = [
        {"iri": row["s"]["value"], "label": row["label"]["value"]} for row in res["results"]["bindings"]
    ]

    os.makedirs(os.path.dirname(args.out), exist_ok=True)

    with open(args.out, "w") as f:
        json.dump(data, f, indent=4)
