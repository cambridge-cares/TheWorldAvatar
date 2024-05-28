from argparse import ArgumentParser
import json
import os

from SPARQLWrapper import JSON, SPARQLWrapper

def extract_name(iri: str):
    if "#" in iri:
        iri = iri.rsplit("#", maxsplit=1)[-1]
    if "/" in iri:
        iri = iri.rsplit("/", maxsplit=1)[-1]
    return iri


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("--endpoint", help="SPARQL endpoint to retrieve label data")
    parser.add_argument(
        "--base_class",
        required=True,
        help="IRI of base class of classes to retrieve labels for",
    )
    parser.add_argument(
        "--populate_surface_forms",
        action="store_true",
        help="Whether to populate surface_forms field with the label",
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

    def make_datum(row: dict):
        if args.populate_surface_forms:
            return {
                "iri": row["s"]["value"],
                "clsname": extract_name(args.base_class),
                "label": row["label"]["value"],
                "surface_forms": [row["label"]["value"]],
            }
        else:
            return {"iri": row["s"]["value"], "label": row["label"]["value"]}

    data = [make_datum(row) for row in res["results"]["bindings"]]

    os.makedirs(os.path.dirname(args.out), exist_ok=True)

    with open(args.out, "w") as f:
        json.dump(data, f, indent=4)
