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
    parser.add_argument(
        "--cls_tag",
        required=True,
        help="Value of class tag that will be added to each retrieve entity",
    )
    parser.add_argument(
        "--populate_surface_forms",
        action="store_true",
        help="Whether to populate surface_forms field with the label",
    )
    parser.add_argument(
        "--ontop",
        action="store_true",
        help="Whether the endpoint is an ontop endpoint",
    )
    parser.add_argument("--out", required=True, help="Path to output JSON file")
    args = parser.parse_args()

    sparql_client = SPARQLWrapper(args.endpoint)
    sparql_client.setReturnFormat(JSON)

    def get_query_for_ontop():
        return """SELECT ?s ?label WHERE {{
    ?s a <{base_class}> .
    ?s rdfs:label ?label
}}"""

    def get_query_for_blazegraph():
        return """SELECT ?s ?label WHERE {{
    ?s a/rdfs:subClassOf* <{base_class}> .
    ?s rdfs:label ?label
}}"""

    query = (get_query_for_blazegraph() if not args.ontop else get_query_for_ontop()).format(
        base_class=args.base_class
    )

    sparql_client.setQuery(query)
    res = sparql_client.queryAndConvert()

    def make_datum(row: dict):
        if args.populate_surface_forms:
            return {
                "iri": row["s"]["value"],
                "cls": args.cls_tag,
                "label": row["label"]["value"],
                "surface_forms": [row["label"]["value"]],
            }
        else:
            return {"iri": row["s"]["value"], "label": row["label"]["value"]}

    data = [make_datum(row) for row in res["results"]["bindings"]]

    os.makedirs(os.path.dirname(args.out), exist_ok=True)

    with open(args.out, "w") as f:
        json.dump(data, f, indent=4)
