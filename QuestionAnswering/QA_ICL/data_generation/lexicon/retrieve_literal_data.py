from argparse import ArgumentParser
from collections import defaultdict
import json

from SPARQLWrapper import JSON, SPARQLWrapper


def extract_name(iri: str):
    if "#" in iri:
        iri = iri.rsplit("#", maxsplit=1)[-1]
    if "/" in iri:
        iri = iri.rsplit("/", maxsplit=1)[-1]
    return iri


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument(
        "--clsnames",
        nargs="+",
        required=True,
        help="Classes to retrieve attribute data for",
    )
    parser.add_argument(
        "--tbox_endpoint", required=True, help="SPARQL endpoint to retrieve class data"
    )
    parser.add_argument(
        "--abox_endpoint",
        required=True,
        help="SPARQL endpoint to retrieve the instance data",
    )
    parser.add_argument("--out", required=True, help="Path to output JSON file")
    args = parser.parse_args()

    sparql_client = SPARQLWrapper(args.tbox_endpoint)
    sparql_client.setReturnFormat(JSON)

    query = """SELECT ?s ?p ?o WHERE {{
    VALUES ?s {{ {clsnames} }}
    ?s ?p ?o .
    FILTER ( isLiteral(?o) )
}}""".format(
        clsnames=" ".join("<{iri}>".format(iri=iri) for iri in args.clsnames)
    )
    sparql_client.setQuery(query)
    res = sparql_client.queryAndConvert()
    bindings = [
        {k: v["value"] for k, v in binding.items()}
        for binding in res["results"]["bindings"]
    ]

    clsname2pred2objs = defaultdict(lambda: defaultdict(list))
    for binding in bindings:
        clsname2pred2objs[extract_name(binding["s"])][
            extract_name(binding["p"])
        ].append(extract_name(binding["o"]))
    cls_data = [
        {
            "class": clsname,
            "attributes": {pred: ", ".join(objs) for pred, objs in pred2objs.items()},
        }
        for clsname, pred2objs in clsname2pred2objs.items()
    ]

    query = """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?s ?p ?o WHERE {{
    VALUES ?t {{ {clsnames} }}
    ?s rdf:type ?t .
    ?s ?p ?o .
    FILTER ( isLiteral(?o) || ?p = rdf:type )
}}""".format(
        clsnames=" ".join("<{iri}>".format(iri=iri) for iri in args.clsnames)
    )
    sparql_client.endpoint = args.abox_endpoint
    sparql_client.setQuery(query)
    res = sparql_client.queryAndConvert()
    bindings = [
        {k: v["value"] for k, v in binding.items()}
        for binding in res["results"]["bindings"]
    ]

    instance2pred2objs = defaultdict(lambda: defaultdict(list))
    for binding in bindings:
        instance2pred2objs[binding["s"]][extract_name(binding["p"])].append(
            extract_name(binding["o"])
        )

    instance_data = [
        {
            "iri": iri,
            "attributes": {pred: ", ".join(objs) for pred, objs in pred2objs.items()},
        }
        for iri, pred2objs in instance2pred2objs.items()
    ]

    data = {"classes": cls_data, "instances": instance_data}

    with open(args.out, "w") as f:
        json.dump(data, f, indent=4)
