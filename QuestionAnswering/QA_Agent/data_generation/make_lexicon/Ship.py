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


def make_surface_forms(label: str):
    sfs = [label]

    if label.startswith("Ship:"):
        name = label[len("Ship:") :].strip()
        sfs.append(name)

    return sfs


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument(
        "--endpoint", required=True, help="SPARQL endpoint to retrieve ship data"
    )
    parser.add_argument(
        "--lexicon_out", required=True, help="Path to JSON lexicon file"
    )
    args = parser.parse_args()

    sparql_client = SPARQLWrapper(args.endpoint)
    sparql_client.setReturnFormat(JSON)

    query = """PREFIX disp: <https://www.theworldavatar.com/kg/ontodispersion/>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>

SELECT ?IRI ?name WHERE {
  ?IRI a disp:Ship ; rdfs:label ?name .
}"""
    sparql_client.setQuery(query)
    res = sparql_client.queryAndConvert()
    bindings = [
        {k: v["value"] for k, v in binding.items()}
        for binding in res["results"]["bindings"]
    ]

    instance_data = [
        {
            "iri": binding["IRI"],
            "label": binding["name"],
        }
        for binding in bindings
    ]

    lexicon = [
        {
            "iri": row["iri"],
            "label": row["label"],
            "surface_forms": make_surface_forms(row["label"]),
        }
        for row in instance_data
    ]

    os.makedirs(os.path.dirname(args.lexicon_out), exist_ok=True)

    with open(args.lexicon_out, "w") as f:
        json.dump(lexicon, f, indent=4)
