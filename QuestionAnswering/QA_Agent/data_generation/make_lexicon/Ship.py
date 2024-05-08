from argparse import ArgumentParser
import json
import os
from typing import Dict

from SPARQLWrapper import JSON, SPARQLWrapper


def extract_name(iri: str):
    if "#" in iri:
        iri = iri.rsplit("#", maxsplit=1)[-1]
    if "/" in iri:
        iri = iri.rsplit("/", maxsplit=1)[-1]
    return iri


def make_surface_forms(attrs: Dict[str, str]):
    sfs = [attrs["label"]]

    if attrs["label"].startswith("Ship: "):
        name = attrs["label"][len("Ship: ") :]
        sfs.append(name)
    else:
        name = attrs["label"]
    sfs.append(" ".join([word.capitalize() for word in name.split()]))

    sfs.append(attrs["mmsi"])
    sfs.append("MMSI " + attrs["mmsi"])

    return sfs


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument(
        "--endpoint", required=True, help="SPARQL endpoint to retrieve ship data"
    )
    parser.add_argument("--data_out", required=True, help="Path to JSON data file")
    parser.add_argument(
        "--lexicon_out", required=True, help="Path to JSON lexicon file"
    )
    args = parser.parse_args()

    sparql_client = SPARQLWrapper(args.endpoint)
    sparql_client.setReturnFormat(JSON)

    query = """PREFIX disp: <https://www.theworldavatar.com/kg/ontodispersion/>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>

SELECT ?IRI ?MMSI ?name WHERE {
  ?IRI a disp:Ship ; rdfs:label ?name .
  ?IRI disp:hasProperty [ a disp:MMSI ; om:hasValue/om:hasNumericalValue ?MMSI ]
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
            "attributes": {
                "label": binding["name"],
                "mmsi": binding["MMSI"],
            },
        }
        for binding in bindings
    ]

    data = {"instances": instance_data}

    os.makedirs(os.path.dirname(args.data_out), exist_ok=True)

    with open(args.data_out, "w") as f:
        json.dump(data, f, indent=4)

    lexicon = [
        {
            "iri": row["iri"],
            "label": row["attributes"]["label"],
            "surface_forms": make_surface_forms(row["attributes"]),
        }
        for row in instance_data
    ]

    os.makedirs(os.path.dirname(args.lexicon_out), exist_ok=True)

    with open(args.lexicon_out, "w") as f:
        json.dump(lexicon, f, indent=4)
