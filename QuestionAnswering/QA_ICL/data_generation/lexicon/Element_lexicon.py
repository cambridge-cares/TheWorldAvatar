from argparse import ArgumentParser
from collections import defaultdict
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
        "--out", required=True, help="Path to output JSON file"
    )
    args = parser.parse_args()

    sparql_client = SPARQLWrapper(args.endpoint)
    sparql_client.setReturnFormat(JSON)

    query = """PREFIX pt: <http://www.daml.org/2003/01/periodictable/PeriodicTable#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>

SELECT ?Element ?ElementName ?ElementSymbol ?Label WHERE {
  ?Element a pt:Element .
  ?Element os:hasElementName/os:value ?ElementName .
  ?Element os:hasElementSymbol/os:value ?ElementSymbol .
  ?Element rdfs:label ?Label .
}"""
    sparql_client.setQuery(query)
    res = sparql_client.queryAndConvert()
    bindings = [
        {k: v["value"] for k, v in binding.items()}
        for binding in res["results"]["bindings"]
    ]

    iri2label = dict()
    iri2sfs = defaultdict(list)
    for binding in bindings:
        iri2label[binding["Element"]] = binding["Label"]
        iri2sfs[binding["Element"]].extend(
            [binding["ElementName"], binding["ElementSymbol"], binding["Label"]]
        )

    lexicon = [
        {
            "iri": iri,
            "clsname": "Element",
            "label": label,
            "surface_forms": list(set(iri2sfs[iri])),
        }
        for iri, label in iri2label.items()
    ]

    os.makedirs(os.path.dirname(args.out), exist_ok=True)

    with open(args.out, "w") as f:
        json.dump(lexicon, f, indent=4)
