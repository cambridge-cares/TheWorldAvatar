from argparse import ArgumentParser
import json
import os

import rdflib
from rdflib.term import URIRef


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("filenames", nargs="+", help="Paths to owl files")
    parser.add_argument(
        "--path2headtypes",
        help="Path to txt file containing \n separated list of head node types to retrieve properties for",
        default=None,
    )
    parser.add_argument(
        "--path2prefixes",
        help="Path to json file containing namespace prefix mappings",
        default=None,
    )
    parser.add_argument("--out", help="Path to output file", required=True)
    args = parser.parse_args()

    g = rdflib.Graph()
    for filename in args.filenames:
        g.parse(filename, format="xml")

    if args.path2headtypes:
        with open(args.path2headtypes, "r") as f:
            head_types = f.read().split()
        head_types = [URIRef(e) for e in head_types]
    else:
        head_types = [
            x
            for x, in g.query("SELECT DISTINCT * WHERE { ?s a owl:Class }")
            if isinstance(x, URIRef)
        ]

    if args.path2prefixes:
        with open(args.path2prefixes, "r") as f:
            prefixes = json.load(f)
        name2iri = {row["name"]: row["uri"] for row in prefixes}
    else:
        prefixes = []
        name2iri = dict()

    def get_relations_from_head(stype: URIRef):
        query = f"""SELECT DISTINCT ?p ?o  WHERE {{ 
    VALUES ?s {{ {stype.n3()} }}
    {{
        ?p rdfs:domain ?s . 
        {{
            SELECT ?o WHERE {{
                {{
                    ?p rdfs:range ?o .
                }} UNION {{
                    ?p rdfs:range ?Range .
                    ?Range rdf:type owl:Class ; owl:unionOf ?Collection .
                    ?Collection rdf:rest*/rdf:first ?o .
                }}
            }}
        }}
    }} UNION {{
        {{
            SELECT ?p ?o WHERE {{
                ?s rdfs:subClassOf ?Restriction .
                ?Restriction a owl:Restriction ; owl:onProperty ?p .
                {{
                    ?Restriction owl:allValuesFrom [ rdf:type owl:Class ; owl:unionOf ?Collection ] .
                    ?Collection rdf:rest*/rdf:first ?o .
                }} UNION {{
                    ?Restriction owl:allValuesFrom ?o .
                }}
            }}
        }}
    }} UNION {{
        VALUES ?p {{ rdfs:subClassOf }}
        ?s ?p ?o .
    }}
}}"""
        for p, o in g.query(query):
            if isinstance(o, URIRef):
                yield (stype, p, o)

    relations = [
        triple for stype in head_types for triple in get_relations_from_head(stype)
    ]
    edge_types = [p for p in set(p for _, p, _ in relations)]

    def flatten(x: URIRef):
        x = str(x)
        if name2iri:
            for name, iri in name2iri.items():
                if x.startswith(iri):
                    x = "{a}:{b}".format(a=name, b=x[len(iri) :])
                    break
        return x

    def get_type_description(type: URIRef):
        query = f"""SELECT ?comment ?label WHERE {{
    VALUES ?s {{ {type.n3()} }}
    OPTIONAL {{
        ?s rdfs:comment ?comment .
    }}
    OPTIONAL {{
        ?s rdfs:label ?label .
    }}
}}"""
        for comment, label in g.query(query):
            return {"iri": flatten(type), "comment": comment, "label": label}
        return {"iri": flatten(type), "comment": None, "label": None}

    schema = {
        "prefixes": prefixes,
        "node_types": [get_type_description(n) for n in head_types],
        "edge_types": [get_type_description(e) for e in edge_types],
        "relations": [
            dict(s=flatten(s), p=flatten(p), o=flatten(o)) for s, p, o in relations
        ],
    }

    dirname = os.path.dirname(args.out)
    if dirname:
        os.makedirs(dirname, exist_ok=True)

    with open(args.out, "w") as f:
        json.dump(schema, f, indent=4)
