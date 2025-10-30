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
    parser.add_argument("--out", help="Path to output directory", required=True)
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

    def get_relations_from_head(stype: URIRef):
        query = f"""SELECT DISTINCT ?p ?o 
WHERE {{ 
    VALUES ?s {{ {stype.n3()} }}
    {{
        ?s rdfs:subClassOf* ?superclass .
        ?p rdfs:domain ?superclass . 
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
    }}
}}"""
        for p, o in g.query(query):
            if isinstance(o, URIRef):
                yield (stype, p, o)

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
            data = {"iri": type, "comment": comment, "label": label}
            return {k: v for k, v in data.items() if v}
        return {"iri": type}

    print("Extracting descriptions of node types...", end=" ", flush=True)
    node_types = [get_type_description(n) for n in head_types]
    print("Done", flush=True)

    print("Extracting descriptions of relations...", end=" ", flush=True)
    relations = [
        triple for stype in head_types for triple in get_relations_from_head(stype)
    ]
    print("Done", flush=True)

    print("Extracting descriptions of edge types...", end=" ", flush=True)
    edge_types = [p for p in set(p for _, p, _ in relations)]
    edge_types = [get_type_description(e) for e in edge_types]
    print("Done", flush=True)

    os.makedirs(args.out, exist_ok=True)

    annotations = node_types + edge_types
    with open(os.path.join(args.out, "annotations.json"), "w", encoding="UTF-8") as f:
        json.dump(annotations, f, indent=4, ensure_ascii=False)

    with open(os.path.join(args.out, "relations.json"), "w", encoding="UTF-8") as f:
        json.dump([{"s": s, "p": p, "o": o} for s, p, o in relations],
            f, indent=4, ensure_ascii=False)
