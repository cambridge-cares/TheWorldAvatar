from argparse import ArgumentParser
import json
import os

import rdflib
from rdflib.term import URIRef

if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("filenames", nargs="+", help="Paths to owl files")
    parser.add_argument(
        "--path2entities",
        help="Path to txt file containing \n separated entity classes to retrieve properties for",
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

    if args.path2entities:
        with open(args.path2entities, "r") as f:
            entities = f.read().split()
        entities = [URIRef(e) for e in entities]
    else:
        entities = [
            x
            for x, in g.query("SELECT DISTINCT * WHERE { ?s a owl:Class }")
            if isinstance(x, URIRef)
        ]

    if args.path2prefixes:
        with open(args.path2prefixes, "r") as f:
            prefixes = json.load(f)
        name2iri = {row["name"]: row["uri"] for row in prefixes}
    else:
        prefixes = None
        name2iri = None

    relationships = []
    for s in entities:
        query = f"""SELECT DISTINCT ?p ?o  WHERE {{ 
        VALUES ?s {{ {s.n3()} }}
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
                relationships.append((s, p, o))

    def flatten(x: URIRef):
        x = str(x)
        if name2iri:
            for name, iri in name2iri.items():
                if x.startswith(iri):
                    x = "{a}:{b}".format(a=name, b=x[len(iri) :])
                    break
        return x

    schema = dict()
    if prefixes:
        schema["prefixes"] = prefixes
    schema["entities"] = [flatten(e) for e in entities]
    schema["relationships"] = [
        dict(s=flatten(s), p=flatten(p), o=flatten(o)) for s, p, o in relationships
    ]

    dirname = os.path.dirname(args.out)
    if dirname:
        os.makedirs(dirname, exist_ok=True)

    with open(args.out, "w") as f:
        json.dump(schema, f, indent=4)
