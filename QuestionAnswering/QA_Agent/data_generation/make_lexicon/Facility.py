from argparse import ArgumentParser
import json
import os

from SPARQLWrapper import SPARQLWrapper, JSON


class KgClient:
    def __init__(self, endpoint: str):
        client = SPARQLWrapper(endpoint)
        client.setReturnFormat(JSON)
        self.client = client

    def query(self, query: str):
        self.client.setQuery(query)
        return self.client.queryAndConvert()


if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument(
        "--bg_endpoint", required=True, help="SPARQL endpoint to ontocompany TBox"
    )
    parser.add_argument(
        "--ontop_endpoint", required=True, help="SPARQL endpoint to ontocompany ABox"
    )
    parser.add_argument("--out", required=True, help="Path to output JSON file")
    args = parser.parse_args()

    query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany/>

SELECT * WHERE {
  ?s rdfs:subClassOf* <http://www.theworldavatar.com/kg/ontocompany/IndustrialFacility>
}"""
    bg_client = KgClient(args.bg_endpoint)
    res = bg_client.query(query)
    clses = [binding["s"]["value"] for binding in res["results"]["bindings"]]

    query = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX ontocompany: <http://www.theworldavatar.com/kg/ontocompany/>

SELECT ?Facility ?FacilityName ?CompanyName ?pred WHERE {{
  VALUES ?FacilityType {{ {types} }}
  ?Facility rdf:type ?FacilityType .
  ?Facility rdfs:label ?FacilityName .
}}""".format(
        types=" ".join("<{iri}>".format(iri=iri) for iri in clses)
    )

    ontop_client = KgClient(args.ontop_endpoint)
    res = ontop_client.query(query)
    bindings = [
        {k: v["value"] for k, v in binding.items()}
        for binding in res["results"]["bindings"]
    ]

    data = [
        {
            "iri": binding["Facility"],
            "label": binding["FacilityName"],
            "surface_forms": [],
        }
        for binding in bindings
    ]

    os.makedirs(os.path.dirname(args.out), exist_ok=True)
    with open(args.out, "w") as f:
        json.dump(data, f, indent=4)
