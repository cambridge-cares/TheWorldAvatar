

from argparse import ArgumentParser
import json

from SPARQLWrapper.SPARQLExceptions import QueryBadFormed
from tqdm import tqdm

from locate_then_ask.kg_client import KgClient

PREFIXES = """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX ocape: <http://www.theworldavatar.com/ontology/ontocape/material/substance/reaction_mechanism.owl#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX okin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
PREFIX occ: <http://www.theworldavatar.com/ontology/ontocompchem/OntoCompChem.owl#>
PREFIX op: <http://www.theworldavatar.com/ontology/ontoprovenance/OntoProvenance.owl#>

PREFIX obe: <https://www.theworldavatar.com/kg/ontobuiltenv/>
PREFIX dabgeo: <http://www.purl.org/oema/infrastructure/>
PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX ict: <http://ontology.eil.utoronto.ca/icontact.owl#>

"""

if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument("--kg_endpoint", type=str, required=True)
    parser.add_argument("--dataset_path", type=str, required=True)
    args = parser.parse_args()

    kg_client = KgClient(args.kg_endpoint) 
    with open(args.dataset_path, "r") as f:
        data = json.load(f)

    for datum in tqdm(data):
        try:
            query = PREFIXES + datum["query"]["sparql"]
            kg_client.query(query)
        except QueryBadFormed as e:
            print("Query badly formed")
            print(datum)
            print(e)
            print()