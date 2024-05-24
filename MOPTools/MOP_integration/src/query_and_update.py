from pyderivationagent.kg_operations import PySparqlClient

OM                          = 'http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#'
OS                          = 'http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#'
RDFS                        = 'http://www.w3.org/2000/01/rdf-schema#'

SPARQL_QUERY_ENDPOINT: str  = 'http://68.183.227.15:3838/blazegraph/namespace/OntoMOPs'
SPARQL_UPDATE_ENDPOINT: str = 'http://68.183.227.15:3838/blazegraph/namespace/OntoMOPs'
KG_USERNAME: str            = 'bg_user'
KG_PASSWORD: str            = 'admin'

sparql_client = PySparqlClient(
            query_endpoint=SPARQL_QUERY_ENDPOINT, 
            update_endpoint=SPARQL_UPDATE_ENDPOINT,
            kg_user=KG_USERNAME, 
            kg_password=KG_PASSWORD
        )

# Construct the SPARQL query
query = """
PREFIX om: <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>

SELECT ?MOPIRI ?CCDCNum
WHERE {
    ?MOPIRI om:hasCCDCNumber ?CCDCNum
}
LIMIT 10
"""

# Execute the query
response = sparql_client.performQuery(query)

# Process and print the results
for res in response:
    print(f"MOP IRI: {res['MOPIRI']}, CCDC Number: {res['CCDCNum']}")
