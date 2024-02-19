from SPARQLWrapper import SPARQLWrapper, SPARQLExceptions, POST, JSON


class KgClient:
    QUERY_PREFIXES = (
        "PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>\n"
        "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n"
        "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n"
    )

    def __init__(self, kg_endpoint: str):
        sparql = SPARQLWrapper(kg_endpoint)
        sparql.setReturnFormat(JSON)
        sparql.setMethod(POST)
        self.sparql = sparql

    def query(self, query: str):
        """Queries the KG and returns the response with the following format
        {
            "head": {
                "vars": List[str]
            },
            "results": {
                "bindings": [
                    Dict[
                        str,
                        {
                            "datatype": NotRequired[str],
                            "type": str
                            "value" str
                        }
                    ]
                ]
            }
        }
        """
        if not query.startswith("PREFIX"):
            query = self.QUERY_PREFIXES + query

        self.sparql.setQuery(query)

        try:
            return self.sparql.queryAndConvert()
        except SPARQLExceptions.QueryBadFormed:
            return None
