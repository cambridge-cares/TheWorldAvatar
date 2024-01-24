from SPARQLWrapper import POST, SPARQLWrapper, JSON

QUERY_PREFIXES = (
    "PREFIX os: <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>\n"
    "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n"
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n"
)


class KgClient:
    def __init__(self, kg_endpoint: str):
        sparql = SPARQLWrapper(kg_endpoint)
        sparql.setReturnFormat(JSON)
        sparql.setMethod(POST)
        self.sparql = sparql

    def query(self, query: str, limit: int = -1):
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
        query = query.strip()
        if not query.startswith("PREFIX"):
            query = QUERY_PREFIXES + query
        if limit > 0:
            query += f"LIMIT {limit}"

        self.sparql.setQuery(query)
        return self.sparql.queryAndConvert()
