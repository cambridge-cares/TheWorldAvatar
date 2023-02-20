from SPARQLWrapper import SPARQLWrapper, JSON


class IntegratedTrainingFileCreator:

    def __init__(self, sparql_namespace, endpoint_url="http://www.theworldavatar.com/blazegraph"):
        self.sparql_namespace = sparql_namespace
        self.endpoint_url = endpoint_url

    def query_blazegraph(self, query):
        print(f"Querying {self.endpoint_url}")
        sparql = SPARQLWrapper(f"{self.endpoint_url}/namespace/" + self.sparql_namespace + "/sparql")
        sparql.setQuery(query)
        sparql.setReturnFormat(JSON)
        results = sparql.query().convert()
        return results
