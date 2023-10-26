from SPARQLWrapper import SPARQLWrapper, JSON


class KgClient:
    def __init__(self, kg_endpoint: str):
        client = SPARQLWrapper(endpoint=kg_endpoint)
        client.setReturnFormat(JSON)
        self.client = client

    def query(self, query: str):
        self.client.setQuery(query)
        return self.client.queryAndConvert()["results"]["bindings"]
