from typing import Optional
from SPARQLWrapper import SPARQLWrapper, JSON


class KgClient:
    def __init__(self, kg_endpoint: str, user: Optional[str] = None, pw: Optional[str] = None):
        client = SPARQLWrapper(endpoint=kg_endpoint)
        client.setReturnFormat(JSON)
        if user is not None and pw is not None:
            client.setCredentials(user=user, passwd=pw)

        self.client = client

    def query(self, query: str):
        self.client.setQuery(query)
        return self.client.queryAndConvert()
