from SPARQLWrapper import SPARQLWrapper, JSON
import re


class SPARQLEngine:

    def __init__(self):
        self.endpoint_base_url = 'http://dbpedia.org/sparql'
        self.sparql = SPARQLWrapper(self.endpoint_base_url)

    def fire_query(self, query):
        sparql = self.sparql
        sparql.setQuery(query)
        sparql.setReturnFormat(JSON)
        results = sparql.query().convert()
        return results
