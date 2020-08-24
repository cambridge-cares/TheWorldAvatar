# pip install sparqlwrapper
# https://rdflib.github.io/sparqlwrapper/
import _thread
import sys
from pprint import pprint

from SPARQLWrapper import SPARQLWrapper, JSON


class SPARQLQuery:
    def __init__(self):
        self.endpoint_url = "https://query.wikidata.org/sparql"

    def get_results(self, query):
        user_agent = "WDQS-example Python/%s.%s" % (sys.version_info[0], sys.version_info[1])
        # TODO adjust user agent; see https://w.wiki/CX6
        sparql = SPARQLWrapper(self.endpoint_url, agent=user_agent)
        sparql.setQuery(query)
        sparql.setReturnFormat(JSON)
        return sparql.query().convert()

    def query(self, query):
        results = self.get_results(query)
        print('=================  the result is =================')
        pprint(results)
        return results

    def process_multiple_queries(self, sparql_queries):
        length = len(sparql_queries)
        two_queries = sparql_queries[:2]
        print(two_queries)

        try:
            _thread.start_new_thread(self.query, (two_queries[0],))
            _thread.start_new_thread(self.query, (two_queries[1],))
        except:
            print("Error: unable to start thread")
