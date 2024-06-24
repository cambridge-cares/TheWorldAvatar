# endpoint_url = "https://query.wikidata.org/sparql"
import _thread
import sys
from pprint import pprint
from time import sleep

from SPARQLWrapper import SPARQLWrapper, JSON

q2 = '''
       SELECT ?oLabel ?v ?unitLabel
        WHERE {
        ?o wdt:P31 wd:Q20054492 . # class    
        ?o wdt:P2101 ?x . # attribute_2  
        ?o p:P2067/psv:P2067 ?value . # attribute_1 x 2 
        ?value wikibase:quantityAmount ?v .
        ?value wikibase:quantityUnit ?unit .
        SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
        FILTER(?x > -100 ).  } # comparison numerical_value  

'''
endpoint_url = 'https://idsm.elixir-czech.cz/sparql/endpoint/idsm'

query_pubchem = '''
PREFIX sachem: <http://bioinfo.uochb.cas.cz/rdf/v1.0/sachem#>

SELECT * WHERE {
?COMPOUND sachem:substructureSearch [
    sachem:query "CC(=O)Oc1ccccc1C(O)=O" ].
}
LIMIT 1000
'''

def run_query():
    print('=================  the result from %s is =================')
    user_agent = "WDQS-example Python/%s.%s" % (sys.version_info[0], sys.version_info[1])

    sparql = SPARQLWrapper(endpoint_url, agent=user_agent)
    sparql.setQuery(query_pubchem)
    sparql.setReturnFormat(JSON)
    result = sparql.query().convert()
    pprint(result)
    return result


def process_multiple_queries(sparql_queries):
    length = len(sparql_queries)
    two_queries = sparql_queries[:2]
    try:
        print('starting the thread')
        _thread.start_new_thread(run_query)
    #  _thread.start_new_thread(run_query, (two_queries[1], 2,))
    except:
        print("Error: unable to start thread")


query = '''
SELECT ?oLabel ?v ?unitLabel
        WHERE {
        ?o wdt:P31 wd:Q159226 . # class    
        ?o wdt:P2102 ?x . # attribute_2  
        ?o p:P361/psv:P361 ?value . # attribute_1 x 2 
        ?value wikibase:quantityAmount ?v .
        ?value wikibase:quantityUnit ?unit .
        SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
        FILTER(?x > -100 ).  } # comparison numerical_value 
'''

# qs = [query, q2]
# process_multiple_queries(qs)
run_query()

