import json
import time
from pprint import pprint

import requests
from concurrent.futures import ThreadPoolExecutor, as_completed


def find_valid_results(result):
    try:
        bindings = result[0]['results']['bindings']
    except TypeError:
        # result empty
        bindings = []
    if len(bindings) == 0:
        return None
    else:
        return result


def make_request(_url, index, query):
    headers = {'Accept': 'application/sparql-results+json'}
    html = requests.get(_url, stream=True, headers=headers)
    try:
        json.loads(html.content.decode('utf-8'))
        return json.loads(html.content.decode('utf-8')), index, query
    except:
        return html.content.decode('utf-8'), index, query


def divide_list(l, n=2):
    # looping till length l
    for i in range(0, len(l), n):
        yield l[i:i + n]


class SPARQLQuery:
    def __init__(self):
        self.endpoint = 'https://query.wikidata.org/sparql?format=json&query='

    def start_queries(self, _all_queries):
        valid_results = []
        # divide the queries into groups of 2
        list_of_query_list = list(divide_list(_all_queries))
        for query_list in list_of_query_list:
            self.make_multiple_requests(query_list, valid_results)
        sorted_valid_results = sorted(valid_results, key=lambda x: x[1])
        if len(sorted_valid_results) == 0:
            return None
        else:
            return sorted_valid_results

    def make_multiple_requests(self, query_list, valid_results):
        processes = []
        with ThreadPoolExecutor(max_workers=len(query_list)) as executor:
            counter = 0
            for q in query_list:  # only select the first 5 queries in the list
                counter = counter + 1
                try:
                    rurl = self.endpoint + requests.utils.quote(q)
                    processes.append(executor.submit(make_request, rurl, counter, q))
                    time.sleep(1)
                except Exception as e:
                    time.sleep(1)
                    # print(str(e))

        for task in as_completed(processes):
            r = task.result()
            r = find_valid_results(r)
            if r is not None:
                valid_results.append(r)
        if len(valid_results) == 0:
            return None
        else:
            sorted_results = sorted(valid_results, key=lambda x: x[1])
            return sorted_results[0]


if __name__ == '__main__':
    sq = SPARQLQuery()
    q1 = '''          
            PREFIX schema: <http://schema.org/>
            SELECT *
            WHERE 
            {
              wd:Q2270 rdfs:label ?v .
              wd:Q2270 schema:description ?v2.
              SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
              FILTER ( lang(?v) = "en" )
              FILTER ( lang(?v2) = "en" )
            }
            '''
    q2 = '''
                PREFIX schema: <http://schema.org/>
            SELECT *
            WHERE 
            {
              wd:xxx rdfs:label ?v .
              wd:xxx schema:description ?v2.
              SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
              FILTER ( lang(?v) = "en" )
              FILTER ( lang(?v2) = "en" )
            }
    '''
    queries = [q1, q2]
    long_queries = [q1, q2, q1, q2, q1, q2, q1, q2, q1, q2, q1, q2, q1, q2, q1, q2, q1, q2]
    # rst_1 = sq.start_queries(queries)
    # pprint(rst_1)
    rst_2 = sq.start_queries(long_queries)
    for r in rst_2:
        print('-------------------')
        pprint(r)
