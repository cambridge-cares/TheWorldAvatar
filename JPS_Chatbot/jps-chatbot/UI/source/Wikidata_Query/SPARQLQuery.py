import requests
from concurrent.futures import ThreadPoolExecutor, as_completed
import json
import time


def make_request(_url, index, query):
    headers = {'Accept': 'application/sparql-results+json'}
    html = requests.get(_url, stream=True, headers=headers)
    try:
        json.loads(html.content.decode('utf-8'))
        return json.loads(html.content.decode('utf-8')), index, query
    except:
        print('[ERROR SPARQL Query 14]: failed to get result from Wikidata')
        return html.content.decode('utf-8'), index, query


def identity_valid_result(result):
    try:
        r = result[0]['results']['bindings']
    except:
        print('[ERROR SPARQL Query 22]: failed to find valid result')
        return None
    if len(r) > 0:
        return result
    else:
        print('[ERROR SPARQL Query 27]: failed to find valid result')
        return None


class SPARQLQuery:
    def __init__(self, socketio):
        self.endpoint = 'https://query.wikidata.org/sparql?format=json&query='
        self.iteration_round = 0
        self.query_step = 2
        self.socketio = socketio

    def start_queries(self, queries):
        valid_results = []
        r = None
        while r is None:
            if len(queries) < self.query_step:  # each time first self.query_step many steps
                self.query_step = len(queries)
            r = self.start_multiple_queries(queries, valid_results)
            if len(queries) == 0:
                return None
            else:
                queries = queries[self.query_step:]  # remove the first self.query_step elements from the queries
            time.sleep(2)

        return r

    def start_multiple_queries(self, queries, valid_results):

        print('=========================== Queries generated ========================')
        if len(queries)>= 1:
            print('The query with highest score is', queries[0])

        print('starting a batch quest of ', len(queries), 'for iteration', self.iteration_round)
        self.iteration_round = self.iteration_round + 1
        processes = []
        with ThreadPoolExecutor(max_workers=self.query_step) as executor:
            counter = 0
            for q in queries[:self.query_step]:  # only select the first 5 queries in the list
                counter = counter + 1
                try:
                    rurl = self.endpoint + requests.utils.quote(q)
                    if '&query=' in rurl:
                        print('the url requested', rurl.split('&query=')[0])
                    processes.append(executor.submit(make_request, rurl, counter, q))
                    time.sleep(1)
                except:
                    time.sleep(1)
                    pass
        for task in as_completed(processes):
            r = identity_valid_result(task.result())
            if r is not None:
                valid_results.append(r)
        if len(valid_results) == 0:
            return None
        else:
            sorted_results = sorted(valid_results, key=lambda x: x[1])
            return sorted_results[0]
