# pip install sparqlwrapper
# https://rdflib.github.io/sparqlwrapper/
import _thread
import sys
from pprint import pprint
from time import sleep

from SPARQLWrapper import SPARQLWrapper, JSON
import requests
from concurrent.futures import ThreadPoolExecutor, as_completed
import json
import time

from functools import lru_cache

@lru_cache(maxsize=None)
def make_request(_url, index, query):
    headers = {'Accept': 'application/sparql-results+json'}
    html = requests.get(_url, stream=True, headers=headers)
    try:
        json.loads(html.content.decode('utf-8'))
        return json.loads(html.content.decode('utf-8')), index, query
    except:
        print(html.content.decode('utf-8'))
        return html.content.decode('utf-8'), index, query


def identity_valid_result(result):
    try:
        r = result[0]['results']['bindings']
        print('---------- valid result ------------')
        print(r)

    except:
        return None
    if len(r) > 0:
        return result
    else:
        return None


class SPARQLQuery:
    def __init__(self, socketio):
        self.endpoint = 'https://query.wikidata.org/sparql?format=json&query='
        self.iteration_round = 1
        self.query_step = 2
        self.socketio = socketio

    def start_queries(self, queries):
        # try:
        #     from __main__ import socketio
        #     print('Importing socketIO from main in interpretation')
        # except ImportError:
        #     from run import socketio
        #     print('Importing socketIO from run_socket in interpretation')
        self.socketio.emit('coordinate_agent', 'Querying the Wikidata Knowledge Graph')

        self.iteration_round = 1
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

        print('starting a batch quest of ', len(queries), 'for iteration', self.iteration_round)
        self.iteration_round = self.iteration_round + 1
        processes = []
        with ThreadPoolExecutor(max_workers=self.query_step) as executor:
            counter = 0
            for q in queries[:self.query_step]:  # only select the first 5 queries in the list
                counter = counter + 1
                try:
                    rurl = self.endpoint + requests.utils.quote(q)
                    print('the url requested', rurl)
                    processes.append(executor.submit(make_request, rurl, counter, q))
                    time.sleep(1)
                except:
                    time.sleep(1)
                    pass
        for task in as_completed(processes):
            r = identity_valid_result(task.result())
            print('----------- task result ----------')
            print(task.result())
            print(type(task.result()))
            if r is not None:
                valid_results.append(r)
                # TODO: find the result with the highest ranking ...
                # TODO: put the attribute names in m
        # pprint(valid_results)
        print('we have got line 83', len(valid_results))

        if len(valid_results) == 0:
            return None
        else:
            print('we have got line 86')
            sorted_results = sorted(valid_results, key=lambda x: x[1])
            print('=============== sorted result =================')
            print(sorted_results)


            return sorted_results[0]
