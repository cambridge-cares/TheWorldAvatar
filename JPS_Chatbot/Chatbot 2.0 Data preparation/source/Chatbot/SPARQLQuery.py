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
    except:
        return None
    if len(r) > 0:
        return result
    else:
        return None


class SPARQLQuery:
    def __init__(self):
        self.endpoint = 'https://query.wikidata.org/sparql?format=json&query='
        self.iteration_round = 1
        self.query_step = 2

    def start_queries(self, queries):
        valid_results = []
        r = None
        while r is None:
            if len(queries) < self.query_step:  # each time first self.query_step many steps
                self.query_step = len(queries)
            r = self.start_multiple_queries(queries, valid_results)
            queries = queries[self.query_step:]  # remove the first self.query_step elements from the queries
            time.sleep(2)
            if len(queries) == 0:
                return None
        return r

    def start_multiple_queries(self, queries, valid_results):

        print('starting a batch quest of ', len(queries), 'for iteration', self.iteration_round)
        self.iteration_round = self.iteration_round + 1
        processes = []
        with ThreadPoolExecutor(max_workers=self.query_step) as executor:
            counter = 0
            for q in queries[:self.query_step]:  # only select the first 5 queries in the list
                counter = counter + 1
                rurl = self.endpoint + requests.utils.quote(q)
                print('the url requested', rurl)
                processes.append(executor.submit(make_request, rurl, counter, q))
                time.sleep(1)

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
        if len(valid_results) == 0:
            return None
        else:
            sorted_results = sorted(valid_results, key=lambda x: x[1])
            # print('=============== sorted result =================')
            return sorted_results[0]
