#!/usr/bin/python3

import _thread
import json
import time

# # Define a function for the thread
# def print_time( threadName, delay):
#    count = 0
#    while count < 5:
#       time.sleep(delay)
#       count += 1
#       print ("%s: %s" % ( threadName, time.ctime(time.time()) ))
#
# # Create two threads as follows
# try:
#    _thread.start_new_thread( print_time, ("Thread-1", 2, ) )
#    _thread.start_new_thread( print_time, ("Thread-2", 4, ) )
# except:
#    print ("Error: unable to start thread")
#
# while 1:
#     pass
from pprint import pprint

import requests
from concurrent.futures import ThreadPoolExecutor, as_completed

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

query = '''
SELECT ?oLabel ?v ?unitLabel
        WHERE {
        ?o wdt:P31 wd:Q159226 . 
        ?o wdt:P2102 ?x .  
        ?o p:P361/psv:P361 ?value .  
        ?value wikibase:quantityAmount ?v .
        ?value wikibase:quantityUnit ?unit .
        SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
        FILTER(?x > -100 ).  }  
'''
url_list = [q2, query]


def make_request(_url):
    headers = {'Accept': 'application/sparql-results+json'}
    html = requests.get(_url, stream=True, headers=headers)
    return html.content.decode('utf-8')


def start_multiple_queries(queries):
    processes = []
    with ThreadPoolExecutor(max_workers=10) as executor:
        for q in queries:
            rurl = 'https://query.wikidata.org/sparql?format=json&query=' + requests.utils.quote(q)
            processes.append(executor.submit(make_request, rurl))
            time.sleep(1)

    for task in as_completed(processes):
        identity_valid_result(json.loads(task.result()))


def identity_valid_result(result):
    r = (result['results']['bindings'])
    if r is []:
        return None
    else:
        return r


start_multiple_queries(url_list)
