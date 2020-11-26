##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 24 Nov 2020                      #
##########################################

"""The query_endpoint function can operate on any triple store via an endpoint URL to
retrieve both data (e.g., instances) and semantics (e.g., classes and relationships
of instances), describing a domain of interest."""

from SPARQLWrapper import SPARQLWrapper as sparql
from SPARQLWrapper import JSON as json

def query_endpoint(endpoint, query):
    s = sparql(endpoint)
    s.setQuery(query)
    s.setReturnFormat(json)
    results = s.query().convert()
    return results