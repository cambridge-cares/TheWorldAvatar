from SPARQLWrapper import SPARQLWrapper, CSV, JSON, POST
import json 
from tqdm import tqdm
import time
import numpy as np 
import pandas as pd
import io
from tabulate import tabulate
import os
import numpy as np 
import bs4 as bs 
from requests_html import HTMLSession
import datetime
import uuid
from datetime import  timezone

def delete_loc():
    '''
    Description:
    Delete all gas instance triples 
    
    WARNING - DO NOT USE UNLESS YOU ARE SURE WHAT THIS IS DOING. 
    '''
    
    query = '''PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ns1:     <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX gasgrid: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#>
    PREFIX loc:     <http://www.bigdata.com/rdf/geospatial/literals/v1#>
    PREFIX geo:     <http://www.bigdata.com/rdf/geospatial#>
    PREFIX comp:	<http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>

    delete {?s ?p ?o}
    where {
    ?s rdf:type comp:GasTerminal.
    ?s ?p ?o.
    }'''
    
    # perform query 
    sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/ontogasgrid/sparql")
    sparql.setMethod(POST)
    sparql.setQuery(query)
    ret = sparql.query()
    return

# delete_loc()
def delete_gas_history():
    '''
    Description:
    Delete all gas instance triples 
    
    WARNING - DO NOT USE UNLESS YOU ARE SURE WHAT THIS IS DOING. 
    '''
    
    query = '''PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ns1:     <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX gasgrid: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#>
    PREFIX loc:     <http://www.bigdata.com/rdf/geospatial/literals/v1#>
    PREFIX geo:     <http://www.bigdata.com/rdf/geospatial#>
    PREFIX comp:	<http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>

    delete 
    where {
    ?gas comp:atGasVolumeRate ?p.
    ?term comp:hasTaken ?gas .
    ?gas rdf:type comp:IntakenGas .
    ?gas comp:atUTC ?s .}'''
    
    # perform query 
    sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/ontogasgrid/sparql")
    sparql.setMethod(POST)
    sparql.setQuery(query)
    ret = sparql.query()
    
    query = '''PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ns1:     <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX gasgrid: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#>
    PREFIX loc:     <http://www.bigdata.com/rdf/geospatial/literals/v1#>
    PREFIX geo:     <http://www.bigdata.com/rdf/geospatial#>
    PREFIX comp:	<http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>

    delete 
    where {
    ?gas comp:atUTC ?s .}'''
    
    # perform query 
    sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/ontogasgrid/sparql")
    sparql.setMethod(POST)
    sparql.setQuery(query)
    ret = sparql.query()
    
    query = '''PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ns1:     <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX gasgrid: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#>
    PREFIX loc:     <http://www.bigdata.com/rdf/geospatial/literals/v1#>
    PREFIX geo:     <http://www.bigdata.com/rdf/geospatial#>
    PREFIX comp:	<http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>

    delete 
    where {
    ?s comp:atGasVolumeRate ?o .}'''
    
    # perform query 
    sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/ontogasgrid/sparql")
    sparql.setMethod(POST)
    sparql.setQuery(query)
    ret = sparql.query()
    
    query = '''PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ns1:     <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX gasgrid: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#>
    PREFIX loc:     <http://www.bigdata.com/rdf/geospatial/literals/v1#>
    PREFIX geo:     <http://www.bigdata.com/rdf/geospatial#>
    PREFIX comp:	<http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>

    delete 
    where {
    ?s rdf:type comp:IntakenGas .}'''
    
    # perform query 
    sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/ontogasgrid/sparql")
    sparql.setMethod(POST)
    sparql.setQuery(query)
    ret = sparql.query()
    return
# delete_gas_history()

def delete_usage_history():
    '''
    Description:
    Delete all usage triples 

    WARNING - DO NOT USE UNLESS YOU ARE SURE WHAT THIS IS DOING. 
    '''


    query = '''PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ns1:     <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX gasgrid: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#>
    PREFIX gas: <http://www.theworldavatar.com/ontology/ontogasgrid/ontogasgrid.owl#>
    PREFIX loc:     <http://www.bigdata.com/rdf/geospatial/literals/v1#>
    PREFIX geo:     <http://www.bigdata.com/rdf/geospatial#>
    PREFIX comp:	<http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>

    delete 
    where {
       ?s rdf:type comp:OfftakenGas.
       ?r comp:hasUsed ?s.
       ?r gas:hasConsumingGasMeters ?m.
       ?s comp:hasStartUTC ?st .
       ?s comp:hasEndUTC ?en .
       ?r gas:hasNonConsumingGasMeters ?nm. 
       ?s comp:hasGasEnergy ?ge. }'''

    sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/ontogasgrid/sparql")
    sparql.setMethod(POST) # POST query, not GET
    sparql.setQuery(query)
    ret = sparql.query()

    return

# delete_usage_history()