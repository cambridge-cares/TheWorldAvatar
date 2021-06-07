from py4jps.resources import JpsBaseLib
import os
from tqdm import tqdm
import time
import numpy as np 
import pandas as pd
from SPARQLWrapper import SPARQLWrapper, CSV, JSON, POST
from shapely import geometry, wkt, ops 

# read csv with regional code and WKT strings
df = pd.read_csv('scotland_lsoa_populations/scottish_LSOA.csv')
wkt = df['WKT'].values
code = df['DataZone'].values

# Code to upload 100 polygons at a time for speed
total = len(code)
n_compile = total / 100
remainder = total % 100
n_compile = int(n_compile)
len_query = np.zeros(n_compile+2)
for i in range(1,len(len_query)-1):
    len_query[i] = len_query[i-1] + 100
    len_query[-1] = len_query[-2] + remainder


for g in tqdm(range(len(len_query)-1)):
    i = len_query[g]
    # Start of SPARQL query
    query='''
    PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ons_t:    <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX gsp:     <http://www.opengis.net/ont/geosparql#>
    PREFIX ons:    <http://statistics.data.gov.uk/id/statistical-geography/>
    PREFIX abox:    <http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#> 

    INSERT DATA
    {{       
    '''

    middle_num = int(len_query[g+1]-len_query[g])
    # Iterating over 100 (or less) regions
    for j in range(middle_num):

        addition = 'abox:%s_geometry rdf:type gsp:Geometry . \n '%(code[int(i+j)]) # geometry instance (regional code used for URI)
        query += addition
        addition = 'ons:%s gsp:hasGeometry abox:%s_geometry . \n '%(code[int(i+j)],code[int(i+j)]) # associating region with geometry
        # NOTE: the region iteself is not defined here as it's class (statistical geography) because it was already defined 
        query += addition
        addition = 'abox:%s_geometry gsp:asWKT "%s" . \n '%(code[int(i+j)],wkt[int(i+j)]) # adding WKT string property to geometry instance
        query += addition 
    # end of SPARQL query 
    query += '}}'

    # namespace and endpoint to update triple-store
    DEF_NAMESPACE = 'ontogasgrid'
    LOCAL_KG = "http://localhost:9999/blazegraph"
    LOCAL_KG_SPARQL = LOCAL_KG + '/namespace/'+DEF_NAMESPACE+'/sparql'

    sparql = SPARQLWrapper(LOCAL_KG_SPARQL)
    sparql.setMethod(POST) # POST query, not GET
    sparql.setQuery(query)
    start = time.time()
    ret = sparql.query().convert()
    end = time.time()




