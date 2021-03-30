from py4jps.resources import JpsBaseLib
import os
from tqdm import tqdm
import time
import numpy as np 
import pandas as pd
from SPARQLWrapper import SPARQLWrapper, CSV, JSON, POST
from shapely import geometry, wkt, ops 

def query_usage(limit):
    '''
    Querying the KG for all regions gas-usages in 2019 
    '''
    if limit == False:
        limit = str(100000000)
    limit = str(limit)
    # clearing terminal
    os.system('clear')
    
    # # Setting up KG Router
    # jpsBaseLibGW = JpsBaseLib()
    # jpsBaseLibGW.launchGateway()
    # jpsGW_view = jpsBaseLibGW.createModuleView()
    # jpsBaseLibGW.importPackages(jpsGW_view,"uk.ac.cam.cares.jps.base.query.*")
    # KGRouter = jpsGW_view.KGRouter

    query='''
    PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ons_t:    <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX gsp:     <http://www.opengis.net/ont/geosparql#>

    SELECT ?s ?geom
    WHERE
    {       
    ?s rdf:type ons_t:Statistical-Geography.
    OPTIONAL{ ?s gsp:hasGeometry ?o.
              ?o gsp:asWKT ?geom}
    }
    '''
    # KGClient = KGRouter.getKnowledgeBaseClient('http://kb/ontogasgrid',True , False)
    # ret = KGClient.executeQuery(query)
    # ret = ret.toList()
    # print(ret)

    # performing query
    # jpsBaseLibGW = JpsBaseLib()
    # jpsBaseLibGW.launchGateway()
    # jpsGW_view = jpsBaseLibGW.createModuleView()
    # jpsBaseLibGW.importPackages(jpsGW_view,"uk.ac.cam.cares.jps.base.query.*")
    # KGRouter = jpsGW_view.KGRouter
    DEF_NAMESPACE = 'ontogasgrid'
    LOCAL_KG = "http://localhost:9999/bigdata"
    LOCAL_KG_SPARQL = LOCAL_KG + '/namespace/'+DEF_NAMESPACE+'/sparql'
    # KGClient = jpsGW_view.RemoteKnowledgeBaseClient(LOCAL_KG_SPARQL)
    # ret = KGClient.executeQuery(query)

    # Querying using SPARQLWrapper for now
    sparql = SPARQLWrapper(LOCAL_KG_SPARQL)
    sparql.setMethod(POST) # POST query, not GET
    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    print('Starting Gas Usage Query...')
    start = time.time()
    ret = sparql.query().convert()
    end = time.time()
    print('Finished in a time of ',np.round(end-start,3),' seconds')

    # parsing JSON into an array 
    values = ret['results']['bindings']
    head = ret['head']['vars']
    res_array = np.zeros((len(values)+1,len(head)),dtype='object')
    res_array[0,:] = head
    i = 1
    for row in values:
        j = 0 
        for val in row.values():
            res_array[i,j] = val['value']
            j += 1 
        i += 1 

    usage_vals = res_array[1:,:]

    # preassigning centroid array
    centroids = np.zeros((len(usage_vals),1),dtype='object')
    i = 0 
    del_index = [] 
    # iterating over WKT Literals
    for area in usage_vals: 
        # loading WKT
        try:
            wkt_repr = wkt.loads(area[-1])
            usage_vals[i,-1] = wkt_repr
            # calculating and storing centroid
            centroids[i,:] = [wkt_repr.centroid]
            i += 1 
        except TypeError: 
            del_index.append(i)
            usage_vals[i,-1] = 'None'
            centroids[i,:] = ['None']
            i += 1 
    # concatenating results 
    usage_vals = np.concatenate((usage_vals,centroids),axis=1)
    usage_vals = np.delete(usage_vals,del_index,axis=0)
    return usage_vals

def query_localdistribution():
    '''
    Querying location of local distributions
    '''
    query = """
    PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ns1:     <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX gasgrid: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#>
    PREFIX loc:     <http://www.bigdata.com/rdf/geospatial/literals/v1#>
    PREFIX geo:     <http://www.bigdata.com/rdf/geospatial#>
    PREFIX comp:	<http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>

    SELECT ?location ?term
    WHERE
    {
    ?term rdf:type comp:LocalDistribution.
    ?term rdfs:label ?label.
    ?term loc:lat-lon ?location.}
    """
    # jpsBaseLibGW = JpsBaseLib()
    # jpsBaseLibGW.launchGateway()
    # jpsGW_view = jpsBaseLibGW.createModuleView()
    # jpsBaseLibGW.importPackages(jpsGW_view,"uk.ac.cam.cares.jps.base.query.*")
    # KGRouter = jpsGW_view.KGRouter
    DEF_NAMESPACE = 'ontogasgrid'
    LOCAL_KG = "http://localhost:9999/bigdata"
    LOCAL_KG_SPARQL = LOCAL_KG + '/namespace/'+DEF_NAMESPACE+'/sparql'
    # KGClient = jpsGW_view.RemoteKnowledgeBaseClient(LOCAL_KG_SPARQL)
    # ret = KGClient.executeQuery(query)

    sparql = SPARQLWrapper(LOCAL_KG_SPARQL)
    sparql.setMethod(POST) # POST query, not GET
    sparql.setQuery(query)
    sparql.setReturnFormat(JSON)
    print('Starting Local Distribution Offtake Query...')
    start = time.time()
    ret = sparql.query().convert()
    end = time.time()
    print('Finished in a time of ',np.round(end-start,3),' seconds')

    # parsing JSON into an array 
    values = ret['results']['bindings']
    head = ret['head']['vars']
    res_array = np.zeros((len(values)+1,len(head)),dtype='object')
    res_array[0,:] = head
    i = 1
    for row in values:
        j = 0 
        for val in row.values():
            res_array[i,j] = val['value']
            j += 1 
        i += 1 
    # parsing blazegraph location to shapely shape.
    res_array = res_array[1:,:]
    for i in range(len(res_array)):
        lon,lat = res_array[i,1].split('#')
        # parsing as point
        wkt_point = geometry.Point(float(lat),float(lon))
        res_array[i,1] = wkt_point
    return res_array


def area_nearest_dist(area_limit):
    '''
    Taking centroids and LDZs, 
    assigning nearest LDZ to each centroid,
    returning array of ONS URI and nearest LDZ
    '''
    # getting points
    results = query_usage(limit=area_limit)
    ld_locs = query_localdistribution()
    # set of points as LDZ
    ld_points = geometry.MultiPoint(ld_locs[:,1])
    area_centroids = results[:,-1]
    # preassigning results array
    nearest_ld = np.zeros((len(area_centroids),2),dtype='object')
    i = 0 
    print('Calculating nearest Local Distribution Offtakes')
    # calculating nearest LDZ for each LSOA offtake
    for point in tqdm(area_centroids):
        # nearest
        nearest_ld[i,0] = ops.nearest_points(point,ld_points)[1]
        for j in range(len(ld_locs)):
            # checking to get name
            if nearest_ld[i,0] == ld_locs[j,1]:
                nearest_ld[i,1] = ld_locs[j,0]
        nearest_ld[i,0] = results[i,0]
        i += 1 

    query = """
PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ns1:     <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
PREFIX gasgrid: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#>
PREFIX loc:     <http://www.bigdata.com/rdf/geospatial/literals/v1#>
PREFIX ons:     <http://statistics.data.gov.uk/id/statistical-geography/>
PREFIX geo:     <http://www.bigdata.com/rdf/geospatial#>
PREFIX comp:	<http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>
PREFIX abox:    <http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/>

INSERT DATA
{
    """
    for row in nearest_ld:
        additional ='ons:'+row[0].split('/')[-1]+' comp:hasClosestDistributionOfftake ' + 'abox:'+row[1].split('/')[-1] + ' .  \n '
        query += additional
    query += '}'
    DEF_NAMESPACE = 'ontogasgrid'
    LOCAL_KG = "http://localhost:9999/bigdata"
    LOCAL_KG_SPARQL = LOCAL_KG+'/namespace/'+DEF_NAMESPACE+'/sparql'

    sparql = SPARQLWrapper(LOCAL_KG_SPARQL)
    sparql.setMethod(POST) # POST query, not GET
    sparql.setQuery(query)
    print('Adding link between ONS areas and Local Distribution Offtakes')
    start = time.time()
    ret = sparql.query()
    end = time.time()
    print('Finished in a time of ',np.round(end-start,3),' seconds')
    return

area_nearest_dist(area_limit=False)





