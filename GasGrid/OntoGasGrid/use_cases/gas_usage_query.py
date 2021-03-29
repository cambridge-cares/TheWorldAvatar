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

    os.system('clear')
    
    # Setting up KG Router
    # jpsBaseLibGW = JpsBaseLib()
    # jpsBaseLibGW.launchGateway()
    # jpsGW_view = jpsBaseLibGW.createModuleView()
    # jpsBaseLibGW.importPackages(jpsGW_view,"uk.ac.cam.cares.jps.base.query.*")
    # KGRouter = jpsGW_view.KGRouter

    query='''
    PREFIX ons: <http://statistics.data.gov.uk/id/statistical-geography/>
    PREFIX gcomp: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>
    PREFIX om: <http://www.ontology-of-units-of-measure.org/resource/om-2/>
    PREFIX gsp: <http://www.opengis.net/ont/geosparql#>
    PREFIX geo: <http://www.opengis.net/ont/geosparql#> 
            
    SELECT ?s ?sd ?ed ?gasused ?unit ?geom
    WHERE
    { 
    {
    ?s  gcomp:hasUsed ?gas .  
    ?gas gcomp:hasStartUTC ?sd .
    ?gas gcomp:hasEndUTC ?ed .
    ?meas om:hasPhenomenon ?gas.
    ?meas om:hasValue ?value.
    ?value om:hasNumericalValue ?gasused.
    ?value om:hasUnit ?unit.
    FILTER (?ed > "2019-06-01T12:00:00+00:00"^^xsd:dateTime)
    } 
    SERVICE <http://statistics.data.gov.uk/sparql>
    {
        ?s gsp:hasGeometry ?o .
        ?o gsp:asWKT ?geom .
    }}
    LIMIT %s
    '''%(limit)
    # KGClient = KGRouter.getKnowledgeBaseClient('http://kb/ontogasgrid',True , False)
    # ret = KGClient.executeQuery(query)
    # ret = ret.toList()
    # print(ret)

    # performing query
    sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/ontogasgrid/sparql")
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
    centroids = np.zeros((len(usage_vals),1),dtype='object')
    i = 0 
    for area in usage_vals: 
        wkt_repr = wkt.loads(area[-1])
        usage_vals[i,-1] = wkt_repr
        centroids[i,:] = [wkt_repr.centroid]
        i += 1 
    
    usage_vals = np.concatenate((usage_vals,centroids),axis=1)

    return usage_vals

def query_localdistribution():
    queryString = """
    PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ns1:     <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX gasgrid: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#>
    PREFIX loc:     <http://www.bigdata.com/rdf/geospatial/literals/v1#>
    PREFIX geo:     <http://www.bigdata.com/rdf/geospatial#>
    PREFIX comp:	<http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>

    SELECT ?location ?label
    WHERE
    {
    ?term rdf:type comp:LocalDistribution.
    ?term rdfs:label ?label.
    ?term loc:lat-lon ?location.}
    """
    sparql = SPARQLWrapper("http://www.theworldavatar.com/blazegraph/namespace/ontogasgrid/sparql")
    sparql.setMethod(POST) # POST query, not GET
    sparql.setQuery(queryString)
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
    res_array = res_array[1:,:]
    for i in range(len(res_array)):
        lon,lat = res_array[i,0].split('#')
        wkt_point = geometry.Point(float(lat),float(lon))
        res_array[i,0] = wkt_point
    return res_array

results = query_usage(limit=10)
ld_locs = query_localdistribution()
ld_points = geometry.MultiPoint(ld_locs[:,0])
area_centroids = results[:,-1]
nearest_ld = np.zeros((len(area_centroids),2),dtype='object')
i = 0 
for point in area_centroids:
    nearest_ld[i,0] = ops.nearest_points(point,ld_points)[1]
    for j in range(len(ld_locs)):
        if nearest_ld[i,0] == ld_locs[j,0]:
            nearest_ld[i,1] = ld_locs[j,1]
    i += 1 

# Location of nearest LDZ Offtake for each area centroid
# Feed back into the KG? 
print(nearest_ld[:,1])



