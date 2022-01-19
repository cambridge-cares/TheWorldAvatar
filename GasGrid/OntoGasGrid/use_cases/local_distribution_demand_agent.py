import matplotlib.pyplot as plt 
import matplotlib.colors as pltc
from py4jps.resources import JpsBaseLib
import os
from tqdm import tqdm
import time
import numpy as np 
import pandas as pd
from SPARQLWrapper import SPARQLWrapper, CSV, JSON, POST
from shapely import geometry, wkt, ops 

query = '''
PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ons_t:    <http://statistics.data.gov.uk/def/statistical-geography#>
PREFIX gsp:     <http://www.opengis.net/ont/geosparql#>
PREFIX ons:    <http://statistics.data.gov.uk/id/statistical-geography/>
PREFIX abox:    <http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
PREFIX comp:    <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>
PREFIX om:       <http://www.ontology-of-units-of-measure.org/resource/om-2/>
PREFIX gns:  <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#>
PREFIX cape: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>

SELECT  ?usage ?pipename
WHERE{
  ?area rdf:type ons_t:Statistical-Geography;
     comp:hasUsed ?p;
  	 comp:hasClosestDistributionOfftake ?closest.
  ?energy om:hasPhenomenon ?p;
          om:hasValue ?val.
  ?val om:hasNumericalValue ?usage.
  ?closest comp:isConnectedToPipeline ?obj. 
  {?obj gns:hasPipeConnectionOutput ?objpipe.}
  UNION
  {?objpipe gns:entersPipeConnection ?obj.}
  {?pipeseg gns:hasStartPart ?objpipe.}
  UNION
  {?pipeseg gns:hasEndPart ?objpipe.}
  ?overall cape:hasSubsystem ?pipeseg.
  ?overall rdfs:label ?pipename.
  }
'''


DEF_NAMESPACE = 'ontogasgrid'
LOCAL_KG = "http://localhost:9999/blazegraph"
LOCAL_KG_SPARQL = LOCAL_KG + '/namespace/'+DEF_NAMESPACE+'/sparql'

sparql = SPARQLWrapper(LOCAL_KG_SPARQL)
sparql.setMethod(POST) # POST query, not GET
sparql.setQuery(query)
sparql.setReturnFormat(JSON)
ret = sparql.query().convert()
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

pipes = np.unique(res_array[:,1])
pipe_usage = [] 
for pipe in pipes:
  sum = 0 
  for val,val_pipe in res_array[:,:]:
    val = float(val)
    if val_pipe == pipe:
      sum += val
  pipe_usage.append(sum)

pipe_usage = np.array(pipe_usage)
cmap = plt.get_cmap('inferno')
pipe_usage = (pipe_usage - np.min(pipe_usage))/(np.max(pipe_usage)-np.min(pipe_usage))

pipe_usage = np.array([pipe_usage])
pipes = np.array([pipes])
pipe_data = np.concatenate((pipes,pipe_usage),axis=0).T 




jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()

jpsGW_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsGW_view,"uk.ac.cam.cares.jps.base.query.*")

KGRouter = jpsGW_view.KGRouter

queryString = """PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ns1:     <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
PREFIX gasgrid: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#>
PREFIX loc:     <http://www.blazegraph.com/rdf/geospatial/literals/v1#>
PREFIX geo:     <http://www.blazegraph.com/rdf/geospatial#>
PREFIX comp:	<http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>

SELECT ?location ?order ?label
WHERE
{
?pipe rdf:type gasgrid:GridPipeline.
?pipe rdfs:label ?label.
?pipe ns1:hasSubsystem ?segment.
?segment gasgrid:hasEndPart ?end.
?end gasgrid:entersPipeConnection ?connection.
?connection loc:lat-lon ?location.
?connection gasgrid:hasOrder ?order.
}
"""
DEF_NAMESPACE = 'ontogasgrid'
LOCAL_KG = "http://localhost:9999/blazegraph"
LOCAL_KG_SPARQL = LOCAL_KG + '/namespace/'+DEF_NAMESPACE+'/sparql'
KGClient = jpsGW_view.RemoteStoreClient(LOCAL_KG_SPARQL)
ret = KGClient.executeQuery(queryString)

# KGClient = KGRouter.getKnowledgeBaseClient('http://kb/ontogasgrid', True, False)
# ret = KGClient.executeQuery(queryString)

ret = ret.toList()
num_ret = len(ret)
ret_array = np.zeros((num_ret,4),dtype='object')
header = ['lat','lon','order','name']
for i in tqdm(range(num_ret)):
    lat,lon = ret[i]['location'].split('#')
    ret_array[i,:] = [lat,lon,float(ret[i]['order']),ret[i]['label']]
ret = pd.DataFrame(ret_array,columns=header)

unique_names = ret['name'].unique() # name of all individual pipes

# start of geoJSON file 
geojson_file = """{
"type": "FeatureCollection",
"name": "pipe_network",
"crs": { "type": "name", "properties": { "name": "urn:ogc:def:crs:OGC:1.3:CRS84" } },
"features":["""

for i in range(len(pipe_data)):
    # getting all coordinates for each unique pipe name
    name = pipe_data[i,0]
    val = pipe_data[i,1]
    rgba = cmap(val)
    hex = pltc.to_hex(rgba)

    pipe = ret.loc[ret['name']==name].values
    # sort pipe order
    sort_index = np.argsort(pipe[:,2])
    sorted_pipe = pipe[sort_index,:]
    # start of geoJSON line 
    feature_start = """{ "type": "Feature", "properties": {"name": "%s","stroke-width":5,"stroke":"%s"}, "geometry": { "type": "MultiLineString", "coordinates": [ [""" %(name,hex)
    # appending coordinates 
    for j in range(len(sort_index)):
            feature_start += "["+sorted_pipe[j,1]+","+sorted_pipe[j,0]+"],"
    # finishing line 
    feature_start = feature_start[:-1]
    feature_start += "]]}},"
    geojson_file += '\n'+feature_start

# removing last comma as is last line
geojson_file = geojson_file[:-1]
# finishing file end 
end_geojson = """
]
}
"""
geojson_file += end_geojson
# saving as geoJSON
output_folder = 'OntoGasGrid/use_cases'
geojson_written = open(output_folder+'/pipe_network_output.geojson','w')
geojson_written.write(geojson_file)
geojson_written.close() 
  