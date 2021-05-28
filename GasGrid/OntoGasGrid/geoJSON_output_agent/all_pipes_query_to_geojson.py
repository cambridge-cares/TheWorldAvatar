from tqdm import tqdm
from py4jps.resources import JpsBaseLib
import time
import os 
import numpy as np 
import pandas as pd

'''
First query location, name, and order of all PipeConnections
--------------------------------------------------------
'''

jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()

jpsGW_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsGW_view,"uk.ac.cam.cares.jps.base.query.*")

KGRouter = jpsGW_view.KGRouter

queryString = """PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX ns1:     <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
PREFIX gasgrid: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#>
PREFIX loc:     <http://www.bigdata.com/rdf/geospatial/literals/v1#>
PREFIX geo:     <http://www.bigdata.com/rdf/geospatial#>
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
       
}"""


# Possible KG locations
DEF_NAMESPACE = 'ontogasgrid'
LOCAL_KG = "http://localhost:9999/blazegraph"
CMCL_KG = "http://kg.cmclinnovations.com:81/blazegraph"

# Possible output locations
LOCAL_OUT = "OntoGasGrid/geoJSON_output_agent/geoJSON_output"
CMCL_OUT = "/var/www/html/gas-grid/"

# Determine the location of the KG using an environment variable
SPARQL_STRING = ''
TARGET_MODE = os.environ['TARGET_MODE']
print('TARGET_MODE is \'' + TARGET_MODE + '\'')

if TARGET_MODE == 'CMCL' :
	print('In CMCL mode...')
	print('    ...using KG at: ' + CMCL_KG)
	print('    ...outputting at: ' + CMCL_OUT)
	SPARQL_STRING = CMCL_KG + '/namespace/' + DEF_NAMESPACE + '/sparql'
	OUTPUT_FOLDER = CMCL_OUT
else:
	print('In LOCAL mode...')
	print('    ...using KG at: ' + LOCAL_KG)
	print('    ...outputting at: ' + LOCAL_OUT)
	SPARQL_STRING = LOCAL_KG + '/namespace/' + DEF_NAMESPACE + '/sparql'
	OUTPUT_FOLDER = LOCAL_OUT
	
	
KGClient = jpsGW_view.RemoteKnowledgeBaseClient(SPARQL_STRING)
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


for i in range(len(unique_names)):
    # getting all coordinates for each unique pipe name
    name = unique_names[i]
    pipe = ret.loc[ret['name']==unique_names[i]].values
    # sort pipe order
    sort_index = np.argsort(pipe[:,2])
    sorted_pipe = pipe[sort_index,:]
    # start of geoJSON line 
    feature_start = """{ "type": "Feature", "properties": {"name": "%s" ,"stroke":"#000000"}, "geometry": { "type": "MultiLineString", "coordinates": [ [""" % name
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

try:
  os.mkdir(OUTPUT_FOLDER)
except FileExistsError:
  print('Directory already exists')
geojson_written = open(OUTPUT_FOLDER+'/pipe_network.geojson','w')
geojson_written.write(geojson_file)
geojson_written.close() 

            
    
