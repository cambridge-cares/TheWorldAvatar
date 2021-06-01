from py4jps.resources import JpsBaseLib
import os
from tqdm import tqdm
import time
import numpy as np 
import pandas as pd


# start of geoJSON file 
geojson_file = """
{
  "type": "FeatureCollection",
  "features": ["""

offtake_types = ['LocalDistribution','PowerStation','IndustrialUser','Storage']
colors = ['#f78086','#ca3549','#5c1d20','#f9372d']

jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()

jpsGW_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsGW_view,"uk.ac.cam.cares.jps.base.query.*")

KGRouter = jpsGW_view.KGRouter

for i in range(len(offtake_types)):
  offtake_type = offtake_types[i]
  color = colors[i]
  queryString = """PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  PREFIX ns1:     <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
  PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX gasgrid: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#>
  PREFIX loc:     <http://www.bigdata.com/rdf/geospatial/literals/v1#>
  PREFIX geo:     <http://www.bigdata.com/rdf/geospatial#>
  PREFIX comp:	<http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>

  SELECT ?location ?label
  WHERE
  {
  ?term rdf:type comp:%s.
  ?term rdfs:label ?label.
  ?term loc:lat-lon ?location.
  }"""%(offtake_type)

  
  # Possible KG locations
  DEF_NAMESPACE = 'ontogasgrid'
  LOCAL_KG = "http://localhost:9999/blazegraph"
  CMCL_KG = "http://kg.cmclinnovations.com:81/blazegraph"

  # Possible output locations
  LOCAL_OUT = "OntoGasGrid/geoJSON_output_agent/geoJSON_output"
  CMCL_OUT = "/var/www/html/gas-grid/"

  # Determine the location of the KG using an environment variable
  SPARQL_STRING = ''
  TARGET_MODE = os.getenv('TARGET_MODE', 'LOCAL')

  if TARGET_MODE is None or TARGET_MODE != 'CMCL' :
      print('In LOCAL mode...')
      print('    ...using KG at: ' + LOCAL_KG)
      print('    ...outputting at: ' + LOCAL_OUT)
      SPARQL_STRING = LOCAL_KG + '/namespace/' + DEF_NAMESPACE + '/sparql'
      OUTPUT_FOLDER = LOCAL_OUT
  else:
      print('In CMCL mode...')
      print('    ...using KG at: ' + CMCL_KG)
      print('    ...outputting at: ' + CMCL_OUT)
      SPARQL_STRING = CMCL_KG + '/namespace/' + DEF_NAMESPACE + '/sparql'
      OUTPUT_FOLDER = CMCL_OUT
	
  KGClient = jpsGW_view.RemoteKnowledgeBaseClient(SPARQL_STRING)
  ret = KGClient.executeQuery(queryString)

  # KGClient = KGRouter.getKnowledgeBaseClient('http://kb/ontogasgrid', True, False)
  # ret = KGClient.executeQuery(queryString)

  ret = ret.toList()
  num_ret = len(ret)
  ret_array = np.zeros((num_ret,3),dtype='object')
  header = ['lat','lon','name']
  for i in tqdm(range(num_ret)):
      try:
          lat,lon = ret[i]['location'].split('#')
          ret_array[i,:] = [lat,lon+',',ret[i]['label']]
      except:
          ret_array[i,:] = ['','',ret[i]['label']]
  ret = pd.DataFrame(ret_array,columns=header).values
  

  # <http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/Langholm>

  for i in range(num_ret):
    if len(ret[i,0]) > 1:

      feature = """{
        "type": "Feature",
        "properties": {
          "marker-color": "%s",
          "marker-size": "medium",
          "marker-symbol": "",
          "Offtake Point (License Name)": "%s",
          "Type of Offtake": "%s"
        },
        "geometry": {
          "type": "Point",
          "coordinates": [
            %s
            %s
          ]
        }
      },"""%(color,ret[i,2],offtake_type,ret[i,1],ret[i,0])
      geojson_file += '\n'+feature

    else:
      feature = """{
        "type": "Feature",
        "properties": {
          "marker-color": "%s",
          "marker-size": "medium",
          "marker-symbol": "",
          "Offtake Point (License Name)": "%s",
          "Type of Offtake": "%s"
        },
        "geometry": {
          "type": "Polygon",
          "coordinates": []
        }
      },"""%(color,ret[i,2],offtake_type)
      geojson_file += '\n'+feature

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

geojson_written = open(OUTPUT_FOLDER+'/offtakes.geojson','w')
geojson_written.write(geojson_file)
geojson_written.close() 

            
    
