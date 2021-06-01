from py4jps.resources import JpsBaseLib
import time
import numpy as np 
import os 
import pandas as pd


def query_to_geoJSON(class_namespace,class_name,class_label,endpoint):
  '''
  DESCRIPTION: 
  Produces a .geoJSON file by querying a triple-store for a specific class
  with the attribute lat-lon from the namespace http://www.bigdata.com/rdf/geospatial/literals/v1#
  If you encode your locations using alternative methods you will have to change this.
  The .geoJSON file produced encodes single point locations and can include their attributes.

  INPUTS:
  class_URI:      The URI of the class you want to produce a geoJSON file of (with namespace)
  class_label:    The name of the class you are querying, used to name the final geoJSON file

  OUTPUTS:
  A file of the form class_label.geoJSON in the working directory. 
  '''
  jpsBaseLibGW = JpsBaseLib()
  jpsBaseLibGW.launchGateway()
  jpsGW_view = jpsBaseLibGW.createModuleView()
  jpsBaseLibGW.importPackages(jpsGW_view,"uk.ac.cam.cares.jps.base.query.*")
  KGRouter = jpsGW_view.KGRouter

  # defining the query string given the class URI
  queryString = """PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX loc:     <http://www.bigdata.com/rdf/geospatial/literals/v1#>
  PREFIX ns:   <%s>

  SELECT ?location ?label
  WHERE
  {
  ?term rdf:type ns:%s.
  ?term rdfs:label ?label.
  ?term loc:lat-lon ?location.
  }"""%(class_namespace,class_name)


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
  
 # performing SPARQL query  

  KGClient = jpsGW_view.RemoteKnowledgeBaseClient(SPARQL_STRING)
  ret = KGClient.executeQuery(queryString)

  # KGClient = KGRouter.getKnowledgeBaseClient('http://kb/ontogasgrid', True, False)
  # ret = KGClient.executeQuery(queryString)

  ret = ret.toList()
  num_ret = len(ret)
  # assigning memory to results array 
  ret_array = np.zeros((num_ret,3),dtype='object')
  header = ['lat','lon','name']
  # iterating over results and allocating properties from query 
  for i in range(num_ret):
      lat,lon = ret[i]['location'].split('#')
      ret_array[i,:] = [lat,lon,ret[i]['label']]
  ret = ret_array 
  # allocating start of .geoJSON file 
  geojson_file = """
  {
    "type": "FeatureCollection",
    "features": ["""
  # iterating over features (rows in results array)
  for i in range(num_ret):
      # creating point feature 
      feature = """{
        "type": "Feature",
        "properties": {
          "name": "%s"
        },
        "geometry": {
          "type": "Point",
          "coordinates": [
            %s,
            %s
          ]
        }
      },"""%(ret[i,2],ret[i,1],ret[i,0])
      # adding new line 
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
  geojson_written = open(OUTPUT_FOLDER+'/'+class_label+'.geojson','w')
  geojson_written.write(geojson_file)
  geojson_written.close() 
  print('Succesfully created geoJSON file')
  return 


# querying terminals from ontogasgrid
endpoint = "http://www.theworldavatar.com/blazegraph/namespace/ontogasgrid/sparql"
class_namespace = 'http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#' 
class_name = 'GasTerminal'
class_label = 'terminals'
query_to_geoJSON(class_namespace,class_name,class_label,endpoint)

