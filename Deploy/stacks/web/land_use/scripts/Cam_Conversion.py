from SPARQLWrapper import SPARQLWrapper, CSV, JSON
import json
from tqdm import tqdm
import time
import numpy as np
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
  # defining the query string given the class URI
  queryString = """

PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX vocabTerm: <http://vocab.datex.org/terms#>
PREFIX ns:<%s>
PREFIX nskg: <http://www.theworldavatar.com/kb/ontocropmapgml/>
PREFIX ontocitygml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#>


SELECT ?location ?label
WHERE {
?cropMap rdf:type ns:%s .
?cropMap ontocitygml:boundedBy nskg:Envelope_of_Crop_Map_of_England_2019_Cambridgeshire .
?cropMap vocabTerm:centrePoint ?location .
?cropMap ns:hasLucode ?label .
}

"""%(class_namespace,class_name)

  # performing SPARQL query
  sparql = SPARQLWrapper(endpoint)
  sparql.setReturnFormat(JSON)
  sparql.setQuery(queryString)
  start = time.time()
  print('Querying...')
  ret = sparql.queryAndConvert()
  end = time.time()
  # parsing JSON SPARQL results into an array
  print('Finished in ',np.round(end-start,2),' seconds')
  ret = ret['results']['bindings']
  num_ret = len(ret)
  # assigning memory to results array
  ret_array = np.zeros((num_ret,3),dtype='object')
  header = ['lat','lon','name']
  # iterating over results and allocating properties from query
  for i in tqdm(range(num_ret)):
      lat,lon = ret[i]['location']['value'].split('#')
      ret_array[i,:] = [lat,lon,ret[i]['label']['value']]
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
  geojson_written = open(class_label+'.geojson','w')
  geojson_written.write(geojson_file)
  geojson_written.close()
  return


# querying terminals from ontogasgrid
endpoint = "http://192.168.1.218:8989/blazegraph/namespace/ontocropmapgml/sparql"
class_namespace = 'http://www.theworldavatar.com/ontology/ontocropmapgml/OntoCropMapGML.owl#'
class_name = 'CropMap'
class_label = 'Cam'
query_to_geoJSON(class_namespace,class_name,class_label,endpoint)
