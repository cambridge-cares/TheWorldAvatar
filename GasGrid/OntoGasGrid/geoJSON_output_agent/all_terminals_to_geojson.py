
import time
import numpy as np 
import os 
import pandas as pd

from datetime import datetime as dt
from py4jps.resources import JpsBaseLib


# Local KG location (fallback)
FALLBACK_KG = "http://localhost:9999/blazegraph/"

# Output location
OUTPUT_FOLDER = "/var/www/html/gas-grid"

# SPARQL query string
QUERY = """PREFIX rdf:	<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  PREFIX rdfs:	<http://www.w3.org/2000/01/rdf-schema#>
  PREFIX loc:	<http://www.bigdata.com/rdf/geospatial/literals/v1#>
  PREFIX ns:	<http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>

  SELECT ?location ?label
  WHERE
  {
  ?term rdf:type ns:GasTerminal.
  ?term rdfs:label ?label.
  ?term loc:lat-lon ?location.
  }"""


def initialiseGateway():
	"""
		Initialise the JPS Base Library
	"""
	jpsBaseLibGW = JpsBaseLib()
	jpsBaseLibGW.launchGateway()

	jpsBaseLibView = jpsBaseLibGW.createModuleView()
	jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.query.*")
	return jpsBaseLibView.RemoteKnowledgeBaseClient(getKGLocation("ontogasgrid"))


def getKGLocation(namespace):
	"""
		Determines the correct URL for the KG's SPARQL endpoint.
		
		Arguments:
			namespace - KG namespace.
			
		Returns:
			Full URL for the KG.
	"""
	
	# Check for the KG_LOCATION environment variable, using local fallback
	kgRoot = os.getenv('KG_LOCATION', FALLBACK_KG)
	
	if kgRoot.endswith("/"):
		return kgRoot +  "namespace/" + namespace + "/sparql"
	else:
		return kgRoot +  "/namespace/" + namespace + "/sparql"


def outputTerminals():
	kgClient = initialiseGateway()
	print("INFO: Using KG endpoint at", getKGLocation("ontogasgrid"))

	ret = kgClient.executeQuery(QUERY)

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

	# Saving as geoJSON
	try:
		global OUTPUT_FOLDER
		os.mkdir(OUTPUT_FOLDER)
	except FileExistsError:
		print('Directory already exists')
	except:
		OUTPUT_FOLDER = "."

	print("Writing GeoJSON file...")
	geojson_written = open(OUTPUT_FOLDER + '/terminals.geojson','w')
	geojson_written.write(geojson_file)
	geojson_written.close()
	print("GeoJSON file written to:", OUTPUT_FOLDER + '/terminals.geojson')


# Entry point
outputTerminals()
