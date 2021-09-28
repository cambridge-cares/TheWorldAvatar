import os
import time
import numpy as np 
import pandas as pd

from tqdm import tqdm
from datetime import datetime as dt
from py4jps.resources import JpsBaseLib


# Local KG location (fallback)
FALLBACK_KG = "http://localhost:9999/blazegraph/"

# Output location
OUTPUT_FOLDER = "/var/www/html/gas-grid"

# SPARQL Query string
QUERY = """PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
	PREFIX ns1:     <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
	PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
	PREFIX gasgrid: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#>
	PREFIX loc:     <http://www.bigdata.com/rdf/geospatial/literals/v1#>
	PREFIX geo:     <http://www.bigdata.com/rdf/geospatial#>
	PREFIX comp:	<http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>

	SELECT ?location ?label ?type ?zone ?area ?ntszone ?pipe 
	WHERE
	{
	?term rdf:type comp:%s.
	?term rdfs:label ?label.
	?term loc:lat-lon ?location.
	?term rdf:type ?type.
	?term comp:hasLinepackZone ?zone.
	?term comp:hasNTSExitArea ?area.
	?term comp:hasNTSExitZone ?ntszone.
	?term comp:isConnectedToPipeline ?pipe}"""


def initialiseGateway():
	"""
		Initialise the JPS Base Library
	"""
	jpsBaseLibGW = JpsBaseLib()
	jpsBaseLibGW.launchGateway()

	jpsBaseLibView = jpsBaseLibGW.createModuleView()
	jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.query.*")
	return jpsBaseLibView.RemoteStoreClient(getKGLocation("ontogasgrid"))

		
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


def outputOfftakes():

	# Initialise the JPS gateway
	kgClient = initialiseGateway()
	print("INFO: Using KG endpoint at", getKGLocation("ontogasgrid"))

	# start of geoJSON file 
	geojson_file = """
	{
	"type": "FeatureCollection",
	"features": ["""

	offtake_types = ['LocalDistribution','PowerStation','IndustrialUser','Storage']
	colors = ['#f78086','#ca3549','#5c1d20','#f9372d']

	for i in range(len(offtake_types)):
		offtake_type = offtake_types[i]
		color = colors[i]
		queryString = QUERY%(offtake_type)
		
		print("INFO: Submitting request " + str(i + 1) + "/" + str(len(offtake_types)) + " at",  dt.now())
		ret = kgClient.executeQuery(queryString)

		ret = ret.toList()
		num_ret = len(ret)
		ret_array = np.zeros((num_ret,8),dtype='object')
		header = ['lat','lon','name','type','zone','area','ntszone','pipe']
		for i in tqdm(range(num_ret)):
			try:
				lat,lon = ret[i]['location'].split('#')
				ret_array[i,:] = [lat,lon+',',ret[i]['label'],ret[i]['type'],ret[i]['zone'],ret[i]['area'],ret[i]['ntszone'],ret[i]['pipe']]
			except:
				ret_array[i,:] = ['','',ret[i]['label'],ret[i]['type'],ret[i]['zone'],ret[i]['area'],ret[i]['ntszone'],ret[i]['pipe']]
		ret = pd.DataFrame(ret_array,columns=header).values
		

		# <http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/Langholm>

		for i in range(num_ret):
			if len(ret[i,0]) > 1:
				new_array = [color,ret[i,2],offtake_type,ret[i,4],ret[i,5],ret[i,6],ret[i,7],ret[i,1],ret[i,0]]
				for j in range(len(new_array)):
					if new_array[j] is None:
						new_array[j] = 'N/A'
				feature = """{
					"type": "Feature",
					"properties": {
					"marker-color": "%s",
					"marker-size": "medium",
					"marker-symbol": "",
					"Offtake Point (License Name)": "%s",
					"Type of Offtake": "%s",
					"Linepack Zone": "%s",
					"NTS Exit Area": "%s",
					"NTS Exit Zone": "%s",
					"Connected to Pipeline": "%s"
					},
					"geometry": {
					"type": "Point",
					"coordinates": [
						%s
						%s
					]
					}
				},"""%(new_array[0],new_array[1],new_array[2],new_array[3],new_array[4],new_array[5],new_array[6],new_array[7],new_array[8])
				geojson_file += '\n'+feature

			else:
				feature = """{
					"type": "Feature",
					"properties": {
					"marker-color": "%s",
					"marker-size": "medium",
					"marker-symbol": "",
					"Offtake Point (License Name)": "%s",
					"Type of Offtake": "%s",
					"Linepack Zone": "%s",
					"NTS Exit Area": "%s",
					"NTS Exit Zone": "%s",
					"Connected to Pipeline": "%s"
					},
					"geometry": {
					"type": "Polygon",
					"coordinates": []
					}
				},"""%(new_array[0],new_array[1],new_array[2],new_array[3],new_array[4],new_array[5],new_array[6])
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
	geojson_written = open(OUTPUT_FOLDER + '/offtakes.geojson','w')
	geojson_written.write(geojson_file)
	geojson_written.close()
	print("GeoJSON file written to:", OUTPUT_FOLDER + '/offtakes.geojson')

            
# Entry point
outputOfftakes()