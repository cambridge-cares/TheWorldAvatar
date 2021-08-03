import time
import os 
import numpy as np 
import pandas as pd

from tqdm import tqdm
from datetime import datetime as dt
from py4jps.resources import JpsBaseLib


# Local KG location (fallback)
FALLBACK_KG = "http://localhost:9999/blazegraph/"

# Output location
OUTPUT_FOLDER = "/var/www/html/gas-grid"

# Maximum batch size for results
BATCH_SIZE = 50_000

# SPARQL query string
QUERY = """PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
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


def outputPipes():
	"""
		Queries the KG for data on pipes then outputs it
		to a GeoJSON file.
	"""
	kgClient = initialiseGateway()
	print("Using KG endpoint:", getKGLocation("ontogasgrid"))
	

	gotAllResults = False
	offset = 1
	iteration = 1
	totalResults = 0

	result = []

	# Run query in batches
	while not gotAllResults:
		print("INFO: Submitting request #" + str(iteration) + " at",  dt.now())
		print("INFO: Limit is " + str(BATCH_SIZE) + ", offset is " + str(offset))

		finalQuery = QUERY + " LIMIT " + str(BATCH_SIZE) + " OFFSET " + str(offset)
		batchResult = kgClient.executeQuery(finalQuery)
		batchResult = batchResult.toList()

		for singleResult in batchResult:
			result.append(singleResult)

		# Check if we have all results
		if len(batchResult) < BATCH_SIZE:
			gotAllResults = True
		else:
			if totalResults == 0:
				offset += (BATCH_SIZE - 1)
			else:	
				offset += BATCH_SIZE

			iteration += 1
			totalResults += len(batchResult)

	num_ret = len(result)
	ret_array = np.zeros((num_ret,4),dtype='object')
	header = ['lat','lon','order','name']

	for i in tqdm(range(num_ret)):
		lat,lon = result[i]['location'].split('#')
		ret_array[i,:] = [lat, lon, float(result[i]['order']), result[i]['label']]
	result = pd.DataFrame(ret_array, columns=header)

	unique_names = result['name'].unique() # name of all individual pipes

	# Start of geoJSON file 
	geojson_file = """{
	"type": "FeatureCollection",
	"name": "pipe_network",
	"crs": { "type": "name", "properties": { "name": "urn:ogc:def:crs:OGC:1.3:CRS84" } },
	"features":["""

	for i in range(len(unique_names)):
		# getting all coordinates for each unique pipe name
		name = unique_names[i]
		pipe = result.loc[result['name']==unique_names[i]].values
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

	# Removing last comma in last line
	geojson_file = geojson_file[:-1]

	# EOF
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
	geojson_written = open(OUTPUT_FOLDER + '/pipe_network.geojson','w')
	geojson_written.write(geojson_file)
	geojson_written.close()
	print("GeoJSON file written to:", OUTPUT_FOLDER + '/pipe_network.geojson')


# Entry point
outputPipes()