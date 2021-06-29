"""
	This script queries the KG for crop data for a number of
	different counties, before exporting to a GeoJSON file.
   
	Once produced, the GeoJSON file is too large to plot using
	MapBox so must be converted to MBTiles and uploaded to their
	website manually.

	Author: support<@>cmclinnovations.com
"""

import os
import sys
import json
from datetime import datetime as dt
from SPARQLWrapper import SPARQLWrapper, JSON

# Local KG location (fallback)
FALLBACK_KG = "http://localhost:9999/blazegraph/"



def getKGLocation():
	'''
		Determines the correct URL for the KG's SPARQL endpoint.
		
		Returns:
			Full URL for the KG.
	'''
	
	# Check for the KG_LOCATION environment variable, using local fallback
	kgRoot = os.getenv('KG_LOCATION', FALLBACK_KG)
	
	if kgRoot.endswith("/"):
		return kgRoot +  "namespace/ontocropmapgml/sparql"
	else:
		return kgRoot +  "/namespace/ontocropmapgml/sparql"



def buildQuery(county):
	'''
		Builds the correct SPARQL query string for the input county.

		Arguments:
			county - CAMBRIDGESHIRE, NORFOLK, or SUFFOLK

		Returns:
			Completed SPARQL string
	'''
	bounds = ""

	if county == "CAMBRIDGESHIRE":
		bounds = "Envelope_of_Crop_Map_of_England_2019_Cambridgeshire"
	elif county == "NORFOLK":
		bounds = "Envelope_of_Crop_Map_of_England_2019_Norfolk"
	elif county == "SUFFOLK":
		bounds = "Envelope_of_Crop_Map_of_England_2019_Suffolk"
	else:
		print("ERROR: Unsupported county, options are CAMBRIDGESHIRE, NORFOLK, or SUFFOLK.")
		exit()

	queryString = """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
		PREFIX vocabTerm: <http://vocab.datex.org/terms#>
		PREFIX ns: <http://www.theworldavatar.com/ontology/ontocropmapgml/OntoCropMapGML.owl#>
		PREFIX nskg: <http://www.theworldavatar.com/kb/ontocropmapgml/>
		PREFIX ontocitygml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#>

		SELECT ?location ?label
		WHERE {
			?cropMap rdf:type ns:CropMap .
			?cropMap ontocitygml:boundedBy nskg:%s .
			?cropMap vocabTerm:centrePoint ?location .
			?cropMap ns:hasLucode ?label .
		}"""%(bounds)

	return queryString



def runQuery(county) :
	'''
		Runs the relevant query for the input county and returns the result.
	'''

	# Get the KG URL
	kgLocation = getKGLocation()
	print("INFO: Determined KG endpoint as '" + kgLocation + "'.")

	# Get the query string
	queryString = buildQuery(county)

	sparql = SPARQLWrapper(kgLocation)
	sparql.setReturnFormat(JSON)
	sparql.setQuery(queryString)

	# Submit the query and await the result
	print("INFO: Submitting request at",  dt.now())

	try:
		# Get results 
		result = sparql.queryAndConvert()
		print("INFO: Request returned at",  dt.now())
	except Exception as error:
		onFailure(error)
		
	onSuccess(result)



def onSuccess(data):
	"""
		Runs when data is successfully returned by the query.
		
		Arguments:
			data - resulting data from KG
	"""

	results = data["results"]["bindings"]
	print("INFO: SPARQL query successful, data received.")
	print("INFO: Number of results is", len(results))
   
	print("SUCCESS: Script completed.")
		


def onFailure(error):
	"""
		Runs when query results in a failure/exception.
		
		Arguments:
			error - thrown errot
	"""
	print("ERROR: Could not complete SPARQL query, error is as follows...")
	print("\n" + str(error) + "\n")
	sys.exit()



def main():
	"""
		Main function.
	"""
	print("\n")
	runQuery("CAMBRIDGESHIRE")


# Entry point, calls main function
if __name__ == "__main__":
	main()