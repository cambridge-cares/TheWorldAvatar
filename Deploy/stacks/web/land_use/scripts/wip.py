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
from geojson import Point, Feature, FeatureCollection, dump

# Local KG location (fallback)
FALLBACK_KG = "http://localhost:9999/blazegraph/"

# Global GeoJSON object
features = []


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
		PREFIX ns:<http://www.theworldavatar.com/ontology/ontocropmapgml/OntoCropMapGML.owl#>
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

	# Submit the query and await the result
	sparql = SPARQLWrapper(kgLocation)
	sparql.setReturnFormat(JSON)
	sparql.setQuery(queryString)
	print("INFO: Submitting request for '" + county + "' at",  dt.now())

	try:
		# Get results 
		result = sparql.queryAndConvert()
		print("INFO: Request returned at",  dt.now())
	except Exception as error:
		onFailure(error, county)
		
	onSuccess(result, county)



def onSuccess(data, county):
	"""
		Runs when data is successfully returned by the query.
		
		Arguments:
			data - resulting data from KG
	"""

	results = data["results"]["bindings"]
	print("INFO: SPARQL query for '" + county + "' successful, data received.")
	print("INFO: Number of results is", len(results))

	# Add to global GeoJSON file
	for result in results:

		# Parse the location
		lat = float(result["location"]["value"].split("#")[0])
		lng = float(result["location"]["value"].split("#")[1])
		point = Point((lat, lng))

		# Get te IRI
		iri = result["label"]["value"]

		features.append(Feature(
			geometry = point,
			properties = {
				"name": iri,
				"county": county.lower()
			}
		))
   		


def onFailure(error, county):
	"""
		Runs when query results in a failure/exception.
		
		Arguments:
			error - thrown errot
	"""
	print("ERROR: Could not complete SPARQL query for '" + county + "', error is as follows...")
	print("\n" + str(error) + "\n")
	sys.exit()



def main():
	"""
		Main function.
	"""
	print("\n")

	# Run query for each county
	runQuery("CAMBRIDGESHIRE")
	#runQuery("NORFOLK")
	#runQuery("SUFFOLK")

	# Write results into single geoJSON file
	featureCollection = FeatureCollection(features)
	with open("land-use.geojson", "w") as f:
		dump(featureCollection, f)

	print("SUCCESS: Script completed.")


# Entry point, calls main function
if __name__ == "__main__":
	main()