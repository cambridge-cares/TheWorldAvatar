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

# Maximum batch size for results
BATCH_SIZE = 100000

# Global GeoJSON object
FEATURES = []


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



def runQuery(kgLocation, county) :
	'''
		Runs the relevant query for the input county and returns the result.
	'''
	print("----------")

	# Get the query string
	queryString = buildQuery(county)

	gotAllResults = False
	offset = 1
	iteration = 1

	# Run query in batches
	while gotAllResults:
		print("INFO: Submitting request #" + str(iteration) + " for '" + county + "' at",  dt.now())

		# Update the queryString
		queryString += " LIMIT " + BATCH_SIZE + " OFFSET " + offset

		# Submit the query and await the result
		sparql = SPARQLWrapper(kgLocation)
		sparql.setReturnFormat(JSON)
		sparql.setQuery(queryString)
		
		try:
			# Get results 
			results = sparql.queryAndConvert()["results"]["bindings"]
			print("INFO: Request returned at",  dt.now())
			print("INFO: Found " + str(len(results)) + " more results...")

			# Process results
			onSuccess(results, county)

			# Check if we have all results
			if len(results) < BATCH_SIZE:
				gotAllResults = True
			else:
				offset += (BATCH_SIZE - 1)
				iteration += 1

		except Exception as error:
			onFailure(error, county)
	
		print("\n")

	print("INFO: Finished all queries for '" + county + "' county.")
	print("----------")



def onSuccess(data, county):
	"""
		Runs when data is successfully returned by the query.
		
		Arguments:
			data - resulting data from KG
	"""
	print("INFO: SPARQL query for '" + county + "' successful, data received.")

	# Add to global GeoJSON file
	for result in data:
		# Parse the location
		lat = float(result["location"]["value"].split("#")[0])
		lng = float(result["location"]["value"].split("#")[1])
		point = Point((lat, lng))

		# Get the IRI
		iri = result["label"]["value"]

		FEATURES.append(Feature(
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
	print("INFO: Running queries, this could take a while...")

	# Get the KG URL
	kgLocation = getKGLocation()
	print("INFO: Determined KG endpoint as '" + kgLocation + "'.")

	# Run query for each county
	runQuery(kgLocation, "CAMBRIDGESHIRE")
	runQuery(kgLocation, "NORFOLK")
	runQuery(kgLocation, "SUFFOLK")

	# Write results into single geoJSON file
	print("INFO: Queries complete, writing to file...")

	featureCollection = FeatureCollection(FEATURES)
	with open("land-use.geojson", "w") as f:
		dump(featureCollection, f)

	print("SUCCESS: Script completed.")


# Entry point, calls main function
if __name__ == "__main__":
	main()