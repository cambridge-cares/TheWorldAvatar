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


def runQuery(kgLocation, county, features) :
	'''
		Runs the relevant query for the input county and returns the result.
	'''
	print("----------")

	# Get the query string
	queryString = buildQuery(county)

	gotAllResults = False
	offset = 1
	iteration = 1
	totalResults = 0

	# Run query in batches
	while not gotAllResults:
		print("INFO: Submitting request #" + str(iteration) + " for '" + county + "' at",  dt.now())
		
		# Update the queryString
		print("INFO: Limit is " + str(BATCH_SIZE) + ", offset is " + str(offset))
		newQuery = queryString + " LIMIT " + str(BATCH_SIZE) + " OFFSET " + str(offset)

		# Submit the query and await the result
		sparql = SPARQLWrapper(kgLocation)
		sparql.setReturnFormat(JSON)
		sparql.setQuery(newQuery)
		
		try:
			# Get results 
			results = sparql.queryAndConvert()["results"]["bindings"]
			print("INFO: Request returned at",  dt.now())
			print("INFO: Found " + str(len(results)) + " more results...")
			
			# Process results
			onSuccess(results, county, features)

			# Check if we have all results
			if len(results) < BATCH_SIZE:
				gotAllResults = True
			else:
				if totalResults == 0:
					offset += (BATCH_SIZE - 1)
				else:	
					offset += BATCH_SIZE

				iteration += 1
				totalResults += len(results)

		except Exception as error:
			onFailure(error, county)

	print("INFO: Finished all queries for '" + county + "' county.")
	print("INFO: There were a total of " + str(totalResults) + " results.")

	# Write results into single geoJSON file
	print("INFO: Queries complete, writing to file '" + county.lower() + ".geojson'")

	featureCollection = FeatureCollection(features)
	with open(county.lower() + ".geojson", "w") as f:
		dump(featureCollection, f)

	print("----------")


def onSuccess(data, county, features):
	"""
		Runs when data is successfully returned by the query.
		
		Arguments:
			data - resulting data from KG
	"""
	# Add to global GeoJSON file
	for result in data:
		# Parse the location (KG should store lat-long)
		lat = float(result["location"]["value"].split("#")[0])
		lng = float(result["location"]["value"].split("#")[1])

		# GeoJSON requires long-lat
		point = Point((lng, lat))

		# Get the IRI
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
	print("INFO: Running queries, this could take up to 30 minutes...")

	# Get the KG URL
	kgLocation = getKGLocation()
	print("INFO: Determined KG endpoint as '" + kgLocation + "'.")

	# Run query for each county
	runQuery(kgLocation, "CAMBRIDGESHIRE", [])
	runQuery(kgLocation, "NORFOLK", [])
	runQuery(kgLocation, "SUFFOLK", [])

	print("SUCCESS: Script completed.")
	exit()


# Entry point, calls main function
if __name__ == "__main__":
	main()