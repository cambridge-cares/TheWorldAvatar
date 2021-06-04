"""
	This script queries the KG for the last day of flow data for
	every terminal and offtake, this data is then written to a
	local JSON file.
	
	It comprises part of the Gas Grid Agent tool and should be 
	executed daily via crontab.
	
	Author: support<@>cmclinnovations.com
"""

from py4jps.resources import JpsBaseLib
import os 
import sys
import datetime

# Output directory for JSON files
OUTPUT_DIR = "/var/www/html/gas-grid"

# Local KG location (fallback)
FALLBACK_KG = "http://localhost:9999/blazegraph/"


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


def buildQuery():
	"""
		Builds the SPARQL query that will be submitted to the KG.
					
		Returns:
			Built SPARQL query (without KG URL)
	"""
	
	queryString ="""PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
	PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
	PREFIX gas: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>
	PREFIX om: 	<http://www.ontology-of-units-of-measure.org/resource/om-2/>

	SELECT ?s ?label ?UTC ?num_val
	WHERE
	{
		?s rdf:type gas:GasTerminal.
		?s rdfs:label ?label.
		?s gas:hasTaken ?taken.
		?phenom om:hasPhenomenon ?taken. 
		?taken gas:atUTC ?UTC.
		?phenom om:hasValue ?val.
		?val om:hasNumericalValue ?num_val
	}"""
	return queryString


def submitQuery(namespace, callbackSuccess, callbackFailure):
	"""
		Submits the input query to the KG asynchronously (so a timeout can be added).
		Once the request is complete, one of the input callback functions
		will be called.
		
		Arguments:
			namespace 		- KG namespace
			callbackSuccess - function to call on success
			callbackFailure - function to call on failure
	"""
	
	# Get the KG URL
	kgLocation = getKGLocation(namespace)
	print("INFO: Determined KG endpoint as '" + kgLocation + "'.")
	
	# Get the query string
	queryString = buildQuery()

	# Initialise JPS base library
	jpsBaseLib = JpsBaseLib()
	jpsBaseLib.launchGateway()
	
	# Load the Query classes
	jpsView = jpsBaseLib.createModuleView()
	jpsBaseLib.importPackages(jpsView, "uk.ac.cam.cares.jps.base.query.*")
	
	# Create instances
	KGRouter = jpsView.KGRouter
	KGClient = jpsView.RemoteKnowledgeBaseClient(kgLocation)
		
	# Submit the query and await the result
	today = datetime.datetime.now()
	print("INFO: Submitting request at ", today)
	
	try:
		result = KGClient.executeQuery(queryString)
		callbackSuccess(result.toList())
	except Exception as error:
		callbackFailure(error)


def onSuccess(data):
	"""
		Runs when data is successfully returned by the query.
		
		Arguments:
			data - resulting data from KG
	"""
	print("INFO: SPARQL query successful, data received.")
	print("INFO: Type of returned data is ", type(data))
	
	# TODO: Write this to a reasonable file format that the
	#		visualisation could read (JSON maybe?).
	
	numResults = len(data)
	print("Number of results is ", len(data))
	
	with open("result.txt", "w") as file:
		file.write(str(data))
		file.close()
		

def onFailure(error):
	"""
		Runs when query results in a failure/exception.
		
		Arguments:
			error - thrown errot
	"""
	print("ERROR: Could not complete SPARQL query, error is as follows...")
	print("\n")
	print(error)
	print("\n")
	


def main():
	"""
		Main function.
	"""
	
	# Execute main logic
	submitQuery("ontogasgrid", onSuccess, onFailure)


# Entry point, calls main function
if __name__ == "__main__":
	main()