"""
    Demo script to show the usage of the elevations.py script. This demo queries all Gas
	Terminals within the KG, gets their locations, determines their elevations, then
	writes the results to a local CSV file.

	Note: As each elevation calculation requires a call to a MapBox API (with the below key),
	be aware that is may use up your monthly API calls quota. For CMCL, this means that if we
	need to run the elevation script on many points, we may need to become premium MapBox members.

    Requires:
        - A valid MapBox API key

    Authors:
        - mdhillman<@>cmclinnovations.com
"""

import elevations
from tqdm import tqdm
from py4jps.resources import JpsBaseLib


# MapBox API Key
MAPBOX_KEY = "YOUR-API-KEY-HERE"

# KG Endpoint
ENDPOINT = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ontogasgrid/sparql"

# SPARQL Query
QUERY = """
    PREFIX rdf:    <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX loc:    <http://www.bigdata.com/rdf/geospatial/literals/v1#>
    PREFIX ns:    <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>

    SELECT ?location ?label
    WHERE
    {
        ?term rdf:type ns:GasTerminal.
        ?term rdfs:label ?label.
        ?term loc:lat-lon ?location.
    }"""

# Initialise JPS Code
jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()
jpsBaseLibView = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLibView, "uk.ac.cam.cares.jps.base.query.*")

# Query and get results
kgClient = jpsBaseLibView.RemoteStoreClient(ENDPOINT)
ret = kgClient.executeQuery(QUERY)
ret = ret.toList()

# Open a file handle
outputFile = open("elevations.csv", "w")
outputFile.write("Name,Latitude,Longitude,Height [m]\n")

# Check each query result
for i in tqdm(range(len(ret))):
	lat,lon = ret[i]['location'].split('#')
	name = ret[i]['label']

	# Get the elevation
	height = elevations.getElevation(MAPBOX_KEY, lat, lon)

	# Output to file
	outputFile.write(name)
	outputFile.write(",")
	outputFile.write(lat)
	outputFile.write(",")
	outputFile.write(lon)
	outputFile.write(",")
	outputFile.write(str(height))
	outputFile.write("\n")

# Close file and finish
outputFile.close()
print("Elevation demo finished, please see 'elevations.csv' for results.")
