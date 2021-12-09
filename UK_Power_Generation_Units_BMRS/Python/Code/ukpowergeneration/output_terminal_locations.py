"""
    This script queries the locations of all Gas Terminals within the KG and writes the results
	to a local GeoJSON file. When deployed as part of the Gas Grid Agent, cron will run this
	script once per day, the website visualisation will then use wget to download it. This ensures
	that if any new Terminals are added to the KG, the visualisation can plot them without
	any manual changes needed.

    Local deployment requires:
        - The gasgridagent.properties file to be set correctly.
        - Triple store endpoint to contain Gas Terminals in expected format.

    Authors:
		- trs53<@>cam.ac.uk
		- mdhillman<@>cmclinnovations.com

	Future improvements:
		- If this script fails (KG endpoint inaccessible, KG response is wrong format, local 
		  memory issues etc.), it should use the EmailSender class to notify developers of the
		   issue. This will require some setup (perhaps in the Python Wrapper).
"""

import os 
import numpy as np 

# Get the jpsBaseLibGateWay instance from the jpsSingletons module
from gasgridagent.jpsSingletons import jpsBaseLibView

# Get settings and functions from kg_utils module
import gasgridagent.kg_utils as kg

# SPARQL query string
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


def outputTerminals():
    """
        Queries the loction of all Gas Terminals from the KG and
        outputs it to a GeoJSON file.
    """
    
    # Read properties file
    kg.read_properties_file(kg.PROPERTIES_FILE)

    # Set URLs to KG SPARQL endpoints (and update properties file accordingly)
    kg.setKGEndpoints(kg.PROPERTIES_FILE)

    # Get the KG endpoint
    print("Getting Gas Terminal locations from endpoint at:", kg.QUERY_ENDPOINT)
    kgClient = jpsBaseLibView.RemoteStoreClient(kg.QUERY_ENDPOINT)

    # Run the query
    ret = kgClient.executeQuery(QUERY)
    ret = ret.toList()
    num_ret = len(ret)

    # Assigning memory to results array 
    ret_array = np.zeros((num_ret,3),dtype='object')

    # Iterating over results and allocating properties from query 
    for i in range(num_ret):
        lat,lon = ret[i]['location'].split('#')
        ret_array[i,:] = [lat,lon,ret[i]['label']]
    ret = ret_array 

    # Allocating start of .geoJSON file 
    geojson_file = """
    {
    "type": "FeatureCollection",
    "features": ["""

    # Iterating over features (rows in results array)
    for i in range(num_ret):

        # Creating point feature 
        feature = """{
        "type": "Feature",
        "properties": {
            "name": "%s"
        },
        "id": %s,
        "geometry": {
            "type": "Point",
            "coordinates": [
            %s,
            %s
            ]
        }
        },"""%(ret[i,2], i, ret[i,1], ret[i,0])

        # Adding new line 
        geojson_file += '\n'+feature

    # Removing last comma as is last line
    geojson_file = geojson_file[:-1]

    # Finishing file end 
    end_geojson = "]}"
    geojson_file += end_geojson

    # Saving as geoJSON
    outputDirectory = kg.OUTPUT_DIR
    try:
        os.mkdir(outputDirectory)
    except FileExistsError:
        print('Directory already exists, will use that.')
    except:
        outputDirectory = "."

	# Write the GeoJSON file
    print("Writing GeoJSON file...")
    geojson_written = open(outputDirectory + '/terminals.geojson','w')
    geojson_written.write(geojson_file)
    geojson_written.close()
    print("GeoJSON file written to:", outputDirectory + '/terminals.geojson')


# ===== ENTRY POINT ====
try:
    outputTerminals()
except:
    sender = jpsBaseLibView.EmailSender()
    sender.sendEmail(
        "GasGridAgent - Exception when outputting terminal locations.",
        """
            The 'output_terminal_locations.py' script of the GasGridAgent has encountered an Exception. This script will continue to execute daily, as the issue may be temporary.
            \n\n
            It is recommended that a developer logs into the relevant VM and checks the logs for the gas-grid-agent Docker container.
        """
    )