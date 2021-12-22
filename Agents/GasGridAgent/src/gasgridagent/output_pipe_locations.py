"""
    This script queries the locations of all Gas Pipes within the KG and writes the results
    to a local GeoJSON file. When deployed as part of the Gas Grid Agent, cron will run this
    script once per day, the website visualisation will then use wget to download it. This ensures
    that if any new Pipes are added to the KG, the visualisation can plot them without
    any manual changes needed.

    Local deployment requires:
        - The gasgridagent.properties file to be set correctly.
        - Triple store endpoint to contain Gas Pipes in expected format.

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
import pandas as pd

from tqdm import tqdm
from datetime import datetime as dt

# Get the jpsBaseLibGateWay instance from the jpsSingletons module
from jpsSingletons import jpsBaseLibView

# Get settings and functions from kg_utils module
import kg_utils as kg

# Maximum batch size for results
BATCH_SIZE = 50_000
    
# SPARQL query string
QUERY = """
    PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ns1:     <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX gasgrid: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#>
    PREFIX loc:     <http://www.bigdata.com/rdf/geospatial/literals/v1#>
    PREFIX geo:     <http://www.bigdata.com/rdf/geospatial#>
    PREFIX comp:    <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>

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


def outputPipes():
    """
        Queries the loction of all Gas Pipes from the KG and
        outputs it to a GeoJSON file.
    """

     # Read properties file
    kg.read_properties_file(kg.PROPERTIES_FILE)

    # Set URLs to KG SPARQL endpoints (and update properties file accordingly)
    kg.setKGEndpoints(kg.PROPERTIES_FILE)

    # Get the KG endpoint
    print("Getting Gas Pipe locations from endpoint at:", kg.QUERY_ENDPOINT)
    kgClient = jpsBaseLibView.RemoteStoreClient(kg.QUERY_ENDPOINT)
    
    gotAllResults = False
    offset = 1
    iteration = 1
    totalResults = 0

    result = []

    # Run query in batches (too many pipes for single query)
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
        # Getting all coordinates for each unique pipe name
        name = unique_names[i]
        pipe = result.loc[result['name']==unique_names[i]].values

        # Sort pipe order
        sort_index = np.argsort(pipe[:,2])
        sorted_pipe = pipe[sort_index,:]

        if len(sorted_pipe) != 1:

            # Start of geoJSON line 
            feature_start = """{ 
				"type": "Feature", 
				"properties": {
					"name": "%s" ,
					"stroke":"#000000"
					},
				"id": %s,
				"geometry": { 
					"type":
					"MultiLineString",
					"coordinates": [ [""" % (name, i)
           
		    # Appending coordinates 
            for j in range(len(sort_index)):
                    feature_start += "["+sorted_pipe[j,1]+","+sorted_pipe[j,0]+"],"
            
			# Finishing line 
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
    outputDirectory = kg.OUTPUT_DIR
    try:
        os.mkdir(outputDirectory)
    except FileExistsError:
        print('Directory already exists, will use that.')
    except:
        outputDirectory = "."

    # Output the GeoJSON file
    print("Writing GeoJSON file...")
    geojson_written = open(outputDirectory + '/pipes.geojson','w')
    geojson_written.write(geojson_file)
    geojson_written.close()
    print("GeoJSON file written to:", outputDirectory + '/pipes.geojson')


# ===== ENTRY POINT ====
try:
    outputPipes()
except:
    sender = jpsBaseLibView.EmailSender()
    sender.sendEmail(
        "GasGridAgent - Exception when outputting pipe locations.",
        """
            The 'output_pipe_locations.py' script of the GasGridAgent has encountered an Exception. This script will continue to execute daily, as the issue may be temporary.
            \n\n
            It is recommended that a developer logs into the relevant VM and checks the logs for the gas-grid-agent Docker container.
        """
    )
