"""
    This script queries the locations of all Gas Offtakes within the KG and writes the results
    to a local GeoJSON file. When deployed as part of the Gas Grid Agent, cron will run this
    script once per day, the website visualisation will then use wget to download it. This ensures
    that if any new Offtakes are added to the KG, the visualisation can plot them without
    any manual changes needed.

    Local deployment requires:
        - The gasgridagent.properties file to be set correctly.
        - Triple store endpoint to contain Gas Offtakes in expected format.

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
from gasgridagent.jpsSingletons import jpsBaseLibView

# Get settings and functions from kg_utils module
import gasgridagent.kg_utils as kg

# SPARQL Query string
QUERY = """
    PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ns1:     <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX gasgrid: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_system.owl#>
    PREFIX loc:     <http://www.bigdata.com/rdf/geospatial/literals/v1#>
    PREFIX geo:     <http://www.bigdata.com/rdf/geospatial#>
    PREFIX comp:    <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>

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
        ?term comp:isConnectedToPipeline ?pipe
    }"""

    
def outputOfftakes():
    """
        Queries the loction of all Gas Offtakes from the KG and
        outputs it to a GeoJSON file.
    """

    # Read properties file
    kg.read_properties_file(kg.PROPERTIES_FILE)

    # Set URLs to KG SPARQL endpoints (and update properties file accordingly)
    kg.setKGEndpoints(kg.PROPERTIES_FILE)

    # Get the KG endpoint
    print("Getting Gas Offtake locations from endpoint at:", kg.QUERY_ENDPOINT)
    kgClient = jpsBaseLibView.RemoteStoreClient(kg.QUERY_ENDPOINT)

    # Start of geoJSON file 
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
					"id": %s,
                    "geometry": {
                    "type": "Point",
                    "coordinates": [
                        %s
                        %s
                    ]
                    }
                },"""%(new_array[0],new_array[1],new_array[2],new_array[3],new_array[4],new_array[5],new_array[6],i,new_array[7],new_array[8])
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

    # Removing last comma as is last line
    geojson_file = geojson_file[:-1]

    # Finishing file end 
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
    geojson_written = open(outputDirectory + '/offtakes.geojson','w')
    geojson_written.write(geojson_file)
    geojson_written.close()
    print("GeoJSON file written to:", outputDirectory + '/offtakes.geojson')

            
# ===== ENTRY POINT ====
try:
    outputOfftakes()
except:
    sender = jpsBaseLibView.EmailSender()
    sender.sendEmail(
        "GasGridAgent - Exception when outputting offtake locations.",
        """
            The 'output_offtake_locations.py' script of the GasGridAgent has encountered an Exception. This script will continue to execute daily, as the issue may be temporary.
            \n\n
            It is recommended that a developer logs into the relevant VM and checks the logs for the gas-grid-agent Docker container.
        """
    )

