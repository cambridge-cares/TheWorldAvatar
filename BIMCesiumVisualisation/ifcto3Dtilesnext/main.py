# Standard library imports
import subprocess

# Third party imports
import ifcopenshell

# Reader imports
import utils 
import ifc2gltf
import ifc2kg
import ifc2tileset as ts

ifc= ifcopenshell.open(utils.INPUT_IFC) 

#Convert and split the ifc model into gltf files
lookupdict= ifc2gltf.conv2gltf(ifc, utils.INPUT_IFC)

#Convert to RDF triples and upload TTL file to local Blazegraph server
ifc2kg.ifc2ttl(utils.INPUT_TTL, utils.NAMESPACE)

# Start blazegraph server
server_cmd ="java -server -Xmx4g -jar resources\\blazegraph.jar"
server_proc = subprocess.Popen(server_cmd, stdout=subprocess.PIPE)
print("Blazegraph server has started")

#Add the filepath triples
ifc2kg.insertgltf_triple(utils.ENDPOINT, lookupdict)

# Query relevant information    
df=ifc2kg.querykg(utils.ENDPOINT)

# Adding tileset for specific asset types
tileset_assets= ts.asset2tileset(df)

#Writing out the tilesets to JSON format
ts.genbuilding_ceiling_tileset()
ts.jsonwriter(tileset_assets,"tileset_assets")

# Terminate blazegraph server
server_proc.terminate()
print("Blazegraph server has been terminated...")
print("Tilesets have been successfully generated")