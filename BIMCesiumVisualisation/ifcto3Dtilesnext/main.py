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

try: 
    #Add the filepath triples
    ifc2kg.insertkg(utils.ENDPOINT, lookupdict)

    # Query relevant information    
    df=ifc2kg.ifcquery(utils.ENDPOINT)

    # Query for time series, comment the code if you know there is no related time series
    #df=ifc2kg.timeseriesquery(utils.ENDPOINT,df)
    
    # Adding tileset for specific asset types
    tileset_assets= ts.asset2tileset(df)

    #Writing out the tilesets to JSON format
    ts.genbuilding_ceiling_tileset()
    ts.jsonwriter(tileset_assets,"tileset_assets")

except Exception as e:
    print(e)

finally:
    # Terminate blazegraph server
    server_proc.terminate()
    print("Blazegraph server has been terminated...")
    print("Tilesets have been successfully generated")