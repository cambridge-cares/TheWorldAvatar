"""
    This script queries the KG for the last day of flow data for
    every terminal and offtake, this data is then written to a
    local JSON file.
    
    It comprises part of the Gas Grid Agent tool and should be 
    executed daily via crontab.
    
    Author: support<@>cmclinnovations.com
"""

import os 
import sys
import json
import shutil
from datetime import datetime as dt
from datetime import timedelta as td
from py4jps.resources import JpsBaseLib

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


def buildQuery(hoursAgo):
    """
        Builds the SPARQL query that will be submitted to the KG.
        
        Parameters:
            hoursAgo: limit for oldest result
            
        Returns:
            Built SPARQL query (without KG URL)
    """
    
    queryString ="""PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX gas: <http://www.theworldavatar.com/ontology/ontogasgrid/gas_network_components.owl#>
    PREFIX om:     <http://www.ontology-of-units-of-measure.org/resource/om-2/>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

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
    """
    
    # Get the date 24 hours ago
    dayAgo = dt.now() - td(hours=hoursAgo)
    
    # Format to string in "2021-06-04T02:58:00.000Z" format
    timestampString = dayAgo.strftime("%Y-%m-%dT%H:%M:%S")
    timestampString += ".000Z"
    
    # Append SPARQL filter
    filterString = "FILTER (?UTC > \"" + timestampString + "\"^^xsd:dateTime)"
    queryString += "\t" + filterString + "\n}"    
    return queryString


def submitQuery(namespace, callbackSuccess, callbackFailure):
    """
        Submits the input query to the KG asynchronously (so a timeout can be added).
        Once the request is complete, one of the input callback functions
        will be called.
        
        Arguments:
            namespace         - KG namespace
            callbackSuccess - function to call on success
            callbackFailure - function to call on failure
    """
    
    # Get the KG URL
    kgLocation = getKGLocation(namespace)
    print("INFO: Determined KG endpoint as '" + kgLocation + "'.")
    
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
    today = dt.now()
    print("INFO: Submitting request at", today)
    
    try:
        # Get results from 24 hours ago
        result = KGClient.executeQuery(buildQuery(24))
                    
        if result != None and len(result.toList()) == 0:
            # Try 48 if nothing returns
            print("WARNING: No results in last 24 hours, trying 48...")
            result = KGClient.executeQuery(buildQuery(48))
            
            if result != None and len(result.toList()) == 0:
                # Last resort, try 1 week
                print("WARNING: No results in last 48 hours, trying 1 week...")
                result = KGClient.executeQuery(buildQuery(168))
            
    except Exception as error:
        callbackFailure(error)
        
    callbackSuccess(result.toList())


def onSuccess(data):
    """
        Runs when data is successfully returned by the query.
        
        Arguments:
            data - resulting data from KG
    """
    print("INFO: SPARQL query successful, data received.")
    print("INFO: Number of results is", len(data))
        
    # Convert to JSON
    dataString = str(data).replace("'", "\"")        
    jsonData = json.loads(dataString)
    
    # Write to a dated file
    now = dt.now()
    filename = "flow-data-" + now.strftime("%Y-%m-%d") + ".json"
    filepath = os.path.join(OUTPUT_DIR, filename)
    
    try:
        print("INFO: Writing data to file at", filepath)
        with open(filepath, "w") as file:
            json.dump(jsonData, file)
            file.close()
            
        # Also make a copy to flow-data-latest.json (so we don't have to
        # write a dynamic wget call to download it later).
        latestCopy = os.path.join(OUTPUT_DIR, "flow-data-latest.json")
        shutil.copy(filepath, latestCopy)
        print("INFO: Data also written to file at", latestCopy)
        
    except Exception:
        print("WARNING: Could not write data to file, will try local output...")
        
        try:
            with open(filename, "w") as file:
                json.dump(jsonData, file)
                file.close()
            print("INFO: Data written to file at ./" + filename)
            
        except Exception:
            print("ERROR: Could not write out data file!")
        
    print("SUCCESS: Script completed.")
        

def onFailure(error):
    """
        Runs when query results in a failure/exception.
        
        Arguments:
            error - thrown errot
    """
    print("ERROR: Could not complete SPARQL query, error is as follows...")
    print("\n" + str(error) + "\n")
    sys.exit()


def main():
    """
        Main function.
    """
    print("\n")
    submitQuery("ontogasgrid", onSuccess, onFailure)


# Entry point, calls main function
if __name__ == "__main__":
    main()