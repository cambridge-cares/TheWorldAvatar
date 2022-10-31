#
# This script aims to update a GeoJSON file so that it better fits the format that Mapbox
# expects. In particular, if an ID field is listed within the feature's properties, this
# script will move it to the root of the feature and ensure it has a numerical value.
#

import argparse
import geojson

#
# Check and fix the ID field
#
def checkID(feature, index):
    if "id" in feature:
        # Top level id parameter present
        id = feature["id"]

        if str(id).isdigit():
            feature["id"] = int(id)
        else:
            feature["id"] = index
        return

    elif "id" in feature["properties"]:
        # id present but within properties
        id = feature["properties"]["id"]
        del feature["properties"]["id"]

        if str(id).isdigit():
            feature["id"] = int(id)
        else:
            feature["id"] = index
    
    elif "@id" in feature["properties"]:
        # @id present but within properties
        id = feature["properties"]["@id"]
        del feature["properties"]["@id"]

        if str(id).isdigit():
            feature["id"] = int(id)
        else:
            feature["id"] = index

    elif "ID" in feature["properties"]:
        # ID present but within properties
        id = feature["properties"]["ID"]
        del feature["properties"]["ID"]

        if str(id).isdigit():
            feature["id"] = int(id)
        else:
            feature["id"] = index
    else:
        feature["id"] = index

#
# Check and fix the name parameter
#
def checkName(feature):
    if "Name" in feature["properties"]:
        name = feature["properties"]["Name"]
        del feature["properties"]["Name"]
        feature["properties"]["name"] = name

#
# Remove any properties that are listed but have null values
#
def checkNull(feature):
    toRemove = []
    for key in feature["properties"].keys():
        if feature["properties"][key] is None:
            toRemove.append(key)

    for key in toRemove:
        del feature["properties"][key]

# Entry point
parser = argparse.ArgumentParser()
parser.add_argument("-i", dest="inputFile", type=str, help="Input file")
parser.add_argument("-o", dest="outputFile", type=str, help="Output location")
args = parser.parse_args()

print("Loading input file: " + args.inputFile)
print("Writing to file at: " + args.outputFile)

# Load the input GeoJSON
with open(args.inputFile) as f:
    inputJSON = geojson.load(f)
print("Input file loaded.")

# Iterate through features
print("Iterating through features...")
featureCount = len(inputJSON["features"])

for i in range(0, featureCount):
    feature = inputJSON["features"][i]

    checkID(feature, (i + 1))
    checkName(feature)
    checkNull(feature)

print("... iteration complete.")

# Write out adjusted file
with open(args.outputFile, "w") as f:
    geojson.dump(inputJSON, f)
print("Output file written.")
