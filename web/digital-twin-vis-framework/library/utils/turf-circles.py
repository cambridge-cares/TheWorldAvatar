#
# 
#

import argparse
import geojson

from turfpy.transformation import circle

#
# Generates a circle from the input Point feature
#
def generateCircle(feature, radius):
    return circle(feature, radius, 16)
  

# Entry point
parser = argparse.ArgumentParser()
parser.add_argument("-f", dest="featureFile", type=str, help="Features file")
parser.add_argument("-o", dest="outputFile", type=str, help="Output location")
parser.add_argument("-r", dest="radius", type=str, help="Circle radius")
args = parser.parse_args()

print("Loading features file: " + args.featureFile)
print("Writing to file at: " + args.outputFile)
print("Using radius: " + args.radius)

# Load the features GeoJSON
with open(args.featureFile) as f:
    featureJSON = geojson.load(f)
    oldFeatures = featureJSON["features"]
print("Features file loaded.")

# Iterate through features
print("Iterating through features...")
newFeatures = []

for feature in oldFeatures:
    if feature["geometry"]["type"] == "Point":
        newFeatures.append(generateCircle(feature, float(args.radius)))

print("... iteration complete.")

# Write out adjusted file
with open(args.outputFile, "w") as f:
   geojson.dump(geojson.FeatureCollection(newFeatures), f)
print("Output file written.")
exit