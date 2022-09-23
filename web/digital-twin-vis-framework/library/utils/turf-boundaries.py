#
# This script takes two GeoJSON files as inputs. The first should define polygon boundaries, and the 
# second any number of features. An output GeoJSON file will be produced that will only contain
# the features from the second file that are entirely bounded one (or more) of the polygons defined in the first.
#

import argparse
import geojson

from turfpy.measurement import boolean_point_in_polygon

#
# Returns true if the feature within the boundary.
#
def checkIfBounded(bounds, points):
    try:
        for point in points:
            geoPoint = geojson.Feature(geometry=geojson.Point((point[0], point[1])))
            if not boolean_point_in_polygon(geoPoint, bounds):
                return False
        return True       
    except:
        return False
  

# Entry point
parser = argparse.ArgumentParser()
parser.add_argument("-b", dest="boundFile", type=str, help="Boundaries file")
parser.add_argument("-f", dest="featureFile", type=str, help="Features file")
parser.add_argument("-o", dest="outputFile", type=str, help="Output location")
args = parser.parse_args()

print("Loading boundaries file: " + args.boundFile)
print("Loading features file: " + args.featureFile)
print("Writing to file at: " + args.outputFile)

# Load the boundary GeoJSON
with open(args.boundFile) as f:
    boundaryJSON = geojson.load(f)
    boundaries = boundaryJSON["features"]
print("Boundary file loaded.")

# Load the features GeoJSON
with open(args.featureFile) as f:
    featureJSON = geojson.load(f)
    oldFeatures = featureJSON["features"]
print("Features file loaded.")

# Iterate through boundaries
print("Iterating...")
newFeatures = []

for boundary in boundaries:
    for feature in oldFeatures:
        if feature not in newFeatures:
            if checkIfBounded(boundary, feature["geometry"]["coordinates"][0]):
                newFeatures.append(feature)

print("... iteration complete.")

# Write out adjusted file
with open(args.outputFile, "w") as f:
   geojson.dump(geojson.FeatureCollection(newFeatures), f)
print("Output file written.")
exit