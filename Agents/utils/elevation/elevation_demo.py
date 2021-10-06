"""
    Demo script to show the usage of the elevations.py script. This demo reads in a CSV with
	sample locations, determines their elevations, then writes out a local CSV with the results.

	Note: As each elevation calculation requires a call to a MapBox API (with the below key),
	be aware that is may use up your monthly API calls quota. For CMCL, this means that if we
	need to run the elevation script on many points, we may need to become premium MapBox members.

    Requires:
        - A valid MapBox API key

    Authors:
        - mdhillman<@>cmclinnovations.com
"""

import csv
import elevations
from tqdm import tqdm

def outputLine(outputFile, name, lat, lon, height):
	"""
		Output line in CSV file.
	"""
	outputFile.write(name)
	outputFile.write(",")
	outputFile.write(str(lat))
	outputFile.write(",")
	outputFile.write(str(lon))
	outputFile.write(",")
	outputFile.write(str(height))
	outputFile.write("\n")


# MapBox API Key
MAPBOX_KEY = "pk.eyJ1IjoibWhpbGxtYW4iLCJhIjoiY2t1NW5wMDF3MnBzaDJ2cWhyZ2MxbHh4diJ9.TPv5CXdhGvFPkL28tTeXTA"

# Input CSV
CSV = "./inputs.csv"

# Open file handles
mapBoxFile = open("results-mapbox.csv", "w")
mapBoxFile.write("Name,Latitude,Longitude,Height [m]\n")
openElevationFile = open("results-open-elevation.csv", "w")
openElevationFile.write("Name,Latitude,Longitude,Height [m]\n")
defraFile = open("results-defra-lidar.csv", "w")
defraFile.write("Name,Latitude,Longitude,Height [m]\n")

# Read the CSV
with open(CSV, mode="r") as csvFile:
	csvReader = csv.DictReader(csvFile)

	# Read each row
	line = 2
	for row in csvReader:
		name = row["Name"]
		lat = float(row["Latitude"])
		lon = float(row["Longitude"])

		# Determine the elevations
		#mapBoxHeight = elevations.mapBox(MAPBOX_KEY, lat, lon)
		#openElevationHeight = elevations.openElevation(lat, lon)
		defraHeight = elevations.lidarWMS(lat, lon, str(line))

		# Add rows in output files
		#outputLine(mapBoxFile, name, lat, lon, mapBoxHeight)
		#outputLine(openElevationFile, name, lat, lon, openElevationHeight)
		outputLine(defraFile, name, lat, lon, defraHeight)

		line += 1


# Close file and finish
csvFile.close()
mapBoxFile.close()
openElevationFile.close()
defraFile.close()
print("Elevation demo finished, please see 'results-*.csv' files for results.")