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


# MapBox API Key
MAPBOX_KEY = "YOUR-API-KEY-HERE"

# Input CSV
CSV = "./recorded-elevations.csv"

# Open a file handle
outputFile = open("determined-elevations.csv", "w")
outputFile.write("Name,Latitude,Longitude,Height [m]\n")

# Read the CSV
with open(CSV, mode="r") as csvFile:
	csvReader = csv.DictReader(csvFile)

	# Read each row
	for row in csvReader:
		name = row["Name"]
		lat = row["Latitude"]
		lon = row["Longitude"]

		# Determine the elevation
		# Get the elevation
		height = elevations.getElevation(MAPBOX_KEY, float(lat), float(lon))

		# Add row in output file
		outputFile.write(name)
		outputFile.write(",")
		outputFile.write(lat)
		outputFile.write(",")
		outputFile.write(lon)
		outputFile.write(",")
		outputFile.write(str(height))
		outputFile.write("\n")
		
# Close file and finish
csvFile.close()
outputFile.close()
print("Elevation demo finished, please see 'elevations.csv' for results.")
