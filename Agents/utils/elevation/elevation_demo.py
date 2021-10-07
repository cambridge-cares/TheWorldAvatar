"""
    Demo script to show the usage of the elevations.py script. This demo reads in a CSV with
	sample locations, determines their elevations, then writes out a local CSV with the results.

    Requires:
        - A remote GeoServer with a valid WMS endpoint

    Authors:
        - mdhillman<@>cmclinnovations.com
"""

import csv
import elevations

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


# WMS Endpoint
WMS = "url-goes-here"

# Input CSV
CSV = "./inputs.csv"

# Open file handles
resultsFile = open("results.csv", "w")
resultsFile.write("Name,Latitude,Longitude,Height [m]\n")

# Read the CSV
with open(CSV, mode="r") as csvFile:
	csvReader = csv.DictReader(csvFile)

	# Read each row
	for row in csvReader:
		name = row["Name"]
		lat = float(row["Latitude"])
		lon = float(row["Longitude"])

		# Determine the elevations
		height = elevations.getHeight(WMS, lat, lon)

		# Add rows in output files
		outputLine(height, name, lat, lon, resultsFile)


# Close file and finish
csvFile.close()
resultsFile.close()
print("Elevation demo finished, please see 'results.csv' file for heights.")