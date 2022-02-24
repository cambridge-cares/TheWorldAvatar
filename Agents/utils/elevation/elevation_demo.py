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

def outputLine(outputFile, name, lat, lon, refHeight, height, absDev, relDev):
	"""
		Output line in CSV file.
	"""
	outputFile.write(name)
	outputFile.write(",")
	outputFile.write(str(lat))
	outputFile.write(",")
	outputFile.write(str(lon))
	outputFile.write(",")
	outputFile.write(str(refHeight))
	outputFile.write(",")
	outputFile.write(str(height))
	outputFile.write(",")
	outputFile.write(str(absDev))
	outputFile.write(",")
	outputFile.write(str(relDev))
	outputFile.write("\n")


# WMS Endpoint
WMS = "url-goes-here"

# Input CSV
CSV = "./inputs.csv"

# Open file handles
resultsFile = open("results.csv", "w")
resultsFile.write("Name,Latitude,Longitude,Ref height [m],Height [m],Absolute deviation [m],Relative deviation [%]\n")

# Read the CSV
with open(CSV, mode="r") as csvFile:
	csvReader = csv.DictReader(csvFile)

	# Read each row
	for row in csvReader:
		name = row["Name"]
		refHeight = float(row["Height [m]"])
		lat = float(row["Latitude"])
		lon = float(row["Longitude"])

		# Determine the elevations
		height = elevations.getHeight(WMS, lat, lon)

		# Calculate deviations
		absDev = abs(refHeight - height)
		relDev = 100.0 * abs(refHeight - height) / refHeight

		# Add rows in output files
		outputLine(resultsFile, name, lat, lon, refHeight, height, absDev, relDev)


# Close file and finish
csvFile.close()
resultsFile.close()
print("Elevation demo finished, please see 'results.csv' file for heights.")