#
# This script exists to do some basic pre-processing on the DUKES data from the UK Government.
# It has been tested with the 5.11 spreadsheet (Power stations in the United Kingdom May 2023)
# sheet, but should work with data sets from other years.
#
# DUKES data is gathered by the Department for Energy Security and Net Zero (DESNZ); once a year
# they ask the major UK power providers to supply them with data on their plant locations. These
# are provided to DESNZ as OS Grid References or Postcodes. Someone at DESNZ then does some
# geospatial wizardy to convert them into Easting/Northing coordinates.
#
# Unfortunately this results in locations that aren't very accurate, sometimes plain incorrect,
# and sometimes missing. The data also often contains duplicates.
#
# This script takes in the 2023 5.11 sheet and produces an output CSV that attempts to quantify
# the quality of the location data. Read the code documentation below for an outline of the 
# steps taken in the production of this quality report.
#
# Author: Michael Hillman (mdhillman<@>cmcl.io)
#

import argparse
import pandas
import numpy
import logging

from datetime import datetime
from geopy.geocoders import Nominatim
from convertbng.util import convert_lonlat
from OSGridConverter import grid2latlong
from dukes_utils import lookup_postcode, calculate_distance, load_postcodes

# Constants
START_ROW = 6   # Row in sheet containing the header values of the table
END_ROW = 6     # Row in sheet containing the header values of the table

# Initialise logging
logger = logging.getLogger("dukes_generate_csv")
logger.setLevel(logging.INFO)
log_file_handler = logging.FileHandler("dukes_generate_csv.log", mode="w")
log_file_handler.setLevel(logging.INFO)
logger.addHandler(log_file_handler)

print("Script running, see log file for details.")
print("This process may take up to 15 minutes...")
logger.info("Start time is " + str(datetime.now()))

# Parse input arguments
parser = argparse.ArgumentParser()
parser.add_argument("--input", help="Location of input XLSX file.")
parser.add_argument("--output", help="Location of output CSV report.")
parser.add_argument("--postcodes", help="Directory containing UK postcode CSVs.")
args = parser.parse_args()

# Check for required arguments
invalid = False
if not args.input:
    print("Missing required --input parameter.")
    invalid = True
    
if not args.output:
    print("Missing required --output parameter.")
    invalid = True

if not args.postcodes:
    print("Missing required --postcodes parameter")
    invalid = True

if invalid:
    exit(-1)

# Pre-load OS Postcode data
logger.info("Loading in OS Postcode data...")
load_postcodes(args.postcodes)
logger.info("OS Postcode data has been cached.")

# Initialise the Geocoder
try:
    geolocator = Nominatim(user_agent="geoapiExercises")
except:
    logger.warn("Could not initialise geolocator, skipping this check.")

# Read the input workbook
logger.info("Reading Excel workbook at: " + args.input)
worksheet = pandas.read_excel(args.input, sheet_name="5.11 Full list", keep_default_na=False)
logger.info("Excel workbook has been read.")

# Clone and fix headings
headings = worksheet.values[START_ROW - 2].copy()
headings[2] = "Site Type"
headings[3] = "Site Technology"
headings[4] = "Combined Heat and Power"
headings[7] = "Installed Capacity (MW)"
headings = numpy.delete(headings, 13)
headings = numpy.delete(headings, 13)
headings = numpy.append(headings, "Latitude")
headings = numpy.append(headings, "Longitude")

# Write headers to a new CSV file
pandas.DataFrame([headings]).to_csv(args.output, index=False, header=False)

# Iterate through origina XLSX rows
for row in range((START_ROW - 1), len(worksheet.values)):
    if row % 100 == 0:
        print("Processing row " + str(row) + " of " + str(len(worksheet.values)))

    # Clone array for row
    output_row = worksheet.values[row].copy()

    # Trim all entries in the array
    output_row = [str(s).strip() for s in output_row]

    # Replace commas
    output_row = [str(s).replace(",", "/") for s in output_row]

    # Round capacity to 2 decimal places
    output_row[7] = round(float(output_row[7]), 2)

    # Get plant location details
    postcode = output_row[11]
    os_reference = output_row[12]
    easting = output_row[13]
    northing = output_row[14]

    # If Easting and Northing provided...
    bng_lng_lat = None
    bng_location = None
    if easting and northing:    
        # Convert it to latitude and longitude
        east = int(float(easting))
        north = int(float(northing))
        bng_lng_lat = convert_lonlat([east], [north])

        if not numpy.isnan(bng_lng_lat[0][0]) and not numpy.isnan(bng_lng_lat[1][0]):
            # Try to workout the country & region the coords are in
            bng_lng_lat_str = str(bng_lng_lat[1][0]) + ", " + str(bng_lng_lat[0][0])

            try:
                if geolocator:
                    bng_location = geolocator.reverse(bng_lng_lat_str)
                else:
                    bng_location = None
            except:
                bng_location = None

    # If an OS Grid Reference is present...
    grid_lat_lng = None
    grid_location = None
    if os_reference:
        # Covert it to latitude and longitude
        try:        
            grid_lat_lng = grid2latlong(os_reference)
            # Try to workout the country & region that postcode is in
            grid_lat_lng_str = str(grid_lat_lng[0][0]) + ", " + str(grid_lat_lng[1][0])
            grid_location = geolocator.reverse(grid_lat_lng_str)
        except:
            grid_lat_lng = None

    # If a postcode is present...
    pc_lng_lat = None
    pc_location = None
    if postcode:
        # Lookup Easting/Northing from OS Code Point CSVs
        pc_bng = lookup_postcode(postcode) 
        if pc_bng:
            # Covert it to latitude and longitude
            pc_lng_lat = convert_lonlat([pc_bng[0]], [pc_bng[1]])
            # Try to workout the country & region that postcode is in
            pc_lat_lng_str = str(pc_lng_lat[1][0]) + ", " + str(pc_lng_lat[0][0])
            pc_location = geolocator.reverse(pc_lat_lng_str)


    # Calculate the KM distance between the OS Grid Reference and the Easting/Northing
    grid_distance = None
    if grid_lat_lng and bng_lng_lat:
        grid_distance = calculate_distance(
            (grid_lat_lng.latitude, grid_lat_lng.longitude),
            (bng_lng_lat[1][0], bng_lng_lat[0][0])
        )

    # Calculate the KM distance between the postcode and the Easting/Northing
    pc_distance = None
    if pc_lng_lat and bng_lng_lat:
        pc_distance = calculate_distance(
            (pc_lng_lat[1][0], pc_lng_lat[0][0]),
            (bng_lng_lat[1][0], bng_lng_lat[0][0])
        )
    
    if bng_lng_lat and not grid_lat_lng and not pc_lng_lat:
        # If Easting/Northing was provided without a grid reference or postcode,
        # then use that as the basis of the location
        output_row = numpy.append(output_row, bng_lng_lat[1][0])
        output_row = numpy.append(output_row, bng_lng_lat[0][0])
        logger.info(output_row[1] + ": Using given Easting/Northing for location as no Grid Reference or Postcode was provided.")
    
    elif bng_lng_lat and grid_lat_lng:
        # If Easting/Northing was provided WITH a grid reference, then check the
        # distance, and location to try and determine which to use as the location

        if grid_distance and grid_distance < 15:
            # If the distance between Easting/Nothing and grid reference is small, 
            # then we'll use the lat/long from the grid reference
            output_row = numpy.append(output_row, grid_lat_lng[0][0])
            output_row = numpy.append(output_row, grid_lat_lng[1][0])
            logger.info(output_row[1] + ": Given Grid Reference is close to given Easting/Northing, using Grid Reference for location.")

        elif grid_distance and grid_distance > 15:
            # If the distance between Easting/Nothing and grid reference is large, 
            # then we'll check the locations against the given country/region.
            
            if grid_location:
                country = output_row[9].lower()
                region = output_row[10].lower()

                bng_location_match = country in bng_location.address.lower() or region in bng_location.address.lower()
                grid_location_match = country in grid_location.address.lower() or region in grid_location.address.lower()

                if grid_location_match and not bng_location_match:
                    # Grid reference location looks better, use that
                    output_row = numpy.append(output_row, grid_lat_lng[0][0])
                    output_row = numpy.append(output_row, grid_lat_lng[1][0])
                    logger.info(output_row[1] + ": Given Grid Reference is far from given Easting/Northing but matches given Region and/or Country, using Grid Reference for location.")

                elif not grid_location_match and bng_location_match:
                    # Easting/northing location looks better use that
                    output_row = numpy.append(output_row, bng_lng_lat[1][0])
                    output_row = numpy.append(output_row, bng_lng_lat[0][0])
                    logger.info(output_row[1] + ": Given Easting/Northing is far from given Grid Reference but matches given Region and/or Country, using Easting/Northing for location.")

                else:
                    # Use the grid reference
                    output_row = numpy.append(output_row, grid_lat_lng[0][0])
                    output_row = numpy.append(output_row, grid_lat_lng[1][0])
                    logger.info(output_row[1] + ": Given Grid Reference is far from given Easting/Northing an neither matches given Region and/or Country, reverting to Grid Reference for location.")
        else:
            # As the primary source from the power providers, use the grid
            # reference location over the given easting/northing
            output_row = numpy.append(output_row, grid_lat_lng[0][0])
            output_row = numpy.append(output_row, grid_lat_lng[1][0])
            logger.info(output_row[1] + ": Could not determine distance between given Easting/Northing and Grid Reference, reverting to Grid Reference for location.")

    elif bng_lng_lat and pc_lng_lat:
        # If Easting/Northing was provided WITH a postcode, then check the
        # distance, and location to try and determine which to use as the location

        if pc_distance and pc_distance < 15:
            # If the distance between Easting/Nothing and postcode is small, 
            # then we'll use the lat/long from the Easting/Nothing
            output_row = numpy.append(output_row, pc_lng_lat[1][0])
            output_row = numpy.append(output_row, pc_lng_lat[0][0])
            logger.info(output_row[1] + ": Given Postcode is close to given Easting/Northing, using Postcode for location.")

        elif pc_distance and pc_distance > 15:
            # If the distance between Easting/Nothing and postcode is large, 
            # then we'll check the locations against the given country/region.
            
            if pc_location:
                country = output_row[9].lower()
                region = output_row[10].lower()

                bng_location_match = country in bng_location.address.lower() or region in bng_location.address.lower()
                pc_location_match = country in pc_location.address.lower() or region in pc_location.address.lower()

                if pc_location_match and not bng_location_match:
                    # Postcode location looks better, use that
                    output_row = numpy.append(output_row, pc_lng_lat[1][0])
                    output_row = numpy.append(output_row, pc_lng_lat[0][0])
                    logger.info(output_row[1] + ": Given Postcode is far from given Easting/Northing but matches given Region and/or Country, using Postcode for location.")

                elif not pc_location_match and bng_location_match:
                    # Easting/northing location looks better use that
                    output_row = numpy.append(output_row, bng_lng_lat[1][0])
                    output_row = numpy.append(output_row, bng_lng_lat[0][0])
                    logger.info(output_row[1] + ": Given Easting/Northing is far from given Postcode but matches given Region and/or Country, using Easting/Northing for location.")

                else:
                    # Use the postcode
                    output_row = numpy.append(output_row, pc_lng_lat[1][0])
                    output_row = numpy.append(output_row, pc_lng_lat[0][0])
                    logger.info(output_row[1] + ": Given Postcode is far from given Easting/Northing an neither matches given Region and/or Country, reverting to Postcode for location.")
        else:
            # As the primary source from the power providers, use the
            # postcode location over the given easting/northing
            output_row = numpy.append(output_row, pc_lng_lat[1][0])
            output_row = numpy.append(output_row, pc_lng_lat[0][0])
            logger.info(output_row[1] + ": Could not determine distance between given Easting/Northing and Postcode, reverting to Postcode for location.")


    # Remove original easting/northing, we don't want to output them
    output_row = numpy.delete(output_row, [13, 14])

    # Append row to CSV (if it's resulted in a determined location)
    if len(output_row) == len(headings):
        pandas.DataFrame([output_row]).to_csv(args.output, mode="a", index=False, header=False)

logger.info("Script finished.")
logger.info("End time is " + str(datetime.now()))
print("Script finished, please check generated report CSV.")