# 
# Utility functions for DUKES data clean up.
#
# Author: Michael Hillman (mdhillman<@>cmcl.io)
#

import pandas
import numpy

from geopy import distance
from os import listdir
from os.path import isfile, join

# Dictionary of postcode regions to CSV content
POSTCODES = {}

"""
    Preloads postcode region data from CSVs.

    @param directory Directory containing region CSVs
"""
def load_postcodes(directory):

    files = [f for f in listdir(directory) if isfile(join(directory, f))]

    for file in files:
        name = file.split(".")[0]
        data = pandas.read_csv(join(directory, file)).values
        POSTCODES[name] = data

"""
    Uses the OS Code-Point CSVs to lookup the Easting/Northing
    for the input postcode string.

    @param postcode Postcode string
    
    @returns [easting, northing] or None
"""
def lookup_postcode(postcode):
    postcode = postcode.lower()
    region = postcode[:2]

    if region in POSTCODES:
        csv = POSTCODES[region]

        for row in range(0, len(csv)):
            value = csv[row][0].lower().replace(" ", "")
            
            if postcode == value:
                location = [csv[row][2], csv[row][3]]
                return location
        
    return None

"""
    Returns the distance between two WGS:84 Latitude/Longitude
    positions (in KM).

    @param location_one (latitude, longitude)
    @param location_two (latitude, longitude)

    @returns distance in KM
"""
def calculate_distance(location_one, location_two):
    if numpy.isnan(location_one[0]) or numpy.isnan(location_one[1]):
        return None
    
    if numpy.isnan(location_two[0]) or numpy.isnan(location_two[1]):
        return None
    
    return distance.distance(location_one, location_two).km