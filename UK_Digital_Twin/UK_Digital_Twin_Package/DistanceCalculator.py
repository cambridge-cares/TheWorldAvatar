#########################################################
# Author: Wanni Xie (wx243@cam.ac.uk)                   #    
#         modified from John Atherton (ja685@cam.ac.uk) #
# Last Update Date: 08 May 2021                         #
#########################################################

"""This module is designed to calculate the distance between two given GPS locations."""

from math import sin, cos, sqrt, atan2, radians

def DistanceBasedOnGPDLocation(GPSLocationArray): # GPSLocationArray = [Point1_lat, Point1_long, Point2_lat, Point2_long]
    if len(GPSLocationArray) == 4:
        pass
    else:
        print('GPS location is not sufficient.')
        return None
    # print('The input [Point1_lat, Point1_long, Point2_lat, Point2_long] are: ', GPSLocationArray)
    # approximate radius of earth in km
    R = 6371.0
        
    lat1 = radians(float(GPSLocationArray[0]))
    lat2 = radians(float(GPSLocationArray[2]))
    lon1 = radians(float(GPSLocationArray[1]))
    lon2 = radians(float(GPSLocationArray[3]))
           
    dlon = lon2 - lon1
    dlat = lat2 - lat1
    
    a = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
    c = 2 * atan2(sqrt(a), sqrt(1 - a))
    distance = R * c
    
    return round(distance, 4)