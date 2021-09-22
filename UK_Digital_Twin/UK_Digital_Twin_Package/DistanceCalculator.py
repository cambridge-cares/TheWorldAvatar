#########################################################
# Author: Wanni Xie (wx243@cam.ac.uk)                   #    
#         modified from John Atherton (ja685@cam.ac.uk) #
# Last Update Date: 30 August 2021                      #
#########################################################

"""This module is designed to calculate the distance between two given GPS locations."""

from math import sin, cos, sqrt, atan2, radians

def DistanceBasedOnGPSLocation(GPSLocationArray): # GPSLocationArray = [Point1_lat, Point1_long, Point2_lat, Point2_long]
    if len(GPSLocationArray) == 4:
        pass
    else:
        print('GPS location is not sufficient.')
        return None
    # print('The input [Point1_lat, Point1_long, Point2_lat, Point2_long] are: ', GPSLocationArray)
    # approximate radius of earth in km
    R = 6371.0
        
    lon1 = radians(float(GPSLocationArray[0]))
    lon2 = radians(float(GPSLocationArray[2]))
    lat1 = radians(float(GPSLocationArray[1]))
    lat2 = radians(float(GPSLocationArray[3]))
           
    dlon = lon2 - lon1
    dlat = lat2 - lat1
    
    a = sin(dlat / 2)**2 + cos(lat1) * cos(lat2) * sin(dlon / 2)**2
    c = 2 * atan2(sqrt(a), sqrt(1 - a))
    distance = R * c #unit: km
    
    return round(distance, 4)

# d = DistanceBasedOnGPDLocation(['-0.1373639', '50.8223711', '-2.5879675', '51.4545085'])
# print(d)