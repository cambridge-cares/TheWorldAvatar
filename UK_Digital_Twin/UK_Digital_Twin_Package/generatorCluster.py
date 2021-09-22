###################################################
# Author: Wanni Xie (wx243@cam.ac.uk)             #
# Modified from: John Atherton (ja685@cam.ac.uk)  #
# Last Update Date: 22 Sept 2021                  #
###################################################

from UK_Digital_Twin_Package.DistanceCalculator import DistanceBasedOnGPSLocation as GPS_distance

"""Find the closest bus of a given GPS location (of a power plant or generator)"""
def closestBus(busGPS, lat1, lon1):
  distances = [65534]*len(busGPS) # the large number is the earth's circumference
  j = 0
  for i in busGPS:
    GPSLocationPair = [float(lat1), float(lon1), float(i[1]), float(i[2])]    
    distances[j] = GPS_distance(GPSLocationPair)
    j += 1
    bus_index = distances.index(min(distances))   
  return busGPS[bus_index][0] # return the bus node URL