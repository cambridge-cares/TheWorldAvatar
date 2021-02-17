##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 15 Feb 2021                      #
##########################################

"""This module is defined to convert coordinates from a Coordinate Reference System (CRS)
into another. Currently, it supports the following features:
- EPSG:27700 to WGS84"""
from math import sin, cos, sqrt, tan

from GMLParser import convert_epsg27700_to_wgs84

"""Coordinate conversion constants are defined here to convert from EPSG:27700 to WGS84"""
"""Semi-major axis, a"""
a = 6377563.396
"""Semi-minor axis, b"""
b = 6356256.909
"""Central Meridan Scale, F0"""
F0 = 0.999601271700
"""True origin Easting, E0"""
E0 = 400000.000
"""True origin Northing, N0"""
N0 = -100000.000
"""True origin latitude, Decimal PHI 0"""
DecimalPHI0 = 49.00000000
"""True origin longitude, Decimal LAMBDA 0"""
DecimalLAM0 = -2.00000000
"""Mathematical Constant, Pi"""
Pi = 3.14159265358979

"""Converts from the Easting and Northing coordinates to latitude"""
def e_n_to_lat(easting, northing):
    """Convert an angle measure from decimal to radian"""
    RadPHI0 = DecimalPHI0 * (Pi / 180)

    """Compute values of some intermediate variables"""
    aF0 = a * F0
    bF0 = b * F0
    e2 = ((aF0 * aF0) - (bF0 * bF0)) / (aF0 * aF0)
    n = (aF0-bF0) / (aF0 + bF0)
    Et = easting - E0
    """Compute the value of PHI in radian"""
    PHId = initial_lat(northing, aF0, RadPHI0, n, bF0)
    """"Calculate some intermediate variables using PHId"""
    nu = aF0 / (sqrt(1 - (e2 * pow(sin(PHId), 2))))
    rho = (nu * (1 - e2)) / (1 - (e2 * pow(sin(PHId), 2)))
    eta2 = (nu / rho) - 1
    """Calculate latitiude"""
    VII = (tan(PHId)) / (2 * rho * nu)
    VIII = ((tan(PHId)) / (24 * rho * pow(nu, 3))) * (5 + (3 * pow((tan(PHId)), 2)) + eta2 - (9 * eta2 * pow((tan(PHId)), 2)))
    IX = ((tan(PHId)) / (720 * rho * pow(nu, 5))) * (61 + (90 * pow((tan(PHId)), 2)) + (45 * pow((tan(PHId)), 4)))

    return (180 / Pi) * (PHId - (pow(Et, 2) * VII) + (pow(Et, 4) * VIII) - (pow(Et, 6) * IX))

"""Converts from the easting and northing coordinates to longitude"""
def e_n_to_long(easting, northing):
    """Convert an angle measure from decimal to radian"""
    RadPHI0 = DecimalPHI0 * (Pi / 180)
    RadLAM0 = DecimalLAM0 * (Pi / 180)
    """Evaluates some intermediate variables"""
    aF0 = a * F0
    bF0 = b * F0
    e2 = (pow(aF0, 2) - pow(bF0, 2)) / pow(aF0, 2)
    n = (aF0 - bF0) / (aF0 + bF0)
    Et = easting - E0
    """Compute the value of PHI in radian"""
    PHId = initial_lat(northing, aF0, RadPHI0, n, bF0)
    """Evaluate some intermediate variables that rely on PHId"""
    nu = aF0 / (sqrt(1 - (e2 * pow((sin(PHId)), 2))))
    rho = (nu * (1 - e2)) / (1 - pow(e2 * (sin(PHId)), 2))
    eta2 = (nu / rho) - 1
    """Calculate longitude"""
    X = pow((cos(PHId)), -1) / nu
    XI = (pow((cos(PHId)), -1) / (6 * pow(nu, 3))) * ((nu / rho) + (2 * pow((tan(PHId)), 2)))
    XII = (pow((cos(PHId)), -1) / (120 * pow(nu, 5))) * (5 + (28 * pow((tan(PHId)), 2)) + (24 * pow((tan(PHId)), 4)))
    XIIA = (pow((cos(PHId)), -1) / (5040 * pow(nu, 7))) * (61 + (662 * pow((tan(PHId)), 2)) + (1320 * pow((tan(PHId)), 4)) + (720 * pow((tan(PHId)), 6)))

    return (180 / Pi) * (RadLAM0 + (Et * X) - (pow(Et, 3) * XI) + (pow(Et, 5) * XII) - (pow(Et, 7) * XIIA))

"""Calculates the initial value for latitude in radian"""
def initial_lat(northing, aF0, RadPHI0, n, bF0):
    PHI1 = ((northing - N0) / aF0) + DecimalPHI0
    M = marc(bF0, n, RadPHI0, PHI1)
    PHI2 = ((northing - N0 - M) / aF0) + PHI1
    """Calculate the final value of initial latitude"""
    while abs(northing - N0 - M) > 0.00001:
        PHI2 = ((northing - N0 - M) / aF0) + PHI1
        M = marc(bF0, n, RadPHI0, PHI2)
        PHI1 = PHI2
    return PHI2

"""Calculate the meridional arc"""
def marc(bF0, n, RadPHI0, PHI1):
    return bF0 * (((1 + n + ((5 / 4) * pow(n, 2)) + ((5 / 4) * pow(n, 3))) * (PHI1 - RadPHI0))
                  - (((3 * n) + (3 * pow(n, 2)) + ((21 / 8) * pow(n, 3))) * (sin(PHI1 - RadPHI0)) * (cos(PHI1 + RadPHI0)))
                  + ((((15 / 8) * pow(n, 2)) + ((15 / 8) * pow(n, 3))) * (sin(2 * (PHI1 - RadPHI0))) * (cos(2 * (PHI1 + RadPHI0))))
                  - (((35 / 24) * pow(n, 3)) * (sin(3 * (PHI1 - RadPHI0))) * (cos(3 * (PHI1 + RadPHI0)))))

"""A single point converter that converts from easting and northing coordinates to
latitude and longitude coordinates"""
def e_n_to_lat_long(coordinates, delimeter):
    return str(e_n_to_lat(float(coordinates.split(delimeter)[0]), float(coordinates.split(delimeter)[1])))\
           +delimeter+str(e_n_to_long(float(coordinates.split(delimeter)[0]), float(coordinates.split(delimeter)[1])))

"""A multiple point converter that converts from easting and northing coordinates to
latitude and longitude coordinates"""
def e_n_to_lat_long_multiple(coordinates, delimeter, n_of_points):
    point_list_lat_long = ""
    point_list_e_n = coordinates.split(delimeter)
    for i in range(0, n_of_points*2, 2):
        if i == 0:
            point_list_lat_long = point_list_lat_long + str(e_n_to_lat(float(point_list_e_n[i]), float(point_list_e_n[i+1])))\
                              +delimeter+str(e_n_to_long(float(point_list_e_n[i]), float(point_list_e_n[i+1])))
        else:
            point_list_lat_long = point_list_lat_long + delimeter +str(e_n_to_lat(float(point_list_e_n[i]), float(point_list_e_n[i+1])))\
                              +delimeter+str(e_n_to_long(float(point_list_e_n[i]), float(point_list_e_n[i+1])))
    return point_list_lat_long
if __name__ == '__main__':
    print(e_n_to_lat(651409.903, 313177.270))
    print(e_n_to_long(651409.903, 313177.270))
    print(e_n_to_lat(495783, 143522))
    print(e_n_to_long(495783, 143522))
    print(e_n_to_lat(539686, 309714)) #52.66676759829613
    print(e_n_to_long(539686, 309714)) #0.06588493447562084
    print(convert_epsg27700_to_wgs84("539686#309714", "#"))
    print(convert_epsg27700_to_wgs84("374477#267522", "#"))
    print(e_n_to_lat(374477, 267522))
    print(e_n_to_long(374477, 267522))
    print(e_n_to_lat (539706.3125999998,309679.06259999983))
    print(e_n_to_long(539706.3125999998,309679.06259999983))
    print(e_n_to_lat(539666.3125999998, 309679.06259999983))
    print(e_n_to_long(539666.3125999998, 309679.06259999983))
    print(e_n_to_lat(539646.3125999998, 309713.68759999983))
    print(e_n_to_long(539646.3125999998, 309713.68759999983))
    print(e_n_to_lat(539666.3125999998, 309748.34380000085))
    print(e_n_to_long(539666.3125999998, 309748.34380000085))
    print(e_n_to_lat(539706.3125999998, 309748.34380000085))
    print(e_n_to_long(539706.3125999998, 309748.34380000085))
    print(e_n_to_lat(539726.3125999998, 309713.68759999983))
    print(e_n_to_long(539726.3125999998, 309713.68759999983))
    print(e_n_to_lat(539706.3125999998, 309679.06259999983))
    print(e_n_to_long(539706.3125999998, 309679.06259999983))
    print(e_n_to_lat_long_multiple(
        "549266.3125999998#303443.68759999983#549246.3125999998"
        "#303409.03119999915#549206.3125999998#303409.03119999915"
        "#549186.3125999998#303443.68759999983#549206.3125999998"
        "#303478.31259999983#549246.3125999998#303478.31259999983"
        "#549266.3125999998#303443.68759999983",
        "#", 7))
