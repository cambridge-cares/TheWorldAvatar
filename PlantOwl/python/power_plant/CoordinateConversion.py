##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 15 Feb 2021                      #
##########################################

"""This module is defined to convert coordinates from a Coordinate Reference System (CRS)
into another. Currently, it supports the following features:
- EPSG:27700 to WGS84"""
from math import sin, cos

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


def initial_lat(northing, aF0, RadPHI0, n, bF0):
    PHI1 = ((northing - N0) / aF0) + DecimalPHI0
    M = marc(bF0, n, PHI1)
    PHI2 = ((northing - N0 - M) / aF0) + PHI1
    """Calculate the final value of initial latitude"""
    while abs(northing - N0 - M) > 0.00001:
        PHI2 = ((northing - N0 - M) / aF0) + PHI1
        M = marc(bF0, n, RadPHI0, PHI2)
        PHI1 = PHI2
    return PHI2

def marc(bF0, n, RadPHI0, PHI1):
    return bF0 * (((1 + n + ((5 / 4) * (n ^ 2)) + ((5 / 4) * (n ^ 3))) * (PHI1 - RadPHI0))
                  - (((3 * n) + (3 * (n ^ 2)) + ((21 / 8) * (n ^ 3))) * (sin(PHI1 - RadPHI0)) * (cos(PHI1 + RadPHI0)))
                  + ((((15 / 8) * (n ^ 2)) + ((15 / 8) * (n ^ 3))) * (sin(2 * (PHI1 - RadPHI0))) * (cos(2 * (PHI1 + RadPHI0))))
                  - (((35 / 24) * (n ^ 3)) * (sin(3 * (PHI1 - RadPHI0))) * (cos(3 * (PHI1 + RadPHI0)))))