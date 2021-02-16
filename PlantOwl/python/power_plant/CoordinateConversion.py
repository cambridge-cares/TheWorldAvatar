##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 15 Feb 2021                      #
##########################################

"""This module is defined to convert coordinates from a Coordinate Reference System (CRS)
into another. Currently, it supports the following features:
- EPSG:27700 to WGS84"""
from math import sin, cos, sqrt, tan

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
    e2 = ((aF0^2) - (bF0^2)) / (aF0^2)
    n = (aF0-bF0) / (aF0 + bF0)
    Et = easting - E0
    """Compute the value of PHI in radian"""
    PHId = initial_lat(northing, aF0, RadPHI0, n, bF0)
    """"Calculate some intermediate variables using PHId"""
    nu = aF0 / (sqrt(1 - (e2 * ((sin(PHId)) ^ 2))))
    rho = (nu * (1 - e2)) / (1 - (e2 * (sin(PHId)) ^ 2))
    eta2 = (nu / rho) - 1
    """Calculate latitiude"""
    VII = (tan(PHId)) / (2 * rho * nu)
    VIII = ((tan(PHId)) / (24 * rho * (nu ^ 3))) * (5 + (3 * ((tan(PHId)) ^ 2)) + eta2 - (9 * eta2 * ((tan(PHId)) ^ 2)))
    IX = ((tan(PHId)) / (720 * rho * (nu ^ 5))) * (61 + (90 * ((tan(PHId)) ^ 2)) + (45 * ((tan(PHId)) ^ 4)))

    return (180 / Pi) * (PHId - ((Et ^ 2) * VII) + ((Et ^ 4) * VIII) - ((Et ^ 6) * IX))

"""Converts from the easting and northing coordinates to longitude"""
def e_n_to_long(easting, northing):
    """Convert an angle measure from decimal to radian"""
    RadPHI0 = DecimalPHI0 * (Pi / 180)
    RadLAM0 = DecimalLAM0 * (Pi / 180)
    """Evaluates some intermediate variables"""
    aF0 = a * F0
    bF0 = b * F0
    e2 = ((aF0 ^ 2) - (bF0 ^ 2)) / (aF0 ^ 2)
    n = (aF0 - bF0) / (aF0 + bF0)
    Et = easting - E0
    """Compute the value of PHI in radian"""
    PHId = initial_lat(northing, aF0, RadPHI0, n, bF0)
    """Evaluate some intermediate variables that rely on PHId"""
    nu = aF0 / (sqrt(1 - (e2 * ((sin(PHId)) ^ 2))))
    rho = (nu * (1 - e2)) / (1 - (e2 * (sin(PHId)) ^ 2))
    eta2 = (nu / rho) - 1
    """Calculate longitude"""
    X = ((cos(PHId)) ^ -1) / nu
    XI = (((cos(PHId)) ^ -1) / (6 * (nu ^ 3))) * ((nu / rho) + (2 * ((tan(PHId)) ^ 2)))
    XII = (((cos(PHId)) ^ -1) / (120 * (nu ^ 5))) * (5 + (28 * ((tan(PHId)) ^ 2)) + (24 * ((tan(PHId)) ^ 4)))
    XIIA = (((cos(PHId)) ^ -1) / (5040 * (nu ^ 7))) * (61 + (662 * ((tan(PHId)) ^ 2)) + (1320 * ((tan(PHId)) ^ 4)) + (720 * ((tan(PHId)) ^ 6)))

    return (180 / Pi) * (RadLAM0 + (Et * X) - ((Et ^ 3) * XI) + ((Et ^ 5) * XII) - ((Et ^ 7) * XIIA))

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