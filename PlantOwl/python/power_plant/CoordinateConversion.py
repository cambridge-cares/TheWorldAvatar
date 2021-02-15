##########################################
# Author: Feroz Farazi (msff2@cam.ac.uk) #
# Date: 15 Feb 2021                      #
##########################################

"""This module is defined to convert coordinates from a Coordinate Reference System (CRS)
into another. Currently, it supports the following features:
- EPSG:27700 to WGS84"""

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
