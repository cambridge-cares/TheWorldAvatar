"""
utilities.py
Author: Janelle Berscheid, June 2018

A collection of utility methods for handling GPS data, including conversions
"""

import numpy as np
from pyproj import Transformer, CRS


# earth radius from semimajor axis of WGS 84 spheroid
EARTH_RADIUS = 6378137.0 * 2 * np.pi


def wgs_to_utm_code(lat, lon):
    # convert_wgs_to_utm function
    # see https://stackoverflow.com/questions/40132542/get-a-cartesian-projection-accurate-around-a-lat-lng-pair
    utm_band = str(int((np.floor((lon + 180) / 6) % 60) + 1))
    if len(utm_band) == 1:
        utm_band = '0' + utm_band
    if lat >= 0:
        epsg_code = '326' + utm_band
    else:
        epsg_code = '327' + utm_band
    return int(epsg_code)


def get_euclidean_distance(p1, p2):
    """
    It's Euclidean distance. Use as a distance metric for UTM coords (not WGS).
    :param p1: the first x-y point, as a tuple of floats
    :param p2: the second x-y point, as a tuple of floats
    :return: a single floating-point distance value
    """
    return np.sqrt(np.power((p2[0] - p1[0]), 2) + np.power((p2[1] - p1[1]), 2))


def get_projection(lon, lat, code):
    """
    Given WGS readings, project them into UTM for a given city/code.
    :param lon: a 1D numpy float array containing the longitude column of a dataset. Must be the same length as lat.
    :param lat: a 1D numpy float array containing the latitude column of a dataset. Must be the same length as lon.
    :param city: a string containing the name of the city the WGS readings came from.
                Used to determine the correct UTM projection.
    :return: the 1D numpy arrays corresponding to the projected UTM easting and northing coordinates
    """
    transformer = Transformer.from_crs(CRS("EPSG:4326"), CRS(code))

    def transf(x, y):
        return transformer.transform(x, y)

    utm_e, utm_n = np.vectorize(transf)(lat, lon)
    return utm_e, utm_n
