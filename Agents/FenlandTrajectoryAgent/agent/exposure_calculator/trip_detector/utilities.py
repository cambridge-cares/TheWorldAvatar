"""
utilities.py
Author: Janelle Berscheid, June 2018

A collection of utility methods for handling GPS data, including conversions
"""

import numpy as np
import requests
import xml.etree.ElementTree as ET
from pyproj import Transformer, CRS


EARTH_RADIUS = 6378137.0 * 2 * np.pi  # earth radius from semimajor axis of WGS 84 spheroid

PROJ_CODES = {
    "victoria": "epsg:32610",
    "vancouver": "epsg:32610",
    "saskatoon": "epsg:32613",
    "montreal": "epsg:26918",
    "london": "EPSG:27700"
}


def meters_to_decimal_degrees(value):
    """
    Rough conversion of meters to decimal degrees only.
    For proper conversion, use a projection.
    :param value: value in meters
    :return: value in decimal degrees
    """
    return value * 360.0 / EARTH_RADIUS


def decimal_degrees_to_meters(value):
    """
    Rough conversion of decimal degrees to meters only.
    For proper conversion, use a projection.
    :param value: value in meters
    :return: value in decimal degrees
    """
    return value * EARTH_RADIUS / 360.0


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


def get_great_circle_distance(p1, p2):
    """
    Great circle distance: using haversine formula for small distances
    :param p1: the first lon-lat point, as a tuple of floats
    :param p2: the second lon-lat point, as a tuple of floats
    :return: distance (in meters?)
    """
    # Great circle distance between two geopoints
    lat1, lon1 = np.radians(p1[1]), np.radians(p1[0])  # get lat, long in radians
    lat2, lon2 = np.radians(p2[1]), np.radians(p2[0])
    d_lat = lat2 - lat1
    d_lon = lon2 - lon1

    #  Haversine formulae
    ro = 2 * np.arcsin(np.sqrt(np.power(np.sin(d_lat / 2), 2) +
                               np.cos(lat1) * np.cos(lat2) * np.power(np.sin(d_lon / 2), 2)))
    gc_dist = ro * EARTH_RADIUS  # earth radius from semimajor axis of WGS 84 spheroid

    return gc_dist


def get_euclidean_distance(p1, p2):
    """
    It's Euclidean distance. Use as a distance metric for UTM coords (not WGS).
    :param p1: the first x-y point, as a tuple of floats
    :param p2: the second x-y point, as a tuple of floats
    :return: a single floating-point distance value
    """
    return np.sqrt(np.power((p2[0] - p1[0]), 2) + np.power((p2[1] - p1[1]), 2))


def get_address_from_coord(lat, lon, proxy):
    """
    Reverse geocoding of some point to get the street address.
    :param lat: latitude of coord
    :param lon: longitude of coord
    :param proxy: proxy for the request, as a string
    :return: string address of given coordinate
    """
    address = ""
    try:
        # build url query to gmap reverse geocoding
        urlquery = u'http://maps.googleapis.com/maps/api/geocode/xml?latlng=%f,%f&sensor=false' % (lat, lon)
        gmap_response = requests.get(urlquery, proxies=proxy)
        gmap_xml = ET.fromstring(gmap_response.text.encode('utf-8'))

        # get address (first result)
        first_result = gmap_xml.find(u'result/formatted_address')
        if not (first_result is None):
            address = first_result.text
    except Exception as e:
        print(str(e))
        pass
    finally:
        return address


def get_vector_magnitude_column(x, y, z, epoch_length=60):
    """
    Per Ruben on Slack:

        I have been working on my scripts lately, and I came across a crucial mistake in the calculation of Activity
        Intensity when using the vector magnitude. It is not in the scripts of INTERACT, because there is no such
        calculation at the moment. The problem comes down to the epoch length at which the vector magnitude is
        calculated. For example, I am using a cut-point at 15 seconds for older adults, based on the vm. It is a mistake
        to calculate the vm at the 1 second level, and then sum it up to 15 seconds. The three axis should be summed up
        to the 15 seconds first and then the vm should be calculated. It is a mistake that is easy to make, but it
        introduces a large bias. I suppose that in we will be using vm-cutpoints in the future, so I thought it might be
        good to alert this now.

    So calculate with epoch length set accordingly (in seconds) when implementing this function.
    :param x: a 1D numpy float array containing the x axis column of a dataset. Must be the same length as y and z.
    :param y: a 1D numpy float array containing the y axis column of a dataset. Must be the same length as x and z.
    :param z: a 1D numpy float array containing the z axis column of a dataset. Must be the same length as x and y.
    :param epoch_length: the length of the epoch in seconds (see above note)
    :return: a 1D numpy array containing the vector magnitude column
    """
    # TODO: implement utility function
    return


def get_projection(lon, lat, city="london", code=None):
    """
    Given WGS readings, project them into UTM for a given city/code.
    :param lon: a 1D numpy float array containing the longitude column of a dataset. Must be the same length as lat.
    :param lat: a 1D numpy float array containing the latitude column of a dataset. Must be the same length as lon.
    :param city: a string containing the name of the city the WGS readings came from.
                Used to determine the correct UTM projection.
    :return: the 1D numpy arrays corresponding to the projected UTM easting and northing coordinates
    """
    if code is None:
    # TODO: Doesn't work if a city is on the boundary of two UTM zones (per Benoit). Figure out solution for future.
        code = PROJ_CODES[city.strip().lower()]

    transformer = Transformer.from_crs(CRS("EPSG:4326"), CRS(code))

    def transf(x, y):
        return transformer.transform(x, y)

    utm_e, utm_n = np.vectorize(transf)(lat, lon)
    return utm_e, utm_n