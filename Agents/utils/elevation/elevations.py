"""
    This script queries a remote GeoServer using it's WMS endpoint to determine
    the elevation of a particular location. Note that this relies on having 
    such a server, and it containing elevation data for the area around
    the requested location.

    Requires:
        - A remote GeoServer with a valid WMS endpoint

    Authors:
        - mdhillman<@>cmclinnovations.com

    Future improvements:
        - Cache the downloaded tile files.
"""

import json
from owslib.wms import WebMapService

def getHeight(wmsEndpoint, lat, lng):
    """
        Returns the elevation (in meters) for the input location by using the
        a remote WMS endpoint. Note that the remote WMS may return a height
        of 0 meters if the tile containing that location has not been loaded.

        Parameters:
            wmsEndpoint - URL for endpoint
            lat - latitude
            lng - longitude

        Returns:
            Height in meters, NaN if cannot be determined
    """

    # URL for WMS service
    wms = WebMapService(wmsEndpoint, version="1.1.1")

    # Get a 10m^2 bounding box around our point of interest
    latLow = lat - (0.00005) 
    latHigh = lat + (0.00005) 
    lonLow = lng - (0.00005) 
    lonHigh = lng + (0.00005) 

    # Make a WMS call to get the height at the center of that box
    result = wms.getfeatureinfo(
        query_layers=["elevation:LIDAR-DTM-1m-2020"],
        layers=["elevation:LIDAR-DTM-1m-2020"],
        info_format="application/json",
        xy="128,128",
        size="256,256",
        srs="EPSG:4326",
        bbox=(lonLow, latLow, lonHigh, latHigh)
    )    

    try:
        # Make request and parse resulting JSON
        jsonResult = json.loads(result.read())
        height = jsonResult["features"][0]["properties"]["GRAY_INDEX"]
        return height
    except:
        return float("NaN")