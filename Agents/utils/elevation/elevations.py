"""
    This script uses the MapBox RGB Elevation tileset to determine the elevation of a
    particular location.

    Note: As each elevation calculation requires a call to a MapBox API (with your key), be aware
    that is may use up your monthly API calls quota. 

    Requires:
        - A valid MapBox API key

    Authors:
        - mdhillman<@>cmclinnovations.com

    Future improvements:
        - Cache the downloaded tile files.
"""

import os
import time
import wget
import requests
import mercantile
from PIL import Image
from numpy import asarray
from urllib.error import HTTPError
from owslib.wms import WebMapService



# Zoom level (15 is recommended)
ZOOM = 15


def mapBox(apiKey, lat, lng):
    """
        Returns the elevation (in meters) for the input location by using the MapBox
        RGB Elevation tile sets. If some error occurs (i.e. cannot connect to the MapBox
        server, or the location is underwater) a height of NaN will be returned.

        Parameters:
            apiKey - MapBox API Key
            lat - latitude
            lng - longitude

        Returns:
            Height in meters.
    """
            
    # Convert from lat,lon to Slippy tiles coordinates
    tile = mercantile.tile(float(lng), float(lat), ZOOM)

    # Get the actual lat,lon bounds for that tile
    bounds = mercantile.bounds(tile[0], tile[1], tile[2])

    # Download the 256x256 tile image.
    # Note that if this tile is underwater, a HTTPError will be thrown.
    
    try:
        url = "https://api.mapbox.com/v4/mapbox.terrain-rgb/" + str(tile[2]) + "/" + str(tile[0]) + "/" + str(tile[1]) + ".pngraw?access_token=" + apiKey
        filename = wget.download(url, out="tile.pngraw")
    except HTTPError:
        return float("NaN")
    

    # Determine the location of the pixel within the image that
    # corresponds to the input lon,lat position.
    diffX = float(lng) - bounds.west
    diffY = float(lat) - bounds.south
    xRange = bounds.east - bounds.west
    yRange = bounds.north - bounds.south
    pixelX = int((diffX / xRange) * 256)
    pixelY = int((diffY / yRange) * 256)
        
    if pixelX < 0 or pixelX > 255:
        print("ERROR: Could not calculate the pixel location!")
        return float("NaN")

    if pixelY < 0 or pixelY > 255:
        print("ERROR: Could not calculate the pixel location!")
        return float("NaN")

    # Get the pixel's RGB data
    image = Image.open(filename)
    image = image.convert("RGB")
    pixel = image.getpixel((pixelX, pixelY))

    # Decode data into height (from MapBox)
    height = -10000.0 + (pixel[0] * 256.0 * 256.0 + pixel[1] * 256.0 + pixel[2]) * 0.1

    # Remove the tile image and return
    os.remove(filename)
    return height


def openElevation(lat, lng):
    """
        Returns the elevation (in meters) for the input location by using the Open
        Elevation API. Note that an artifical delay is added here to stop flooding
        of the API end point.

        Parameters:
            lat - latitude
            lng - longitude

        Returns:
            Height in meters.
    """
    url = "https://api.open-elevation.com/api/v1/lookup"
    request = requests.get(url+"?locations="+str(lat)+","+str(lng), verify=False)
    try:
        results = request.json().get('results')
        time.sleep(1)
        if 0 < len(results):
            return results[0].get('elevation')
        else:
            print('Could not access Open Elevation API!')
    except:
        print('JSON parsing failed: '+str(request))


def lidarWMS(lat, lng, i):
    """
        Returns the elevation (in meters) for the input location by using the
        LIDAR WMS endpoint from DEFRA.

        Parameters:
            lat - latitude
            lng - longitude

        Returns:
            Height in meters.
    """
    wms = WebMapService("https://environment.data.gov.uk/spatialdata/lidar-composite-digital-terrain-model-dtm-1m-2020/wms", version="1.1.1")

    latLow = lat - (0.001) 
    latHigh = lat + (0.001) 
    lonLow = lng - (0.001) 
    lonHigh = lng + (0.001) 

    image = wms.getmap(
        layers=["1"],
        styles=["default"],
        srs="EPSG:4326",
        bbox=(lonLow, latLow, lonHigh, latHigh),
        size=(500, 500),
        format="image/jpeg",
        transparent=False
    )    

    # Write image (temporarily)
    out = open("temp-defra-lidar-" + i + ".jpeg", 'wb')
    out.write(image.read())
    out.close()

    # Get the pixel in the center
    imagePIL = Image.open("temp-defra-lidar-" + i + ".jpeg")
    imagePIL = imagePIL.convert("RGB")
    pixel = imagePIL.getpixel((249, 249))

    # Calculate height
    height = -10000.0 + (pixel[0] * 256.0 * 256.0 + pixel[1] * 256.0 + pixel[2]) * 0.1
    height = height / 1000

    # Clean up and return
    os.remove("temp-defra-lidar-" + i + ".jpeg")
    return height
 
