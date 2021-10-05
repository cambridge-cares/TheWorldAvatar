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
import wget
import mercantile
from PIL import Image
from numpy import asarray
from urllib.error import HTTPError

# Zoom level (15 is recommended)
ZOOM = 15


def getElevation(apiKey, lat, lon):
	"""
		Returns the elevation (in meters) for the input location by using the MapBox
		RGB Elevation tile sets. If some error occurs (i.e. cannot connect to the MapBox
		server, or the location is underwater) a height of NaN will be returned.

		Parameters:
			apiKey - MapBox API Key
			lat - latitude
			lon - longitude

		Returns:
			Height in meters.
	"""
			
	# Convert from lat,lon to Slippy tiles coordinates
	tile = mercantile.tile(float(lon), float(lat), ZOOM)

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
	diffX = float(lon) - bounds.west
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

	# Read the image
	image = Image.open(filename)
	imageRGB = image.convert("RGB")

	# Get the pixel's RGB data
	pixel = imageRGB.getpixel((pixelX, pixelY))

	# Decode data into height (from MapBox)
	height = -10000.0 + (pixel[0] * 256.0 * 256.0 + pixel[1] * 256.0 + pixel[2]) * 0.1

	# Remove the tile image and return
	os.remove(filename)
	return height