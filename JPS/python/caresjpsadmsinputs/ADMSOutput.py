from pyproj import Proj, transform
import json
import sys
import csv
import math

admsCRS = Proj(init='epsg:28992')
osmCRS = Proj(init='epsg:4326')

def getADMSOutput():

    # clicked coordinates is received and stored in python float variables
    coordinatesLatLon = json.loads(sys.argv[1])

    inputLat = float(coordinatesLatLon[0])
    inputLon = float(coordinatesLatLon[1])


    # iterate through ADMS output file to find grid point closest to clicked coordinates
    with open(r'C:\Users\WE\Dropbox (Cambridge CARES)\IRP3 CAPRICORN shared folder\WENG\buildingontokinlevels.gst') as f:
        reader = csv.reader(f, delimiter=',')

        # Skip header
        next(reader, None)

        # First entry
        firstEntry = next(reader, None)
        firstLon, firstLat = transform(admsCRS, osmCRS, float(firstEntry[4]), float(firstEntry[5]))

        shortestDistance = math.sqrt(math.pow((inputLon - firstLon), 2) + math.pow((inputLat - firstLat), 2))

        entryShortestDistance = [firstLat, firstLon,
                                 float(firstEntry[7]), float(firstEntry[8]),
                                 float(firstEntry[9]), float(firstEntry[10])]

        for row in reader:
            lon, lat = transform(admsCRS, osmCRS, float(row[4]), float(row[5]))

            distance = math.sqrt(math.pow((inputLon - lon), 2) + math.pow((inputLat - lat), 2))

            if distance < shortestDistance:
                entryShortestDistance = [lat, lon, float(row[7]), float(row[8]), float(row[9]), float(row[10])]
                shortestDistance = distance

        return json.dumps(entryShortestDistance)

if __name__ == "__main__":
    print(getADMSOutput())
