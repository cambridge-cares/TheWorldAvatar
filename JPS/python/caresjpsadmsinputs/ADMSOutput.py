# from pyproj import Proj, transform
import json
import sys
import csv
import math

import requests
import logging
from io import StringIO

class LevelFilter(logging.Filter):
    def __init__(self, levels):
        self.levels = levels

    def filter(self, record):
        return record.levelno in self.levels

# admsCRS = Proj(init='epsg:28992')
# osmCRS = Proj(init='epsg:4326')

def getADMSOutput():
    
    # clicked coordinates is received and stored in Python float variables
    filePath = sys.argv[1]
    coordinatesLatLon = json.loads(sys.argv[2])

    inputLat = float(coordinatesLatLon[0])
    inputLon = float(coordinatesLatLon[1])


    # iterate through ADMS output file to find grid point closest to clicked coordinates
    # precondition: input coordinates must be in the format of admsCRS (epsg:28992)
    with open(filePath) as f:
        reader = csv.reader(f, delimiter=',')

        # Skip header
        next(reader, None)

        # First entry
        firstEntry = next(reader, None)
        firstLat = float(firstEntry[4])
        firstLon =  float(firstEntry[5])

        shortestDistance = math.sqrt(math.pow((inputLon - firstLon), 2) + math.pow((inputLat - firstLat), 2))

        entryShortestDistance = [firstLat, firstLon,
                                 float(firstEntry[7]), float(firstEntry[8]),
                                 float(firstEntry[9]), float(firstEntry[10])]

        for row in reader:
            lat = float(row[4])
            lon = float(row[5])
            # lon, lat = transform(admsCRS, osmCRS, float(row[4]), float(row[5]))

            distance = math.sqrt(math.pow((inputLon - lon), 2) + math.pow((inputLat - lat), 2))

            if distance < shortestDistance:
                entryShortestDistance = [lat, lon, float(row[7]), float(row[8]), float(row[9]), float(row[10])]
                shortestDistance = distance

        return json.dumps(entryShortestDistance)

if __name__ == "__main__":
    log_stream = StringIO()
    logging.basicConfig(stream=log_stream,
                        level=logging.INFO,
                        format='%(asctime)s %(levelname)s [Python] %(module)s{} %(message)s'.format(".py"))
    logging.getLogger().addFilter(LevelFilter((logging.INFO, logging.WARNING, logging.ERROR)))

    logging.info('start of ADMSOutput.py')
    requests.post('http://localhost:8080/JPS_BASE/LogServer', data=log_stream.getvalue())
    log_stream.seek(0)
    log_stream.truncate(0)
    
    print(getADMSOutput())
    
    logging.info('end of ADMSOutput.py')
    requests.post('http://localhost:8080/JPS_BASE/LogServer', data=log_stream.getvalue())
    log_stream.seek(0)
    log_stream.truncate(0)
