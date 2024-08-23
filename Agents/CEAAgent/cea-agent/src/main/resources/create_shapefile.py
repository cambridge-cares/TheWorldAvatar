"""
create_shapefile takes in the building geometry data retrieved from knowledge graph and
creates a shapefile in the input format required by CEA
"""

from shapely.wkt import loads
from shapely.geometry import mapping
import argparse
import json
import os
import fiona
from fiona.crs import from_epsg

epsg_4326 = "GEOGCS[\"WGS 84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS 84\",6378137,298.257223563,AUTHORITY[\"EPSG\",\"7030\"]],AUTHORITY[\"EPSG\",\"6326\"]],PRIMEM[\"Greenwich\",0,AUTHORITY[\"EPSG\",\"8901\"]],UNIT[\"degree\",0.01745329251994328,AUTHORITY[\"EPSG\",\"9122\"]],AUTHORITY[\"EPSG\",\"4326\"]]"

def create_shapefile(data, crs, shapefile):
    """
    :param geometries: Contains string of coordinates representing building envelope

    :param heights: Height above ground of building

    :param crs: coordinate reference system of the data

    :param shapefile: Name and path of shapefile to be created

    """

    geometry = []
    attributes = []

    ind = 0

    surroundings_flag = ("surroundings" in shapefile)

    floor_height = 3.2  # approximate floor-to-floor height

    for building in data:
        geometries = building["geometry"]
        height = float(building["height"])
        if not surroundings_flag:
            id = building["id"]

        # Convert geometry data to arrays of points
        for geom in geometries:
            geometry.append(loads(geom))

            if not surroundings_flag:
                initial = "B"
            else:
                initial = "S"

            if ind < 100:
                name = initial + str(ind).zfill(3)
            else:
                name = initial + str(ind)

            if not surroundings_flag:
                name += "_" + str(id) # to identify geometries that belong to the same building

            temp = {'Name': name,
                    'floors_bg': 0,
                    'floors_ag': 0,
                    'height_bg': 0.0,
                    'height_ag': 0.0}

            ind += 1

            # calculate number of floors above ground
            floors = round(height / floor_height)

            if floors != 0:
                temp['floors_ag'] = floors
            else:
                temp['floors_ag'] = 1 # number of floors must be at least 1

            # CEA fails if height is less than or equal to 1 so set a minimum height of 1.00001 m
            if height <= 1.0:
                height = 1.00001

            temp['height_ag'] = height

            attributes.append(temp)

    schema = {
        'geometry': 'Polygon',
        'properties': {'Name': 'str:80',
          'floors_bg': 'int:12',
          'floors_ag': 'int:12',
          'height_bg': 'float:15.12',
          'height_ag': 'float:15.12'}
    }

    if (int(crs) == 4326):
        c = epsg_4326
    else:
        c = from_epsg(int(crs))

    # Create a shapefile using Fiona
    with fiona.open(shapefile, 'w', 'ESRI Shapefile', schema, crs = c) as output:
        for polygon, attr in zip(geometry, attributes):
            # Write each polygon and its attributes to the shapefile
            output.write({
                'geometry': mapping(polygon),
                'properties': attr
            })


def main(argv):
    shapefile_file = argv.file_name
    shapefile = argv.zone_file_location + os.sep + shapefile_file

    with open(argv.data_file_location, "r") as f:
        dataString = f.read()

    data_list = json.loads(dataString)

    try:
        create_shapefile(data_list, argv.crs, shapefile)
    except IOError:
        print('Error while processing file: ' + shapefile)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    # add arguments to the parser
    parser.add_argument("data_file_location")
    parser.add_argument("zone_file_location")
    parser.add_argument("crs")
    parser.add_argument("file_name")

    # parse the arguments
    args = parser.parse_args()
    main(args)
