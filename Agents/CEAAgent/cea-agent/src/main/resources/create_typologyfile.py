from geopandas import GeoDataFrame as Gdf 
import numpy as np
import pysal.lib
import argparse
import json
import os


TYPE_MAPPING = {
    int: ('N', 20, 0),
    np.int32: ('N', 20, 0),
    np.int64: ('N', 20, 0),
    float: ('N', 36, 15),
    np.float64: ('N', 36, 15),
    str: ('C', 25, 0),
    np.bool_: ('L', 1, 0)}

COLUMNS_ZONE_TYPOLOGY = ['Name', 'STANDARD', 'YEAR', '1ST_USE', '1ST_USE_R', '2ND_USE', '2ND_USE_R', '3RD_USE',
                         '3RD_USE_R']


def dataframe_to_dbf(df, dbf_path, specs=None):
    """
    Function taken from CityEnergyAnalyst
    
    Given a pandas Dataframe, write a dbase database to ``dbf_path``.

    :type df: pandas.Dataframe
    :type dbf_path: str
    :param specs: A list of column specifications for the dbase table. Each column is specified by a tuple (datatype,
        size, decimal) - we support ``datatype in ('N', 'C')`` for strings, integers and floating point numbers, if
        no specs are provided (see ``TYPE_MAPPING``)
    :type specs: list[tuple(str, int, int)]
    """
    if specs is None:
        types = [type(df[i].iloc[0]) for i in df.columns]
        specs = [TYPE_MAPPING[t] for t in types]

    # handle case of strings that are longer than 25 characters (e.g. for the "Name" column)
    for i in range(len(specs)):
        t, l, d = specs[i]  # type, length, decimals
        if t == 'C':
            l = max(l, df[df.columns[i]].apply(len).max())
            specs[i] = t, l, d

    dbf = pysal.lib.io.open(dbf_path, 'w', 'dbf')
    dbf.header = list(df.columns)
    dbf.field_spec = specs
    for row in range(len(df)):
        dbf.write(df.iloc[row])
    dbf.close()
    return dbf_path

def create_typologyfile(zone_geometry_path, data, typologyfile):
    """
    Adapted from CityEnergyAnalyst
    
    Create a typology file with input building usage.
    
    :param zone_geometry_path: Path to geometry file zone.shp
    
    :param usage: Building usage

    :param typologyfile: Name and path of typologyfile to be created

    """

    zone = Gdf.from_file(zone_geometry_path).drop('geometry', axis = 1)
    
    zone['STANDARD'] = 'STANDARD1'
    zone['YEAR'] = 2020
    zone['1ST_USE'] = 'MULTI_RES'
    zone['1ST_USE_R'] = 1.0
    zone['2ND_USE'] = "NONE"
    zone['2ND_USE_R'] = 0.0
    zone['3RD_USE'] = "NONE"
    zone['3RD_USE_R'] = 0.0

    ind = 0

    for building in data:
        num = len(building["geometry"])
        usage = building["usage"]

        # assign the same usages and weights for the different geometries that belong to the same building
        for i in range(num):
            j = 0

            # iterate through the different usages and their respective weighting
            for use, weight in usage.items():
                zone.at[i, COLUMNS_ZONE_TYPOLOGY[2*j+3]] = use
                zone.at[i, COLUMNS_ZONE_TYPOLOGY[2*j+4]] = weight
                j += 1

            ind += 1
        
    dataframe_to_dbf(zone[COLUMNS_ZONE_TYPOLOGY], typologyfile)
    

def main(argv):
    typologyfile_file = "typology.dbf"
    typologyfile = argv.file_location + os.sep + typologyfile_file
    zone_geometry_path = argv.file_location + os.sep + "zone.shp"

    with open(argv.data_file_location, "r") as f:
        dataString = f.read()

    data_list = json.loads(dataString)

    try:
        create_typologyfile(zone_geometry_path, data_list, typologyfile)
    except IOError:
        print('Error while processing file: ' + typologyfile)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    # add arguments to the parser
    parser.add_argument("data_file_location")
    parser.add_argument("file_location")

    # parse the arguments
    args = parser.parse_args()
    main(args)
