import sys
import json
from config import Constants
from collections import namedtuple
from admsAplWriter import admsAplWriter
from admsInputDataRetrieverNew import admsInputDataRetriever
from caresjpsutil import PythonLogger

pythonLogger = PythonLogger('admsTest.py')


def get_bdn(buildingdata):
    bdn = namedtuple(Constants.BLD_BDN,
                     [Constants.BLD_NUM, Constants.BLD_NAME, Constants.BLD_TYPE, Constants.BLD_X,
                      Constants.BLD_Y, Constants.BLD_HEIGHT, Constants.BLD_LRNGTH, Constants.BLD_WIDTH,
                      Constants.BLD_ANGLE])
    bdn.BldName = buildingdata[Constants.BLD_NAME]
    bdn.BldNumBuildings = len(bdn.BldName)
    bdn.BldType = buildingdata[Constants.BLD_TYPE]
    bdn.BldX = buildingdata[Constants.BLD_X]
    bdn.BldY = buildingdata[Constants.BLD_Y]
    bdn.BldHeight = buildingdata[Constants.BLD_HEIGHT]
    bdn.BldLength = buildingdata[Constants.BLD_LRNGTH]
    bdn.BldWidth = buildingdata[Constants.BLD_WIDTH]
    bdn.BldAngle = buildingdata[Constants.BLD_ANGLE]

    return bdn


def get_coordinates(coordinates):
    coordinates = json.loads(coordinates)
    xmax = coordinates[Constants.COORD_MAX_CORNER][Constants.COORD_MAX_X]
    ymax = coordinates[Constants.COORD_MAX_CORNER][Constants.COORD_MAX_Y]
    xmin = coordinates[Constants.COORD_MIN_CORNER][Constants.COORD_MIN_X]
    ymin = coordinates[Constants.COORD_MIN_CORNER][Constants.COORD_MIN_Y]
    coordinates[Constants.KEY_MIN_X] = xmin
    coordinates[Constants.KEY_MIN_Y] = ymin
    coordinates[Constants.KEY_MAX_X] = xmax
    coordinates[Constants.KEY_MAX_Y] = ymax

    return coordinates


def main():
    try:
        bdn_data = json.loads(sys.argv[1].replace("'", '"'))
        coor_data = str(sys.argv[2]).replace("'", '"')
        plant = str(sys.argv[3])
        working_dir = str(sys.argv[4])
        coord_sys = sys.argv[5][5:]

        BDN = get_bdn(bdn_data)
        coords = get_coordinates(coor_data)
        pollutants = [Constants.POL_CO2, Constants.POL_CO, Constants.POL_NO2, Constants.POL_HC, Constants.POL_NOX]

        test = admsInputDataRetriever(plant, Constants.BLD_TOPNODE, coords, pollutants, 2, Constants.BLD_LIMIT, False, BDN)
        result = test.get()

        result['Bdn'] = BDN
        result['CoordiSys'] = coord_sys

        writer = admsAplWriter(result, working_dir + '/test.apl')
        writer.write()

    except Exception as e:
        pythonLogger.postErrorToLogServer(e)

if __name__ == "__main__":
    main()

