import datetime
import sys
import json
from pyproj import Proj, transform
from config import Constants
from collections import namedtuple
from caresjpsutil import PythonLogger

pythonLogger = PythonLogger('adms_processor.py')


class AdmsProcessor(object):
    def __init__(self):
        self.ENTITY_TYPE_PLANT = "plant"
        self.ENTITY_TYPE_SHIP = "ship"
        self.entity_type = None
        self.bdn_data = None
        self.coor_data = None
        self.entity = None
        self.working_dir = None
        self.coord_sys = None
        self.targetCRS = None
        self.precipitation = None
        self.chimney_iri = None
        self.input = None

    def get_bdn(self, buildingdata):
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

    def get_coordinates(self, coordinates):
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

    def modify_input_for_ship(self, args):
        result = self.input

        result['Met'] = self.working_dir + '/test.met'

        result['Bkg'] = self.working_dir + '/testbackgrnd.bgd'

        if "2326" in args[6][5:]:
            result['terrindicator'] = "1"
        else:
            result['terrindicator'] = "0"

        result['chemindicator'] = "1"
        result['wetindicator'] = "1"
        now = datetime.datetime.now()
        hournow = now.hour + 1
        if not (6 <= hournow <= 18):
            result['night'] = "1"
            result['dirnight'] = "C:\JPS_DATA\working_dir\JPS\ADMS\chemistrynight.AAI"

        else:
            result['night'] = "0"
            result['dirnight'] = ""

        annualprecipitation = self.precipitation * 365 * 24

        if annualprecipitation < 103:
            so2washout = 0.000001 / 500 * annualprecipitation
        else:
            so2washout = 0.0000019 + annualprecipitation * 0.0000000008

        if self.precipitation < 0.5:
            pm10washout = 0.0016
        elif self.precipitation > 4:
            pm10washout = 0.0072
        else:
            pm10washout = 0.00363

        result['so2washout'] = so2washout
        result['pm10washout'] = pm10washout

        return result


    def setVarsFromArgs(self, args):
        self.entity_type = args[1]
        self.bdn_data = json.loads(args[2].replace("'", '"'))
        self.coor_data = str(args[3]).replace("'", '"')
        self.entity = str(args[4])
        self.working_dir = str(args[5])
        self.coord_sys = args[6][5:]
        if self.entity_type == self.ENTITY_TYPE_SHIP:
            self.targetCRS = Proj(init=args[6][:4].lower() + args[6][4:])
            self.precipitation = float(str(args[7]))
            self.chimney_iri = str(args[8])

    def get_input(self, args):
        result = None
        self.setVarsFromArgs(args)
        BDN = self.get_bdn(self.bdn_data)
        coords = self.get_coordinates(self.coor_data)

        if self.entity_type == self.ENTITY_TYPE_PLANT:
            from admsInputDataRetrieverNew import admsInputDataRetriever

            pollutants = [Constants.POL_CO2, Constants.POL_CO, Constants.POL_NO2, Constants.POL_HC, Constants.POL_NOX]
            test = admsInputDataRetriever(self.entity, Constants.BLD_TOPNODE, coords, pollutants, 2, Constants.BLD_LIMIT,
                                          False, BDN)
            self.input = test.get()


        elif self.entity_type == self.ENTITY_TYPE_SHIP:
            from admsInputDataRetrieverChimney import admsInputDataRetriever

            sourceCRS = Proj(init='epsg:4326')
            targetCRS = Proj(init=args[6][:4].lower() + args[6][4:])
            latitudemid = (float(coords[Constants.KEY_MIN_Y]) + float(coords[Constants.KEY_MAX_Y])) / 2
            longitudemid = (float(coords[Constants.KEY_MIN_X]) + float(coords[Constants.KEY_MAX_X])) / 2
            xmid, ymid = transform(targetCRS, sourceCRS, longitudemid, latitudemid)
            chimney_iri = str(args[8])

            pollutants = [Constants.POL_CO2, Constants.POL_CO, Constants.POL_NO2, Constants.POL_HC, Constants.POL_NOX,
                          Constants.POL_PART_001, Constants.POL_PART_SO2, Constants.POL_PART_O3]

            ship_coordinates_list = []
            chimney_iri_list = []

            for ship in json.loads(self.entity):
                x_coordinate_value = float(ship['lon'])
                y_coordinate_value = float(ship['lat'])
                ship_coordinates_list.append(
                    list(transform(sourceCRS, targetCRS, x_coordinate_value, y_coordinate_value)))
                chimney_iri_list.append(chimney_iri)

            test = admsInputDataRetriever(chimney_iri_list, Constants.BLD_TOPNODE, coords, pollutants, 2,
                                          Constants.BLD_LIMIT,
                                          False, BDN, targetCRS)
            self.input = test.get()

            self.input['Lat'] = ymid
            self.input = self.modify_input_for_ship(args)

            for idx in range(len(ship_coordinates_list)):
                result['Src'][idx].setCoordinates(ship_coordinates_list[idx])
                result['Src'][idx].SrcName = "Chimney-{0}".format(idx + 1)

        self.input['Bdn'] = BDN
        self.input['CoordiSys'] = self.coord_sys

        return self.input, self.working_dir

    def save_apl(self, args):
        if args[1] == self.ENTITY_TYPE_PLANT:
            from admsAplWriter import admsAplWriter
        elif args[1] == self.ENTITY_TYPE_SHIP:
            from admsAplWriterShip import admsAplWriter
        writer = admsAplWriter(self.get_input(args) + Constants.FILE_NAME_APL)
        writer.write()


def main(args):
    try:
        processor = AdmsProcessor()
        processor.save_apl(args)
    except Exception as e:
        pythonLogger.postErrorToLogServer(e)


if __name__ == "__main__":
    main(sys.argv)
