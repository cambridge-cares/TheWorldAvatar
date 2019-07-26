import datetime
import sys
import json
from pyproj import Proj, transform
from collections import namedtuple
from caresjpsutil import PythonLogger
from config import Constants
from adms_apl_builder import AplDirector, AdmsAplPlantBuilder, AdmsAplShipBuilder

pythonLogger = PythonLogger('adms_processor.py')


class AdmsProcessor(object):
    def __init__(self):
        self.sourceCRS = Proj(init=Constants.CRS_EPSG_4326)
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
        self.coords = None
        self.BDN = None

    @staticmethod
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

    @staticmethod
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
    
    def get_washouts(self):
        annual_precipitation = self.precipitation * 365 * 24

        if annual_precipitation < 103:
            so2washout = 0.000001 / 500 * annual_precipitation
        else:
            so2washout = 0.0000019 + annual_precipitation * 0.0000000008

        if self.precipitation < 0.5:
            pm10washout = 0.0016
        elif self.precipitation > 4:
            pm10washout = 0.0072
        else:
            pm10washout = 0.00363
        
        return so2washout, pm10washout
    
    def set_input_ship_src_geo(self, ship_coordinates_list):
        for idx in range(len(ship_coordinates_list)):
            self.input[Constants.KEY_SRC][idx].setCoordinates(ship_coordinates_list[idx])
            self.input[Constants.KEY_SRC][idx].SrcName = Constants.STR_CHIMNEY.format(idx + 1)
        
        latitudemid = (float(self.coords[Constants.KEY_MIN_Y]) + float(self.coords[Constants.KEY_MAX_Y])) / 2
        longitudemid = (float(self.coords[Constants.KEY_MIN_X]) + float(self.coords[Constants.KEY_MAX_X])) / 2
        xmid, ymid = transform(self.targetCRS, self.sourceCRS, longitudemid, latitudemid)
        
        self.input[Constants.KEY_LAT.title()] = ymid

    def set_input_ship_indicators(self, args):
        if str(2326) in args[6][5:]:
            self.input[Constants.KEY_INDICATOR_TERR] = 1
        else:
            self.input[Constants.KEY_INDICATOR_TERR] = 0

        self.input[Constants.KEY_INDICATOR_CHEM] = 1
        self.input[Constants.KEY_INDICATOR_WET] = 1
        
    def set_grid_size(self, args):
        if str(2326) in args[6][5:]:
            self.input[Constants.GRD_X] = 80
            self.input[Constants.GRD_Y] = 80
        else:
            self.input[Constants.GRD_X] = 80
            self.input[Constants.GRD_Y] = 80

    def set_input_ship_night(self):
        now = datetime.datetime.now()
        hournow = now.hour + 1
        if not (6 <= hournow <= 18):
            self.input[Constants.KEY_NIGHT] = 1
            self.input[Constants.KEY_DIR_NIGHT] = Constants.FILEPATH_NIGHT
        else:
            self.input[Constants.KEY_NIGHT] = 0
            self.input[Constants.KEY_DIR_NIGHT] = ""

    def modify_input_for_ship(self, args, ship_coordinates_list):
        self.set_input_ship_src_geo(ship_coordinates_list)
        self.set_input_ship_indicators(args)
        self.set_input_ship_night()
        self.set_grid_size(args)
        
        self.input[Constants.KEY_MET] = self.working_dir + Constants.FILENAME_MET
        self.input[Constants.KEY_BKG] = self.working_dir + Constants.FILENAME_BGD

        so2washout, pm10washout = self.get_washouts()
        self.input[Constants.KEY_WASHOUT_SO2] = so2washout
        self.input[Constants.KEY_WASHOUT_PM10] = pm10washout

    def set_vars_from_args(self, args):
        self.entity_type = args[1]
        self.bdn_data = json.loads(args[2].replace("'", '"'))
        self.coor_data = str(args[3]).replace("'", '"')
        self.entity = str(args[4])
        self.working_dir = str(args[5])
        self.coord_sys = args[6][5:]
        if self.entity_type == Constants.ENTITY_TYPE_SHIP:
            self.targetCRS = Proj(init=args[6][:4].lower() + args[6][4:])
            self.precipitation = float(str(args[7]))
            self.chimney_iri = str(args[8])

    def get_ship_coordinates(self):
        ship_coordinates_list = []
        chimney_iri_list = []

        for ship in json.loads(self.entity.replace("'", '"')):
            x_coordinate_value = float(ship[Constants.KEY_LON])
            y_coordinate_value = float(ship[Constants.KEY_LAT])
            ship_coordinates_list.append(
                list(transform(self.sourceCRS, self.targetCRS, x_coordinate_value, y_coordinate_value)))
            chimney_iri_list.append(self.chimney_iri)
        self.entity = chimney_iri_list

        return ship_coordinates_list

    def retrieve_input(self):
        retriever = None
        if self.entity_type == Constants.ENTITY_TYPE_PLANT:
            from admsInputDataRetrieverNew import admsInputDataRetriever

            pollutants = [Constants.POL_CO2, Constants.POL_CO, Constants.POL_NO2, Constants.POL_HC, Constants.POL_NOX]
            retriever = admsInputDataRetriever(self.entity, Constants.BLD_TOPNODE, self.coords, pollutants, 2,
                                               Constants.BLD_LIMIT, False, self.BDN)
        elif self.entity_type == Constants.ENTITY_TYPE_SHIP:
            from admsInputDataRetrieverChimney import admsInputDataRetriever
            pollutants = [Constants.POL_CO2, Constants.POL_CO, Constants.POL_NO2, Constants.POL_HC, Constants.POL_NOX,
                          Constants.POL_PART_001, Constants.POL_PART_SO2, Constants.POL_PART_O3]
            retriever = admsInputDataRetriever(self.entity, Constants.BLD_TOPNODE, self.coords, pollutants, 2,
                                               Constants.BLD_LIMIT, False, self.BDN, self.targetCRS)
        self.input = retriever.get()

    def get_input(self, args):
        self.set_vars_from_args(args)
        self.BDN = self.get_bdn(self.bdn_data)
        self.coords = self.get_coordinates(self.coor_data)
        ship_coordinates_list = self.get_ship_coordinates()
        self.retrieve_input()

        if self.entity_type == Constants.ENTITY_TYPE_SHIP:
            self.modify_input_for_ship(args, ship_coordinates_list)

        self.input[Constants.KEY_BDN] = self.BDN
        self.input[Constants.KEY_COORD_SYS] = int(self.coord_sys)

    def save_apl(self, args):
        self.get_input(args)
        builder = None
        if args[1] == Constants.ENTITY_TYPE_PLANT:
            builder = AdmsAplPlantBuilder(self.input)
        elif args[1] == Constants.ENTITY_TYPE_SHIP:
            builder = AdmsAplShipBuilder(self.input)
        director = AplDirector()
        director.set_builder(builder)
        apl = director.get_apl()
        spec = apl.specification()
        with open(self.working_dir + Constants.FILE_NAME_APL, 'w') as file:
            file.write(spec)
        file.close()


def main(args):
    try:
        processor = AdmsProcessor()
        processor.save_apl(args)
    except Exception as e:
        pythonLogger.postErrorToLogServer(e)


if __name__ == "__main__":
    main(sys.argv)
