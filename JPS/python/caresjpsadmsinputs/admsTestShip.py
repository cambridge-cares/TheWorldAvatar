import datetime
import json
import sys
from caresjpsutil import PythonLogger
from pyproj import Proj, transform
import admsTest
from admsAplWriterShip import admsAplWriter
from admsInputDataRetrieverChimney import admsInputDataRetriever
from config import Constants

pythonLogger = PythonLogger('admsTest.py')


def main():
    try:
        sourceCRS = Proj(init='epsg:4326')
        targetCRS = Proj(init=sys.argv[5][:4].lower() + sys.argv[5][4:])
        bdn_data = json.loads(sys.argv[1].replace("'", '"'))
        coor_data = str(sys.argv[2]).replace("'", '"')
        ships = json.loads(sys.argv[3])
        working_dir = str(sys.argv[4])
        coord_sys = sys.argv[5][5:]
        precipitation = float(str(sys.argv[6]))
        chimney_iri = str(sys.argv[7])

        BDN = admsTest.get_bdn(bdn_data)
        coords = admsTest.get_coordinates(coor_data)
        pollutants = [Constants.POL_CO2, Constants.POL_CO, Constants.POL_NO2, Constants.POL_HC, Constants.POL_NOX,
                      Constants.POL_PART_001, Constants.POL_PART_SO2, Constants.POL_PART_O3]

        ship_coordinates_list = []
        chimney_iri_list = []

        for ship in ships:
            x_coordinate_value = float(ship['lon'])
            y_coordinate_value = float(ship['lat'])
            ship_coordinates_list.append(list(transform(sourceCRS, targetCRS, x_coordinate_value, y_coordinate_value)))
            chimney_iri_list.append(chimney_iri)

        test = admsInputDataRetriever(chimney_iri_list, Constants.BLD_TOPNODE, coords, pollutants, 2,
                                      Constants.BLD_LIMIT,
                                      False, BDN, targetCRS)
        result = test.get()

        pythonLogger.postInfoToLogServer('calling admsAplWriter ...')
        result['Bdn'] = BDN
        result['CoordiSys'] = coord_sys

        latitudemid = (float(coords[Constants.KEY_MIN_Y]) + float(coords[Constants.KEY_MAX_Y])) / 2
        longitudemid = (float(coords[Constants.KEY_MIN_X]) + float(coords[Constants.KEY_MAX_X])) / 2
        xmid, ymid = transform(targetCRS, sourceCRS, longitudemid, latitudemid)

        result['Met'] = working_dir + '/test.met'
        result['Lat'] = ymid
        result['Bkg'] = working_dir + '/testbackgrnd.bgd'

        if "2326" in sys.argv[5][5:]:
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

        annualprecipitation = precipitation * 365 * 24
        if annualprecipitation < 103:
            so2washout = 0.000001 / 500 * annualprecipitation
        else:
            so2washout = 0.0000019 + annualprecipitation * 0.0000000008

        if precipitation < 0.5:
            pm10washout = 0.0016
        elif precipitation > 4:
            pm10washout = 0.0072
        else:
            pm10washout = 0.00363

        result['so2washout'] = so2washout
        result['pm10washout'] = pm10washout

        for idx in range(len(ship_coordinates_list)):
            result['Src'][idx].setCoordinates(ship_coordinates_list[idx])

            result['Src'][idx].SrcName = "Chimney-{0}".format(idx + 1)

        writer = admsAplWriter(result, working_dir + '/test.apl')
        writer.write()

        pythonLogger.postInfoToLogServer('end')

    except Exception as e:
        pythonLogger.postErrorToLogServer(e)


if __name__ == "__main__":
    main()
