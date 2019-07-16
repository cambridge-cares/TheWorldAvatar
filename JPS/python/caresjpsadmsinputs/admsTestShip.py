import datetime
import json
import sys
from collections import namedtuple
from caresjpsutil import PythonLogger
from pyproj import Proj, transform
import config
from admsAplWriterShip import admsAplWriter
from admsInputDataRetrieverChimney import admsInputDataRetriever

pythonLogger = PythonLogger('admsTest.py')
sourceCRS = Proj(init='epsg:4326')
targetCRS = Proj(init=sys.argv[5][:4].lower() + sys.argv[5][4:])

try:

    buildingdata = json.loads(sys.argv[1].replace("'", '"'))
    BDN = namedtuple('BDN',
                     ['BldNumBuildings', 'BldName', 'BldType', 'BldX', 'BldY', 'BldHeight', 'BldLength', 'BldWidth',
                      'BldAngle'])
    BDN.BldName = buildingdata['BldName']
    BDN.BldNumBuildings = len(BDN.BldName)
    BDN.BldType = buildingdata['BldType']
    BDN.BldX = buildingdata['BldX']
    BDN.BldY = buildingdata['BldY']
    BDN.BldHeight = buildingdata['BldHeight']
    BDN.BldLength = buildingdata['BldLength']
    BDN.BldWidth = buildingdata['BldWidth']
    BDN.BldAngle = buildingdata['BldAngle']

    coordinates = str(sys.argv[2]).replace("'", '"')
    coordinates = json.loads(coordinates)

    xmax = coordinates['uppercorner']['upperx']
    ymax = coordinates['uppercorner']['uppery']
    xmin = coordinates['lowercorner']['lowerx']
    ymin = coordinates['lowercorner']['lowery']

    coordinates['xmin'] = xmin
    coordinates['ymin'] = ymin
    coordinates['xmax'] = xmax
    coordinates['ymax'] = ymax

    latitudemid = (float(ymin) + float(ymax)) / 2
    longitudemid = (float(xmin) + float(xmax)) / 2
    xmid, ymid = transform(targetCRS, sourceCRS, longitudemid, latitudemid)

    ships = json.loads(sys.argv[3])
    chimney_iri = str(sys.argv[7])

    ship_coordinates_list = []
    chimney_iri_list = []

    for ship in ships:
        x_coordinate_value = float(ship['lon'])
        y_coordinate_value = float(ship['lat'])
        ship_coordinates_list.append(list(transform(sourceCRS, targetCRS, x_coordinate_value, y_coordinate_value)))
        chimney_iri_list.append(chimney_iri)

    workingDir = str(sys.argv[4])

    pythonLogger.postInfoToLogServer('workingDir=' + workingDir)

    test = admsInputDataRetriever(chimney_iri_list, config.bldTopnode, coordinates,
                                  ["CO2", "CO", "NO2", "HC", "NOx", "Particulate001", "SO2", "O3"], 2, config.bdnLimit,
                                  False, BDN, targetCRS)
    result = test.get()

    pythonLogger.postInfoToLogServer('calling admsAplWriter ...')
    result['Bdn'] = BDN
    result['CoordiSys'] = sys.argv[5][5:]

    result['Met'] = workingDir + '/test.met'
    result['Lat'] = ymid
    result['Bkg'] = workingDir + '/testbackgrnd.bgd'

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
        result['dirnight'] = "C:\JPS_DATA\workingdir\JPS\ADMS\chemistrynight.AAI"

    else:
        result['night'] = "0"
        result['dirnight'] = ""

    precipitation = float(str(sys.argv[6]))
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

    writer = admsAplWriter(result, workingDir + '/test.apl')
    writer.write()

    pythonLogger.postInfoToLogServer('end')

except Exception as e:
    pythonLogger.postErrorToLogServer(e)
