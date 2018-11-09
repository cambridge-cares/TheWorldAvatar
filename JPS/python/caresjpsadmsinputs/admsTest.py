import sys
import json
import config
from collections import namedtuple
from admsAplWriter import admsAplWriter
from admsInputDataRetrieverNew import admsInputDataRetriever
from caresjpsutil import PythonLogger

pythonLogger = PythonLogger('admsTest.py')

try:
	pythonLogger.postInfoToLogServer('start')
		
	buildingdata = json.loads(sys.argv[1].replace("'",'"'))
	
	BDN = namedtuple('BDN', ['BldNumBuildings','BldName','BldType','BldX','BldY','BldHeight', 'BldLength', 'BldWidth', 'BldAngle'])
	BDN.BldName = buildingdata['BldName']
	BDN.BldNumBuildings = len(BDN.BldName)
	BDN.BldType = buildingdata['BldType']
	BDN.BldX = buildingdata['BldX']
	BDN.BldY = buildingdata['BldY']
	BDN.BldHeight = buildingdata['BldHeight']
	BDN.BldLength = buildingdata['BldLength']
	BDN.BldWidth = buildingdata['BldWidth']
	BDN.BldAngle = buildingdata['BldAngle']
	
	coordinates = str(sys.argv[2]).replace("'",'"');
	coordinates = json.loads(coordinates)
	
	xmax = coordinates['uppercorner']['upperx']
	ymax = coordinates['uppercorner']['uppery']
	
	xmin = coordinates['lowercorner']['lowerx']
	ymin = coordinates['lowercorner']['lowery']
	
	coordinates['xmin'] = xmin
	coordinates['ymin'] = ymin
	coordinates['xmax'] = xmax
	coordinates['ymax'] = ymax

	
	pythonLogger.postInfoToLogServer('coordinates=' + str(coordinates))
	
	plant = str(sys.argv[3])
	# workingDir = str(sys.argv[4]).replace('/','//')
	workingDir = str(sys.argv[4])
	
	pythonLogger.postInfoToLogServer('workingDir=' + workingDir)

	test = admsInputDataRetriever(plant,config.bldTopnode, coordinates,  ["CO2"   ,"CO" ,  "NO2" ,  "HC" ,  "NOx"], 2, config.bdnLimit,False, BDN)
	result = test.get()
	
	pythonLogger.postInfoToLogServer('calling admsAplWirter ...')
	result['Bdn'] = BDN
	writer = admsAplWriter(result, workingDir + '/test.apl')
	writer.write()

	pythonLogger.postInfoToLogServer('end')

except Exception as e:
	pythonLogger.postErrorToLogServer(e)