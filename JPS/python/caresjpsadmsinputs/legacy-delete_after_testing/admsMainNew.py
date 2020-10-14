from admsInputDataRetrieverNew import admsInputDataRetriever
from admsAplWriter import admsAplWriter
from collections import namedtuple
from caresjpsutil import PythonLogger
import json
import sys
import config
import cobbling




if __name__ == "__main__":
    
    pythonLogger = PythonLogger('admsMainNew.py')
    try:
        pythonLogger.postInfoToLogServer('start with ' + sys.argv[0] + '  plant = ' + sys.argv[1] + '  coordinates = ' + sys.argv[2] 
                                         + '  ADMS working dir = ' + sys.argv[3] + '  top = ' + config.bldTopnode)
        cobbling.run()
        coordinates = str(sys.argv[2]).replace("'", "\"").replace('#',',');
        coordinates = json.loads(coordinates)
    
        buildingdata = sys.argv[4].replace("\'","\"")
        buildingdata = json.loads(buildingdata)
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
        
        pythonLogger.postInfoToLogServer('coordinates=' + str(coordinates))
        
        plant = str(sys.argv[1])
        # workingDir = str(sys.argv[4]).replace('/','//')
        workingDir = str(sys.argv[3])
    
        pythonLogger.postInfoToLogServer('workingDir=' + workingDir)
        
        test = admsInputDataRetriever(plant,config.bldTopnode, coordinates,  ["CO2"   ,"CO" ,  "NO2" ,  "HC" ,  "NOx"], 2, config.bdnLimit,False, BDN)
        result = test.get()
        
        pythonLogger.postInfoToLogServer('calling admsAplWriter ...')
        result['Bdn'] = BDN
        writer = admsAplWriter(result, workingDir + '/test.apl')
        writer.write()
        
        pythonLogger.postInfoToLogServer('end')
        
    except Exception as e:
        pythonLogger.postInfoToLogServer(e)