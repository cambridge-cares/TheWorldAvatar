from admsInputDataRetrieverNew import admsInputDataRetriever
from admsAplWriter import admsAplWriter
from collections import namedtuple
import json
import sys
import config

'''
test = admsInputDataRetriever("http://www.theworldavatar.com/TankID_1574.owl#TankID_1574", config.bldTopnode, {'xmin':84400, 'xmax':84600, 'ymin':451000, 'ymax':451300}, ["http://www.theworldavatar.com/OntoCAPE/OntoCAPE/material/substance/substance.owl#CarbonDioxide"], config.bdnLimit = 2, filterSrc = False)


result = test.get()


for key, value in result.items():
    print(key+":")
    print (value) 


writer = admsAplWriter(result, 'testqunimagedaxuebi.apl')

writer.write()
'''


if __name__ == "__main__":
    print('arguments for ' + sys.argv[0])
    print(' plant = ' + sys.argv[1])
    print(' coordintates = ' + sys.argv[2])
    print(' ADMS working dir = ' + sys.argv[3])
    print(' building data = ' + sys.argv[4])
    print()
    print('top:')
    print(config.bldTopnode)
    
    coordinates = str(sys.argv[2]).replace("'", "\"").replace('#',',');
    coordinates = json.loads(coordinates)

    buildingdata = sys.argv[4].replace("\'","\"")
    buildingdata = json.loads(buildingdata)
    BDN = namedtuple('BDN', ['BldNumBuildings','BldName','BldType','BldX','BldY','BldHeight', 'BldLength', 'BldWidth', 'BldAngle'])
    # TODO-AE URGENT 
    BDN.BldNumBuildings = 25
    BDN.BldName = buildingdata['BldName']
    BDN.BldType = buildingdata['BldType']
    BDN.BldX = buildingdata['BldX']
    BDN.BldY = buildingdata['BldY']
    BDN.BldHeight = buildingdata['BldHeight']
    BDN.BldLength = buildingdata['BldLength']
    BDN.BldWidth = buildingdata['BldWidth']
    BDN.BldAngle = buildingdata['BldAngle']
    

    print('calling admsInputDataRetrieverNew ...')

    test = admsInputDataRetriever(str(sys.argv[1]),config.bldTopnode, coordinates,  ["CO2"   ,"CO" ,  "NO2" ,  "HC" ,  "NOx"], 2, config.bdnLimit,False  )
    result = test.get()
    
    result['Bdn'] = BDN

    for key, value in result.items():
        print(key+":")
        print(value) 

    print('calling admsAplWriter ...')

    # AE
    #writer = admsAplWriter(result, "..\..\workingdir\ADMS\test.apl")
    writer = admsAplWriter(result, sys.argv[3] + '\\test.apl')

    writer.write()
    
    print('finished admsAplWriter')