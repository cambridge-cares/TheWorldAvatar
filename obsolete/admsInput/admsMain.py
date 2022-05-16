from admsInputDataRetriever import admsInputDataRetriever
from admsAplWriter import admsAplWriter
#sampleBuilding past:"C://Users/Shaocong/WORK/admsInput/samplebuildingkb.owl"
#sampleBuilding new:"C://Users/Shaocong/WORK/admsInput/107_buildings.owl"

import json
import sys
import config

'''
test = admsInputDataRetriever("http://www.theworldavatar.com/TankID_1574.owl#TankID_1574", config.bldTopnode, {'xmin':84400, 'xmax':84600, 'ymin':451000, 'ymax':451300}, ["http://www.theworldavatar.com/OntoCAPE/OntoCAPE/material/substance/substance.owl#CarbonDioxide"], config.bdnLimit = 2, filterSrc = False)


result = test.get()


for key, value in result.items():
    print(key+":")
    print (value) 


writer = admsAplWriter(result, 'test.apl')

writer.write()
'''

if __name__ == "__main__":
    print('top:')
    print(config.bldTopnode)
    print(sys.argv[2])
    test = admsInputDataRetriever(str(sys.argv[1]), config.bldTopnode, json.loads(sys.argv[2]), ["http://www.theworldavatar.com/OntoCAPE/OntoCAPE/material/substance/substance.owl#CarbonDioxide"], bdnLimit = config.bdnLimit, filterSrc = False  )
    result = test.get()


    for key, value in result.items():
        print(key+":")
        print (value) 


    writer = admsAplWriter(result, 'test.apl')

    writer.write()