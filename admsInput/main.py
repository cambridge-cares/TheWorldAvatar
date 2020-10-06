from admsInputDataRetriever import admsInputDataRetriever
from admsAplWriter import admsAplWriter
#sampleBuilding past:"C://Users/Shaocong/WORK/admsInput/samplebuildingkb.owl"
#sampleBuilding new:"C://Users/Shaocong/WORK/admsInput/107_buildings.owl"



test = admsInputDataRetriever("http://www.theworldavatar.com/TankID_1574.owl#TankID_1574", "http://localhost:3030/testBuilding2", {'xmin':84400, 'xmax':84600, 'ymin':451000, 'ymax':451300}, ["http://www.theworldavatar.com/OntoCAPE/OntoCAPE/material/substance/substance.owl#chlorine"], bdnLimit = 2, filterSrc = False)


result = test.get()


for key, value in result.items():
    print(key+":")
    print (value) 


writer = admsAplWriter(result, 'test.apl')

writer.write()