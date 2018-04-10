from admsInputDataRetriever import admsInputDataRetriever
from admsAplWriter import admsAplWriter
import time 


#sampleBuilding past:"C://Users/Shaocong/WORK/admsInput/samplebuildingkb.owl"
#sampleBuilding new:"C://Users/Shaocong/WORK/admsInput/107_buildings.owl"

start = time.time()

test = admsInputDataRetriever("http://www.theworldavatar.com/Plant-001.owl", "http://www.theworldavatar.com/damecoolquestion/buildings/query", {'xmin':84400, 'xmax':84600, 'ymin':451000, 'ymax':451300}, ["http://www.theworldavatar.com/OntoCAPE/OntoCAPE/material/substance/substance.owl#chlorine"], bdnLimit = 2, filterSrc = False)


result = test.get()


for key, value in result.items():
    print(key+":")
    print (value) 


writer = admsAplWriter(result, 'test.apl')

writer.write()

print('time took:', time.time() - start)
