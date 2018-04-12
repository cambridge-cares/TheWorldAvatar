from admsInputDataRetriever import admsInputDataRetriever
import json
import sys
import time


start = time.time()
# coordinates = json.loads(sys.argv[1])
coordinates =  {'xmin':0, 'xmax':794900, 'ymin':0, 'ymax':4546800}
ai = admsInputDataRetriever('http://www.theworldavatar.com/Plant-001.owl', 'http://www.theworldavatar.com/damecoolquestion/buildingsLite/sparql',coordinates,["http://www.theworldavatar.com/OntoCAPE/OntoCAPE/material/substance/substance.owl#CarbonDioxide"],2,25, False)

ai.connectDB('http://www.theworldavatar.com/damecoolquestion/buildingsLite/sparql')
bdns = ai.filterBdns()
print('###', bdns, '###')


# print('it took ', time.time() - start, 'seconds')

