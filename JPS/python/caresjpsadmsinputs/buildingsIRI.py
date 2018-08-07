from admsInputDataRetriever import admsInputDataRetriever
import json
import sys
import time


start = time.time()
coordinates = json.loads(sys.argv[1].replace('#',',').replace("'",'"'))
#coordinates =  {'xmin':0, 'xmax':794900, 'ymin':0, 'ymax':4546800}
ai = admsInputDataRetriever('http://www.theworldavatar.com/Plant-001.owl', 'http://www.theworldavatar.com/damecoolquestion/buildingsLite/sparql',coordinates,["CO2"],2,25, False)

ai.connectDB('http://www.theworldavatar.com/damecoolquestion/buildingsLite/sparql')
bdns = ai.filterBdns()
# bdns = json.loads(bdns.strip())
bdns = json.dumps(bdns)
print('###', bdns, '###')


# print('it took ', time.time() - start, 'seconds')

