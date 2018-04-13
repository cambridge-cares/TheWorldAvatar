from admsInputDataRetriever import admsInputDataRetriever
from admsAplWriter import admsAplWriter
#sampleBuilding past:"C://Users/Shaocong/WORK/admsInput/samplebuildingkb.owl"
#sampleBuilding new:"C://Users/Shaocong/WORK/admsInput/107_buildings.owl"
from pyproj import Proj, transform
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
    admsCRS = Proj(init='epsg:28992')
    osmCRS = Proj(init='epsg:4326')
    # coordinates = json.loads(sys.argv[2])

#     xmin, ymin = transform(osmCRS,admsCRS , float(coordinates['xmin']), float(coordinates['ymin']))
#     xmax, ymax = transform( osmCRS,admsCRS, float(coordinates['xmax']), float(coordinates['ymax']))
# 
#     coordinates['xmin'] = xmin
#     coordinates['xmax'] = xmax
#     coordinates['ymin'] = ymin
#     coordinates['ymax'] = ymax
    
    coordinates = str(sys.argv[2]).replace("'", "\"").replace("#",",");
    coordinates = json.loads(coordinates)
    test = admsInputDataRetriever(str(sys.argv[1]), config.bldTopnode, coordinates, ["CO2"],2,config.bdnLimit,False  )
    result = test.get()


    for key, value in result.items():
        print(key+":")
        print (value) 


    writer = admsAplWriter(result, 'test.apl')

    writer.write()
