import sys
import json
import config
import rdflib
import requests
from collections import namedtuple
from admsAplWriterShip import admsAplWriter
from admsInputDataRetrieverChimney import admsInputDataRetriever
from caresjpsutil import PythonLogger
from pyproj import Proj, transform

pythonLogger = PythonLogger('admsTest.py')
# sourceCRS = Proj(init='epsg:28992')
sourceCRS = Proj(init='epsg:4326')
# targetCRS = Proj(init='epsg:3857')
targetCRS = Proj(init=sys.argv[5][:4].lower() + sys.argv[5][4:])

try:
#     pythonLogger.postInfoToLogServer('start')
        
    buildingdata = json.loads(sys.argv[1].replace("'",'"'))
    print("BUILDING")
    print(buildingdata)
    print("")
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
    
#     print(transform(sourceCRS, targetCRS, xmax, ymax))
#     print(transform(sourceCRS, targetCRS, xmin, ymin))

    coordinates['xmin'] = xmin
    coordinates['ymin'] = ymin
    coordinates['xmax'] = xmax
    coordinates['ymax'] = ymax
    
    
#     pythonLogger.postInfoToLogServer('coordinates=' + str(coordinates))
    ships = json.loads(sys.argv[3])
    
    ship_coordinates_list = []
    chimney_iri_list = []
    
    for ship in ships:
        print(ship)
        graph = rdflib.Graph().parse(ship)
        query_string_coordinates = """
            PREFIX j1: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
            PREFIX j4: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
      
            SELECT ?coordinateX_value ?coordinateY_value
            WHERE {{ 
                <{0}> j4:hasGISCoordinateSystem ?coordinateSystem .
                    ?coordinateSystem j4:hasProjectedCoordinate_x ?coordinateX .
                        ?coordinateX j1:hasValue ?coordinateX_V .
                            ?coordinateX_V j1:numericalValue ?coordinateX_value .
                    ?coordinateSystem j4:hasProjectedCoordinate_y ?coordinateY .
                        ?coordinateY j1:hasValue ?coordinateY_V .
                            ?coordinateY_V j1:numericalValue ?coordinateY_value .
            }}
        """.format(ship)
     
        query_results = graph.query(query_string_coordinates).bindings
      
        x_coordinate_value = float(query_results[0]['coordinateX_value'].toPython())
        y_coordinate_value = float(query_results[0]['coordinateY_value'].toPython())
        
#         ship_coordinates_list.append((x_coordinate_value, y_coordinate_value))
        print(list(transform(sourceCRS, targetCRS, x_coordinate_value, y_coordinate_value)))
        ship_coordinates_list.append(list(transform(sourceCRS, targetCRS, x_coordinate_value, y_coordinate_value)))
#         print(transform(sourceCRS, targetCRS, x_coordinate_value, y_coordinate_value))
        
    
        # query each ship for chimney
        
        query_string_chimney = """
            PREFIX cp:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#>  
                    PREFIX j1:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> 
                    SELECT ?chimney 
                    WHERE
                    {{ 
                        ?entity  j1:hasSubsystem ?chimney . 
                          ?chimney a cp:Pipe . 
                    }}  
        """
        
        query_results = graph.query(query_string_chimney).bindings
        
        chimney_iri = str(query_results[0]['chimney'].toPython())
        
#         chimney_iri_list.append(chimney_iri)
        chimney_iri_list.append(chimney_iri)
        
    # workingDir = str(sys.argv[4]).replace('/','//')
    workingDir = str(sys.argv[4])
    
    pythonLogger.postInfoToLogServer('workingDir=' + workingDir)
    
    print("PARAMETERS")
    print("")
    print(chimney_iri_list,config.bldTopnode, coordinates,  ["CO2"   ,"CO" ,  "NO2" ,  "HC" ,  "NOx"], 2, config.bdnLimit,False, BDN)
    print("")
    test = admsInputDataRetriever(chimney_iri_list,config.bldTopnode, coordinates,  ["CO2"   ,"CO" ,  "NO2" ,  "HC" ,  "NOx"], 2, config.bdnLimit,False, BDN, targetCRS)
    result = test.get()
    
    print("RESULT TYPE: ")
    print(type(result))
    pythonLogger.postInfoToLogServer('calling admsAplWirter ...')
    result['Bdn'] = BDN
    result['CoordiSys'] = sys.argv[5][5:]
#     result['CoordiSys'] = '3857';
    
    for idx in range(len(ship_coordinates_list)):
        result['Src'][idx].setCoordinates(ship_coordinates_list[idx])
    
        result['Src'][idx].SrcName = "Chimney-{0}".format(idx+1)
    
    #result['Src']
       #item['SrcX1']. item['SrcY1'] 
    writer = admsAplWriter(result, workingDir + '/test.apl')
    writer.write()
    
    pythonLogger.postInfoToLogServer('end')

except Exception as e:
    print(e)
    pythonLogger.postErrorToLogServer(e)