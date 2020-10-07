from pyproj import Proj, transform
from SPARQLWrapper import SPARQLWrapper, JSON
import rdflib
import json
import sys

import os
# caresjpsutilPath = os.path.abspath(os.path.join(os.getcwd(), '../caresjpsutil'))
# sys.path.insert(0, caresjpsutilPath)
from caresjpsutil import returnExceptionToJava, returnResultsToJava
from caresjpsutil import PythonLogger

def sparqlQuery(queryString, sparqlEndPoint):
    sparql = SPARQLWrapper(sparqlEndPoint)
    sparql.setQuery(queryString)
    sparql.setReturnFormat(JSON)

    return sparql.query().convert()

def sparqlBuildingCoordinates(building, sparqlEndPoint):
    
    queryString = '''

            PREFIX p3: <http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#>
            PREFIX j1: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
            PREFIX j2: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>

            SELECT ?polygon ?coordinates ?xval ?yval ?zval
            WHERE
            {{
                {{
                    <{0}> p3:id ?buildingID .
                    <{0}> p3:boundedBy ?groundSurface .
                     ?groundSurface a p3:GroundSurfaceType .
                     ?groundSurface p3:lod2MultiSurface ?multiSurface .
                      ?multiSurface p3:surfaceMember ?polygon .
                       ?polygon p3:exterior ?linearRing .
                        ?linearRing j2:contains ?points .
                         ?points j1:hasGISCoordinateSystem ?coordinates .
                          ?coordinates j1:hasProjectedCoordinate_x ?x .
                           ?x j2:hasValue ?xv .
                            ?xv j2:numericalValue ?xval .
                          ?coordinates j1:hasProjectedCoordinate_y ?y .
                           ?y j2:hasValue ?yv .
                            ?yv j2:numericalValue ?yval.
                          ?coordinates j1:hasProjectedCoordinate_z ?z .
                           ?z j2:hasValue ?zv .
                            ?zv j2:numericalValue ?zval
                }}
                UNION
                {{
                    <{0}> p3:consistsOfBuildingPart ?buildingPart .
                     ?buildingPart p3:boundedBy ?groundSurface .
                     ?groundSurface a p3:GroundSurfaceType .
                     ?groundSurface p3:lod2MultiSurface ?multiSurfaceType .
                      ?multiSurfaceType p3:surfaceMember ?polygon .
                       ?polygon p3:exterior ?linearRing .
                        ?linearRing j2:contains ?points .
                         ?points j1:hasGISCoordinateSystem ?coordinates .
                          ?coordinates j1:hasProjectedCoordinate_x ?x .
                           ?x j2:hasValue ?xv .
                            ?xv j2:numericalValue ?xval .
                          ?coordinates j1:hasProjectedCoordinate_y ?y .
                           ?y j2:hasValue ?yv .
                            ?yv j2:numericalValue ?yval .
                          ?coordinates j1:hasProjectedCoordinate_z ?z .
                           ?z j2:hasValue ?zv .
                            ?zv j2:numericalValue ?zval

                }}
            }}
            ORDER BY ?polygon ?points
    '''.format(building)
    with open('query.txt', 'w') as file:
        file.write(queryString)
    return sparqlQuery(queryString, sparqlEndPoint)


# graph as first parameter
def sparqlBuildingHeights(building, sparqlEndPoint):
    
    queryString = """
        PREFIX p3: <http://www.theworldavatar.com/ontology/ontocitygml/OntoCityGML.owl#>
        PREFIX j2: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>

        SELECT ?height ?minHeight
        WHERE
        {{
            <{0}> p3:measuredHeight ?lengthType .
            <{0}> a p3:BuildingType .
            ?lengthType j2:hasValue ?scalarValue .
            ?scalarValue j2:numericalValue ?height .
    
            OPTIONAL {{
                <{0}> p3:lowerHeight ?minHeightType .
                <{0}> a p3:BuildingType .
                ?minHeightType j2:hasValue ?scalarValueMinHeight .
                ?scalarValueMinHeight j2:numericalValue ?minHeight .    
            }}
        }}

        """.format(building)
        
    return sparqlQuery(queryString, sparqlEndPoint)

def writeFile(buildingCoordinates):
    f = open("output.txt", "w")
    for res in buildingCoordinates:
        f.write(res['building'].toPython() + '\n')
        f.write(res['polygon'].toPython() + '\n')
        f.write(res['coordinates'].toPython() + '\n')
        f.write(res['xval'].toPython() + '\n')
        f.write(res['yval'].toPython() + '\n')
        f.write(res['zval'].toPython() + '\n')
        f.write('\n')
    f.close()

def writeToJSONFile(listBuildingsToTransfer, fileName):
    listBuildingsJSON = {'jsonObj': listBuildingsToTransfer}
    with open(fileName, 'w') as outfile:
        json.dump(listBuildingsJSON, outfile)

def readFromJSONFile(fileName):
    infile = open(fileName, 'r')
    listBuildingsToTransfer = json.load(infile)['jsonObj']
    infile.close()
    return listBuildingsToTransfer

def getBuildingHeights(buildingHeight):
    heights = {}
    if 'minHeight' in buildingHeight[0]:
        heights['minHeight'] = float(buildingHeight[0]['minHeight']['value'])
    else:
        heights['minHeight'] = 0
    heights['height'] = float(buildingHeight[0]['height']['value'])
    return heights

def getBuildingCoordinates(buildingCoordinates, owlCRS, osmCRS):
    building = []
    polygon = []

    firstEntry = buildingCoordinates[0]

    # add first point to polygon
    # convert from citygml CRS to geojson CRS
    point = list(transform(owlCRS, osmCRS,
                           float(firstEntry['xval']['value']),
                           float(firstEntry['yval']['value'])
                           ))
    polygon.append(point)

    # use prevPolygon to compare with current polygon in for-loop
    prevPolygon = firstEntry['polygon']['value']

    # assume that each building in the owl files have the appropriate pairs of coordinates
    for entry in buildingCoordinates[1:]:

        if entry['polygon']['value'] != prevPolygon:
            building.append([polygon])
            polygon = []

        point = list(transform(owlCRS, osmCRS,
                               float(entry['xval']['value']),
                               float(entry['yval']['value'])
                               ))
        polygon.append(point)
        prevPolygon = entry['polygon']['value']

    building.append([polygon])
    return building

# stores each building's data in a Python dictionary in GeoJSON format
def getGeoJSON(listBuildingCoordinates, listBuildingHeights, cityiri):
    listBuildingsToTransfer = []
    if cityiri == "http://dbpedia.org/resource/Hong_Kong":
        for idx, building in enumerate(listBuildingCoordinates):
            python_data = {
                'type': 'FeatureCollection',
                'features': [{
                    'type': 'Feature',
                    'properties': {
                        'height': float(listBuildingHeights[idx]['height']),
                        #'minHeight': listBuildingHeights[idx]['minHeight'],
                        'color': 'red',
                        'roofColor': 'red'
                    },
                    'geometry': {
                        'type': 'MultiPolygon',
                        'coordinates': building
    
                    }
                }]
            }
    
            listBuildingsToTransfer.append(python_data)
    else:
        for idx, building in enumerate(listBuildingCoordinates):
            python_data = {
                'type': 'FeatureCollection',
                'features': [{
                    'type': 'Feature',
                    'properties': {
                        'height': listBuildingHeights[idx]['height'],
                        'minHeight': listBuildingHeights[idx]['minHeight'],
                        'color': 'red',
                        'roofColor': 'red'
                    },
                    'geometry': {
                        'type': 'MultiPolygon',
                        'coordinates': building
    
                    }
                }]
            }
    
            listBuildingsToTransfer.append(python_data)
        

    return listBuildingsToTransfer

def return_buildings():
    with open('./gson_log.txt','w') as f:
        f.write(str(sys.argv[1]))    
    listOfIRIs = sys.argv[1].strip().replace('"','').replace("'",'')[1:-1].split(',')
    cityiri = sys.argv[2]
    with open('./log.txt','w') as file:
        file.write(str(listOfIRIs))
        file.write(cityiri)
    sparqlEndPoint = None
    owlCRS = None
    osmCRS = Proj(init='epsg:4326')
    
    if cityiri == "http://dbpedia.org/resource/The_Hague":
        owlCRS = Proj(init='epsg:28992')
        sparqlEndPoint = "http://www.theworldavatar.com/damecoolquestion/thehaguebuildings/sparql"
    elif cityiri == "http://dbpedia.org/resource/Berlin":
        owlCRS = Proj(init='epsg:25833')
        sparqlEndPoint = "http://www.theworldavatar.com/damecoolquestion/berlinbuildings/sparql"
    elif cityiri == "http://dbpedia.org/resource/Singapore":
        owlCRS = Proj(init='epsg:4326')
        sparqlEndPoint = "http://www.theworldavatar.com/damecoolquestion/mbs/sparql"
    elif cityiri == "http://dbpedia.org/resource/Hong_Kong":
        owlCRS = Proj(init='epsg:4326')
        sparqlEndPoint = "http://www.theworldavatar.com/damecoolquestion/hongkongbuildingsrealdata/sparql"
        #sparqlEndPoint = "http://www.theworldavatar.com/damecoolquestion/hongkongbuildings/sparql"

    if listOfIRIs == []:
        raise ValueError("EMPTY ARRAY")

    # --Obtain list of building heights-- #
    # --Obtain list of building coordinates-- #

    listBuildingHeights = []
    listBuildingCoordinates = []

    for building in listOfIRIs:

        buildingHeight = sparqlBuildingHeights(building, sparqlEndPoint)["results"]["bindings"]
        height = getBuildingHeights(buildingHeight)
        listBuildingHeights.append(height)

        buildingCoordinates = sparqlBuildingCoordinates(building, sparqlEndPoint)["results"]["bindings"]
        coordinates = getBuildingCoordinates(buildingCoordinates, owlCRS, osmCRS)
        listBuildingCoordinates.append(coordinates)


    listBuildingsToTransfer = getGeoJSON(listBuildingCoordinates, listBuildingHeights, cityiri)

    return json.dumps(listBuildingsToTransfer)

if __name__ == "__main__":
    pythonLogger = PythonLogger('ADMSGeoJsonGetter.py')
    pythonLogger.postInfoToLogServer('start of ADMSGeoJsonGetter.py')
    try:
        returnResultsToJava(return_buildings())
        pythonLogger.postInfoToLogServer('end of ADMSGeoJsonGetter.py')
    except Exception as e:
        returnExceptionToJava(e)
        pythonLogger.postInfoToLogServer('end of ADMSGeoJsonGetter.py')