from pyproj import Proj, transform
from SPARQLWrapper import SPARQLWrapper, JSON
import rdflib
import json
import sys

owlCRS = Proj(init='epsg:28992')
osmCRS = Proj(init='epsg:4326')

# graph as first parameter
def sparqlBuildingCoordinates(building):
    
    sparql = SPARQLWrapper("http://www.theworldavatar.com/damecoolquestion/buildingsLite/sparql")
    
    queryString = '''

            PREFIX p3: <http://www.theworldavatar.com/CityGMLOntology.owl#>
            PREFIX j1: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#>
            PREFIX j2: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>

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
    
    sparql.setQuery(queryString)
    sparql.setReturnFormat(JSON)

#     return graph.query(queryString)

    return sparql.query().convert()

# graph as first parameter
def sparqlBuildingHeights(building):
    
    sparql = SPARQLWrapper("http://www.theworldavatar.com/damecoolquestion/buildingsLite/sparql")
    
    queryString = """
        PREFIX p3: <http://www.theworldavatar.com/CityGMLOntology.owl#>
        PREFIX j2: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>

        SELECT ?numericalValue
        WHERE
        {{
            <{0}> p3:measuredHeight ?lengthType .
            <{0}> a p3:BuildingType .
            ?lengthType j2:hasValue ?scalarValue .
            ?scalarValue j2:numericalValue ?numericalValue
        }}

        """.format(building)
        
    sparql.setQuery(queryString)
    sparql.setReturnFormat(JSON)

#     return graph.query(queryString)

    return sparql.query().convert()

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
    return float(buildingHeight[0]['numericalValue']['value'])

def getBuildingCoordinates(buildingCoordinates):
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

    # todo: have to consider the scenario in which there is only one pair of coordinates
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

# stores each building's data in a python dictionary in GeoJSON format
def getGeoJSON(listBuildingCoordinates, listBuildingHeights):
    listBuildingsToTransfer = []

    for idx, building in enumerate(listBuildingCoordinates):
        python_data = {
            'type': 'FeatureCollection',
            'features': [{
                'type': 'Feature',
                'properties': {
                    'height': listBuildingHeights[idx],
                    'minHeight': 0,
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
    
#     graph = rdflib.Graph()
    # todo: switch to variable path
#     graph.parse(r'C:\Users\WE\Dropbox (Cambridge CARES)\IRP3 CAPRICORN shared folder\WENG\107_buildings.owl')

    try:
        listOfIRIs = json.loads(sys.argv[1])
        

        # --Obtain list of building heights-- #
        # --Obtain list of building coordinates-- #
        
        listBuildingHeights = []
        listBuildingCoordinates = []

        for building in listOfIRIs:
#             buildingHeight = sparqlBuildingHeights(graph, building).bindings
            buildingHeight = sparqlBuildingHeights(building)["results"]["bindings"]
            
            height = getBuildingHeights(buildingHeight)
            listBuildingHeights.append(height)

#             buildingCoordinates = sparqlBuildingCoordinates(graph, building).bindings
            buildingCoordinates = sparqlBuildingCoordinates(building)["results"]["bindings"]
            
            coordinates = getBuildingCoordinates(buildingCoordinates)
            listBuildingCoordinates.append(coordinates)

        # --Write building coordinates into a text file-- #
        # writeFile(buildingCoordinates)

    except:
        print("INVALID QUERY")

    listBuildingsToTransfer = getGeoJSON(listBuildingCoordinates, listBuildingHeights)

    # writeToJSONFile(listBuildingsToTransfer, 'buildingData.json')
    # listBuildingsToTransfer = readFromJSONFile('buildingData.json')

    return json.dumps(listBuildingsToTransfer)

if __name__ == "__main__":
    print(return_buildings())