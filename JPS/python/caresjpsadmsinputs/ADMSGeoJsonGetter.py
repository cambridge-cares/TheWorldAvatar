from pyproj import Proj, transform
import rdflib
import json
import sys

owlCRS = Proj(init='epsg:28992')
osmCRS = Proj(init='epsg:4326')

def sparqlBuildingCoordinates(graph, building):
    queryString = '''

            PREFIX j0: <file:/D:/citygmllearn/citygmlhandmade.owl#>
            PREFIX j2: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#>
            PREFIX j3: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>

            SELECT ?polygon ?coordinates ?xval ?yval ?zval
            WHERE
            {{
                {{
                    <{0}> j0:id ?buildingID .
                    <{0}> j0:boundedBy ?groundSurface .
                     ?groundSurface a j0:GroundSurfaceType .
                     ?groundSurface j0:lod2MultiSurface ?multiSurface .
                      ?multiSurface j0:surfaceMember ?polygon .
                       ?polygon j0:exterior ?linearRing .
                        ?linearRing j3:contains ?points .
                         ?points j2:hasGISCoordinateSystem ?coordinates .
                          ?coordinates j2:hasProjectedCoordinate_x ?x .
                           ?x j3:hasValue ?xv .
                            ?xv j3:numericalValue ?xval .
                          ?coordinates j2:hasProjectedCoordinate_y ?y .
                           ?y j3:hasValue ?yv .
                            ?yv j3:numericalValue ?yval.
                          ?coordinates j2:hasProjectedCoordinate_z ?z .
                           ?z j3:hasValue ?zv .
                            ?zv j3:numericalValue ?zval
                }}
                UNION
                {{
                    <{0}> j0:consistsOfBuildingPart ?buildingPart .
                     ?buildingPart j0:boundedBy ?groundSurface .
                     ?groundSurface a j0:GroundSurfaceType .
                     ?groundSurface j0:lod2MultiSurface ?multiSurfaceType .
                      ?multiSurfaceType j0:surfaceMember ?polygon .
                       ?polygon j0:exterior ?linearRing .
                        ?linearRing j3:contains ?points .
                         ?points j2:hasGISCoordinateSystem ?coordinates .
                          ?coordinates j2:hasProjectedCoordinate_x ?x .
                           ?x j3:hasValue ?xv .
                            ?xv j3:numericalValue ?xval .
                          ?coordinates j2:hasProjectedCoordinate_y ?y .
                           ?y j3:hasValue ?yv .
                            ?yv j3:numericalValue ?yval .
                          ?coordinates j2:hasProjectedCoordinate_z ?z .
                           ?z j3:hasValue ?zv .
                            ?zv j3:numericalValue ?zval

                }}
            }}
            ORDER BY ?polygon ?points
    '''.format(building)

    return graph.query(queryString)

def sparqlBuildingHeights(graph, building):
    queryString = """
        PREFIX j0: <file:/D:/citygmllearn/citygmlhandmade.owl#>
        PREFIX j3: <http://www.theworldavatar.com/OntoCAPE/OntoCAPE/upper_level/system.owl#>

        SELECT ?numericalValue
        WHERE
        {{
            <{0}> j0:measuredHeight ?lengthType .
            <{0}> a j0:BuildingType .
            ?lengthType j3:hasValue ?scalarValue .
            ?scalarValue j3:numericalValue ?numericalValue
        }}

        """.format(building)

    return graph.query(queryString)

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
    return float(buildingHeight[0]['numericalValue'].toPython())

def getBuildingCoordinates(buildingCoordinates):
    building = []
    polygon = []

    firstEntry = buildingCoordinates[0]

    # add first point to polygon
    # convert from citygml CRS to geojson CRS
    point = list(transform(owlCRS, osmCRS,
                           float(firstEntry['xval'].toPython()),
                           float(firstEntry['yval'].toPython())
                           ))
    polygon.append(point)

    # use prevPolygon to compare with current polygon in for-loop
    prevPolygon = firstEntry['polygon'].toPython()

    # todo: have to consider the scenario in which there is only one pair of coordinates
    for entry in buildingCoordinates[1:]:

        if entry['polygon'].toPython() != prevPolygon:
            building.append([polygon])
            polygon = []

        point = list(transform(owlCRS, osmCRS,
                               float(entry['xval'].toPython()),
                               float(entry['yval'].toPython())
                               ))
        polygon.append(point)
        prevPolygon = entry['polygon'].toPython()

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
    
    graph = rdflib.Graph()
    graph.parse(r'C:\Users\WE\Dropbox (Cambridge CARES)\IRP3 CAPRICORN shared folder\WENG\107_buildings.owl')

    try:
        listOfIRIs = json.loads(sys.argv[1])
        

        # --Obtain list of building heights-- #
        # --Obtain list of building coordinates-- #
        
        listBuildingHeights = []
        listBuildingCoordinates = []

        for building in listOfIRIs:
            buildingHeight = sparqlBuildingHeights(graph, building).bindings
            height = getBuildingHeights(buildingHeight)
            listBuildingHeights.append(height)

            buildingCoordinates = sparqlBuildingCoordinates(graph, building).bindings
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