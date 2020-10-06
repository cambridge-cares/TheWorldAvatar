from pyproj import Proj, transform
import rdflib
import json
import sys


def sparqlCentrePointAndHeight(graph, powerPlantIRI):
    queryString = """
        PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
        PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
        PREFIX plant: <http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#>

        SELECT ?projectedCoordinateSystem ?value_x ?value_y ?height
        WHERE
        {{
            <{0}> space_and_time_extended:hasGISCoordinateSystem ?projectedCoordinateSystem .
                ?projectedCoordinateSystem space_and_time_extended:hasProjectedCoordinate_x ?coordinate_x .
                    ?coordinate_x system:hasValue ?coordinate_x_value .
                        ?coordinate_x_value system:numericalValue ?value_x .
                ?projectedCoordinateSystem space_and_time_extended:hasProjectedCoordinate_y ?coordinate_y .
                    ?coordinate_y system:hasValue ?coordinate_y_value .
                        ?coordinate_y_value system:numericalValue ?value_y .
            <{0}> plant:hasHeight ?chimneyHeight .
                ?chimneyHeight system:numericalValue ?height
        }}

    """.format(powerPlantIRI)

    return graph.query(queryString)

def getPowerPlantCentrePoint(powerPlantCentrePointAndHeight):
#     print(powerPlantCentrePoint[0]['projectedCoordinateSystem'])
    valueX = float(powerPlantCentrePointAndHeight[0]['value_x'])
    valueY = float(powerPlantCentrePointAndHeight[0]['value_y'])
    return (valueX, valueY)
#     return list(transform(owlCRS, osmCRS, valueX, valueY))  # returns [ longitude, latitude ]

def getPowerPlantHeight(powerPlantCentrePointAndHeight):
    return float(powerPlantCentrePointAndHeight[0]['height'])

# stores each building's data in a python dictionary in GeoJSON format
def getGeoJSON(centrePoint, height, epsg):
    owlCRS = Proj(init=epsg)
    osmCRS = Proj(init='epsg:4326')

    coordinates = []
     
    coordinates.append(list(transform(owlCRS, osmCRS, centrePoint[0] + 5, centrePoint[1])))
    coordinates.append(list(transform(owlCRS, osmCRS, centrePoint[0], centrePoint[1] - 5)))
    coordinates.append(list(transform(owlCRS, osmCRS, centrePoint[0] - 5, centrePoint[1])))
    coordinates.append(list(transform(owlCRS, osmCRS, centrePoint[0], centrePoint[1] + 5)))
    coordinates.append(list(transform(owlCRS, osmCRS, centrePoint[0] + 5, centrePoint[1])))
     
    python_data = { 
        'type': 'FeatureCollection',
        'features': [
            {
                'type': 'Feature',
                'properties': {
                    'height': height,
                    'minHeight': 0,
                    'color': 'black',
                    'roofColor': 'black'
                },
                'geometry': {
                    'type': 'Polygon',
                    'coordinates': [coordinates]
   
                }
            }
        ]
    }

    return json.dumps(python_data)


def returnPowerPlantCentrePoint(powerPlantIRI, epsg):
    graph = rdflib.Graph()
    graph.parse(powerPlantIRI)

    try:

        powerPlantCentrePointAndHeight = sparqlCentrePointAndHeight(graph, powerPlantIRI).bindings
        centrePoint = getPowerPlantCentrePoint(powerPlantCentrePointAndHeight)
        height = getPowerPlantHeight(powerPlantCentrePointAndHeight)
        return getGeoJSON(centrePoint, height, epsg)

    except:
        print("INVALID QUERY")

if __name__ == "__main__":
    powerPlantIRI = sys.argv[1]
    epsg = sys.argv[2]
    print(returnPowerPlantCentrePoint(powerPlantIRI, epsg))
