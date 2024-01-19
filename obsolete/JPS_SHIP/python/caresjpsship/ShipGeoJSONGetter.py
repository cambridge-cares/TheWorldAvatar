from rdflib.graph import Graph
import json
import sys

def read_ship_coordinates(ship_iri, graph):

    query_string = """
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
    """.format(ship_iri)

    query_results = graph.query(query_string).bindings

    x_coordinate_value = float(query_results[0]['coordinateX_value'].toPython())
    y_coordinate_value = float(query_results[0]['coordinateY_value'].toPython())

    return (x_coordinate_value, y_coordinate_value)

def get_geo_json(x_coordinate, y_coordinate):

    coordinates = []

    coordinates.append([x_coordinate + 0.0001, y_coordinate])
    coordinates.append([x_coordinate, y_coordinate - 0.0001])
    coordinates.append([x_coordinate - 0.0001, y_coordinate])
    coordinates.append([x_coordinate, y_coordinate + 0.0001])
    coordinates.append([x_coordinate + 0.0001, y_coordinate])

    python_dict = {
        'type': 'FeatureCollection',
        'features': [
            {
                'type': 'Feature',
                'properties': {
                    'height': 5,
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

    return json.dumps(python_dict)

if __name__ == "__main__":
    list_ship_iri = json.loads(sys.argv[1])

    # list_ship_iri = [
    #     "http://www.theworldavatar.com/kb/sgp/Ship-1.owl#Ship-1",
    #     "http://www.theworldavatar.com/kb/sgp/Ship-2.owl#Ship-2",
    #     "http://www.theworldavatar.com/kb/sgp/Ship-3.owl#Ship-3",
    #     "http://www.theworldavatar.com/kb/sgp/Ship-4.owl#Ship-4",
    #     "http://www.theworldavatar.com/kb/sgp/Ship-5.owl#Ship-5",
    #     "http://www.theworldavatar.com/kb/sgp/Ship-6.owl#Ship-6",
    #     "http://www.theworldavatar.com/kb/sgp/Ship-7.owl#Ship-7",
    #     "http://www.theworldavatar.com/kb/sgp/Ship-8.owl#Ship-8",
    #     "http://www.theworldavatar.com/kb/sgp/Ship-9.owl#Ship-9",
    #     "http://www.theworldavatar.com/kb/sgp/Ship-10.owl#Ship-10",
    # ]

    list_geojson_dict = []

    for ship_iri in list_ship_iri:

        ship_name = ship_iri[ship_iri.rfind('#') + 1:]
        file_destination = "C:/JPS_DATA/workingdir/JPS/SHIP/output/{}.owl".format(ship_name)
        graph = Graph().parse(file_destination)

        # graph = Graph().parse(ship_iri)
        coordinates = read_ship_coordinates(ship_iri, graph)
        x_coordinate = coordinates[0]
        y_coordinate = coordinates[1]
        geojson_dict = get_geo_json(x_coordinate, y_coordinate)
        list_geojson_dict.append(geojson_dict)

    print(json.dumps(list_geojson_dict))