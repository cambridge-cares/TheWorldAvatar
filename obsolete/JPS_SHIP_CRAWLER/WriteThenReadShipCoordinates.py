from SPARQLWrapper import SPARQLWrapper, JSON, POST
from rdflib.graph import Graph
from rdflib.plugins.sparql.processor import processUpdate
import time

ship_owl_files_dir = "C:/TOMCAT/webapps/ROOT/kb/ships"

def sparqlQueryRead(queryString):
    # sparql = SPARQLWrapper("http://www.theworldavatar.com/damecoolquestion/ships/sparql")
    sparql = SPARQLWrapper("http://172.25.182.41/damecoolquestion/ships-persistent/sparql")
    sparql.setQuery(queryString)
    sparql.setReturnFormat(JSON)

    return sparql.query().convert()


def sparqlQueryWrite(queryString):
    # sparql = SPARQLWrapper("http://www.theworldavatar.com/damecoolquestion/ships/update")
    sparql = SPARQLWrapper("http://172.25.182.41/damecoolquestion/ships-persistent/update")
    sparql.setQuery(queryString)
    sparql.setMethod(POST)

    return sparql.query()


def read_ship_coordinates_fuseki(ship_iri):


    queryString = """
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

    queryResults = sparqlQueryRead(queryString)
    queryResults = queryResults['results']['bindings']

    x_coordinate_value = queryResults[0]['coordinateX_value']['value']
    y_coordinate_value = queryResults[0]['coordinateY_value']['value']

    return (x_coordinate_value, y_coordinate_value)

def write_ship_coordinates_fuseki(ship_iri, coordinates):

    (x_coordinate, y_coordinate) = coordinates  # (lon, lat)
    queryString = """
        PREFIX j1: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
        PREFIX j4: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>

        DELETE {{
            ?coordinateX_V j1:numericalValue ?coordinateX_value .
            ?coordinateY_V j1:numericalValue ?coordinateY_value .
        }}
        INSERT
        {{
            ?coordinateX_V j1:numericalValue {1}.
            ?coordinateY_V j1:numericalValue {2} .
        }}
        WHERE
        {{
            <{0}> j4:hasGISCoordinateSystem ?coordinateSystem .
                ?coordinateSystem j4:hasProjectedCoordinate_x ?coordinateX .
                    ?coordinateX j1:hasValue ?coordinateX_V .
                        ?coordinateX_V j1:numericalValue ?coordinateX_value .
                ?coordinateSystem j4:hasProjectedCoordinate_y ?coordinateY .
                    ?coordinateY j1:hasValue ?coordinateY_V .
                        ?coordinateY_V j1:numericalValue ?coordinateY_value .

        }}
    """.format(ship_iri, x_coordinate, y_coordinate)

    sparqlQueryWrite(queryString)
	
def read_ship_coordinates_file(ship_iri, graph):

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

def write_ship_coordinates_file(ship_iri, coordinates, graph, file_destination):

    (x_coordinate, y_coordinate) = coordinates  # (lon, lat)
    query_string = """
        PREFIX j1: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
        PREFIX j4: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>

        DELETE {{
            ?coordinateX_V j1:numericalValue ?coordinateX_value .
            ?coordinateY_V j1:numericalValue ?coordinateY_value .
        }}
        INSERT
        {{
            ?coordinateX_V j1:numericalValue {1}.
            ?coordinateY_V j1:numericalValue {2} .
        }}
        WHERE
        {{
            <{0}> j4:hasGISCoordinateSystem ?coordinateSystem .
                ?coordinateSystem j4:hasProjectedCoordinate_x ?coordinateX .
                    ?coordinateX j1:hasValue ?coordinateX_V .
                        ?coordinateX_V j1:numericalValue ?coordinateX_value .
                ?coordinateSystem j4:hasProjectedCoordinate_y ?coordinateY .
                    ?coordinateY j1:hasValue ?coordinateY_V .
                        ?coordinateY_V j1:numericalValue ?coordinateY_value .

        }}
    """.format(ship_iri, x_coordinate, y_coordinate)

    processUpdate(graph, query_string)

    graph.serialize(destination=file_destination, format='xml')

if __name__ == "__main__":

    list_ship_iri = [
        "http://www.theworldavatar.com/kb/ships/Ship-1.owl#Ship-1",
        "http://www.theworldavatar.com/kb/ships/Ship-2.owl#Ship-2",
        "http://www.theworldavatar.com/kb/ships/Ship-3.owl#Ship-3",
        "http://www.theworldavatar.com/kb/ships/Ship-4.owl#Ship-4",
        "http://www.theworldavatar.com/kb/ships/Ship-5.owl#Ship-5",
        "http://www.theworldavatar.com/kb/ships/Ship-6.owl#Ship-6",
        "http://www.theworldavatar.com/kb/ships/Ship-7.owl#Ship-7",
        "http://www.theworldavatar.com/kb/ships/Ship-8.owl#Ship-8",
        "http://www.theworldavatar.com/kb/ships/Ship-9.owl#Ship-9",
        "http://www.theworldavatar.com/kb/ships/Ship-10.owl#Ship-10",
        "http://www.theworldavatar.com/kb/ships/Ship-11.owl#Ship-11",
		"http://www.theworldavatar.com/kb/ships/Ship-12.owl#Ship-12",
		"http://www.theworldavatar.com/kb/ships/Ship-13.owl#Ship-13",
		"http://www.theworldavatar.com/kb/ships/Ship-14.owl#Ship-14",
		"http://www.theworldavatar.com/kb/ships/Ship-15.owl#Ship-15",
		"http://www.theworldavatar.com/kb/ships/Ship-16.owl#Ship-16",
		"http://www.theworldavatar.com/kb/ships/Ship-17.owl#Ship-17",
		"http://www.theworldavatar.com/kb/ships/Ship-18.owl#Ship-18",
		"http://www.theworldavatar.com/kb/ships/Ship-19.owl#Ship-19",
        "http://www.theworldavatar.com/kb/ships/Ship-20.owl#Ship-20"
    ]

    list_ship_coordinates = [
        [
            (103.85453642892246, 1.2680012983987607), (103.85598426063952, 1.2667850769873361),
            (103.85709097876045, 1.2658386712849963), (103.85830314965888, 1.2647187201962558),
            (103.85969201569841, 1.264281592852094), (103.86057176660205, 1.264107517017203),
            (103.86172287585053, 1.2644403633228147), (103.86295689633432, 1.2649439544590626),
            (103.86418930966354, 1.266484132082312), (103.86545520216595, 1.2685980495080127),
            
			(114.14777057386121, 22.290280268826518
),(114.14829589869862, 22.29047561502098
),(114.14860351525628, 22.290729554396183
),(114.14922262160651, 22.2910641699659
),(114.14970330544563, 22.291378204717798
),(114.15037155231036, 22.291970778220204
),(114.15093555172338, 22.29216985321056
),(114.15159526926666, 22.292421565985826
),(114.15236682416118, 22.292582773494058
),(114.15377569781468, 22.292986299347422)
            # (114.15688347119361, 22.28919149789496), (114.15668547748032, 22.288995039690267),
            # (114.15769462617133, 22.288776882257377), (114.15856102252239, 22.288500532946433),
            # (114.15961770049839, 22.288326764888826), (114.16058564284428, 22.28807947546982),
            # (114.1614820401903, 22.287694221142093), (114.1622957006458, 22.2873551265032),
            # (114.15272202398735, 22.288748502591698), (114.1547441780238, 22.288715623749738)
			
        ],
        [
            (103.84872838417058, 1.2536591034578015), (103.85003234235347, 1.2549903441300752),
            (103.85085729548851, 1.2558325576155138), (103.85173949052643, 1.2564514880876037),
            (103.85282411444179, 1.2573480584165009), (103.85391236747867, 1.258459080781991),
            (103.8546290219168, 1.2591907296560083), (103.85555801840974, 1.2601391633806336),
            (103.85651924004269, 1.260421358992379), (103.85867032455997, 1.2621311003394866),
			
			(114.14938219033935, 22.28999432587223
),(114.14988124315754, 22.290353540817247
),(114.15065070486428, 22.290463902498118
),(114.1515590587931, 22.291007277249918
),(114.15249980773416, 22.291401843315537
),(114.15331422357984, 22.291668471274377
),(114.1543605303088, 22.291819994590885
),(114.15546581720987, 22.291933795306065
),(114.15647166931609, 22.292305698834724
),(114.15783380878351, 22.292777067418164)
        ],
        [
            (103.84928476961656, 1.2516525347743972), (103.85019359266438, 1.2524431821136122),
            (103.85137409818398, 1.2536483871460289), (103.85250213678998, 1.254800027508718),
            (103.85410237760047, 1.2564337498809244), (103.85645467011538, 1.2580162921398539),
            (103.8574950404911, 1.258807022398538), (103.85840726680313, 1.259400179002894),
            (103.85948874897933, 1.2603019811633687), (103.8615762164985, 1.2614281235642681),
			
			(114.15114384903337, 22.289209174193687
),(114.1520136849569, 22.289886297567293
),(114.15279376917287, 22.290388365802663
),(114.15349634766731, 22.290745499028542
),(114.15448367422414, 22.29093474494638
),(114.15537350260357, 22.29129546208736
),(114.15679702857153, 22.292054908732233
),(114.15831324006005, 22.291991282828437
),(114.15984944400445, 22.291611250692014
),(114.1610675654523, 22.290723050043596)            
        ],
        [
            (103.84913829344923, 1.2497349648318632), (103.85055617368712, 1.2508463036815205),
            (103.85239502691576, 1.2525224873505383), (103.85360144919267, 1.253620296852198),
            (103.85484525520648, 1.2548901268058936), (103.85666982789732, 1.2560201037255803),
            (103.85825954185553, 1.2574442337100273), (103.85913615664444, 1.2583391897603171),
            (103.86024481652319, 1.2594710459401748), (103.86234047256954, 1.2614782225671393),
			
			(114.15290226077374, 22.289147195366645
),(114.15361289887868, 22.289432757066553
),(114.15462262421981, 22.289631397414524
),(114.15593758474522, 22.28989569983531
),(114.15730537760601, 22.289969696230013
),(114.15840422846286, 22.289793482045415
),(114.15974915282649, 22.289445651756875
),(114.16090892047788, 22.289145083380596
),(114.16197072023293, 22.28860355397435
),(114.16302833361208, 22.287960332913386)            
        ],
        [
            (103.84872987562024, 1.2486467961815098), (103.85002550689234, 1.2499695357244958),
            (103.85111864402134, 1.250818412231522), (103.85212717978116, 1.251382410560033),
            (103.85435382774561, 1.252797056267112), (103.85587955498086, 1.2548160320301511),
            (103.85771468444527, 1.2559656493898022), (103.85965134983579, 1.2570296594281933),
            (103.8618692067876, 1.2589053268088215), (103.86450392568347, 1.2601843269771782),
            
			(114.15375981023712, 22.288470538067088
),(114.15593233677556, 22.28924432037236
),(114.15669939251775, 22.28902888139047
),(114.15772073804816, 22.288707732196375
),(114.158567236167, 22.28855059663363
),(114.1595887382678, 22.28836692485724
),(114.16067728018301, 22.288073958953543
),(114.16147673095283, 22.287709711285473
),(114.16238817569732, 22.28739243576807
),(114.16316631593013, 22.286244797962336)
        ]
    ]


    # start_time = time.time()
    i = 0

    while True:
        print('coordinate index', i)
        ship_index = 0;
		
        for ship_iri in list_ship_iri:
            write_ship_coordinates_fuseki(ship_iri, list_ship_coordinates[i][ship_index])
            
            
			
            ship_name = ship_iri[ship_iri.rfind('#') + 1:]
            file_destination = "{0}/{1}.owl".format(ship_owl_files_dir, ship_name)
            graph = Graph().parse(file_destination)
            write_ship_coordinates_file(ship_iri, list_ship_coordinates[i][ship_index], graph, file_destination)
            
            print('ship index', ship_index)
            print(read_ship_coordinates_fuseki(ship_iri))
            print(read_ship_coordinates_file(ship_iri, graph))
			
            ship_index = ship_index + 1

        if i == len(list_ship_coordinates) - 1:
            i = 0
        else:
            i = i + 1
        print('\n')
        time.sleep(180)     # every 180 seconds

    # print("{} seconds".format(time.time() - start_time))
