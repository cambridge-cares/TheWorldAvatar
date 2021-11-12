from SPARQLWrapper import SPARQLWrapper, JSON, POST
from rdflib.graph import Graph
from rdflib.plugins.sparql.processor import processUpdate
import time
import sys

ship_owl_files_dir = "C:\\TOMCAT\\webapps\\ROOT\\kb\\ships"

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
    print(file_destination)

    graph.serialize(file_destination, format='xml')

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
        "http://www.theworldavatar.com/kb/ships/Ship-20.owl#Ship-20",
        "http://www.theworldavatar.com/kb/ships/Ship-21.owl#Ship-21",
        "http://www.theworldavatar.com/kb/ships/Ship-22.owl#Ship-22",
        "http://www.theworldavatar.com/kb/ships/Ship-23.owl#Ship-23",
        "http://www.theworldavatar.com/kb/ships/Ship-24.owl#Ship-24",
        "http://www.theworldavatar.com/kb/ships/Ship-25.owl#Ship-25",
        "http://www.theworldavatar.com/kb/ships/Ship-26.owl#Ship-26",
        "http://www.theworldavatar.com/kb/ships/Ship-27.owl#Ship-27",
        "http://www.theworldavatar.com/kb/ships/Ship-28.owl#Ship-28",
        "http://www.theworldavatar.com/kb/ships/Ship-29.owl#Ship-29",
        "http://www.theworldavatar.com/kb/ships/Ship-30.owl#Ship-30",
        "http://www.theworldavatar.com/kb/ships/Ship-31.owl#Ship-31",
        "http://www.theworldavatar.com/kb/ships/Ship-32.owl#Ship-32",
        "http://www.theworldavatar.com/kb/ships/Ship-33.owl#Ship-33",
        "http://www.theworldavatar.com/kb/ships/Ship-34.owl#Ship-34",
        "http://www.theworldavatar.com/kb/ships/Ship-35.owl#Ship-35",
        "http://www.theworldavatar.com/kb/ships/Ship-36.owl#Ship-36",
        "http://www.theworldavatar.com/kb/ships/Ship-37.owl#Ship-37",
        "http://www.theworldavatar.com/kb/ships/Ship-38.owl#Ship-38",
        "http://www.theworldavatar.com/kb/ships/Ship-39.owl#Ship-39",
        "http://www.theworldavatar.com/kb/ships/Ship-40.owl#Ship-40"
    ]

    list_ship_coordinates = [
        [            
            (103.85449852032396, 1.2698855281511163), (103.85534494337327, 1.2697736136819568),
            (103.85640476343727, 1.2694670957482006), (103.85469592023719, 1.2685073233535593),
            (103.85564159682964, 1.2681318958130792), (103.85693301270967, 1.2680370687989955),
            (103.85671298530288, 1.2656042645927341), (103.85868208435656, 1.2641097303498963),
            (103.85940498457884, 1.2628930483535552), (103.86081605450333, 1.262163695703792),
            
            (103.86319046520437, 1.2647591169579984), (103.8637529230258, 1.2673986499844414),
            (103.86665669017232, 1.2694152229443272), (103.86842398591293, 1.2708535073963236),
            (103.8701626089698, 1.272478125147085), (103.8729152877786, 1.2744274425334177),
            (103.87539942590516, 1.2761667230332483), (103.87798888805737, 1.2786044766759599),
            (103.87961998538866, 1.2803079282324685), (103.88035998988545, 1.281126026223941),
            
            (103.88015595958008, 1.279876823350619), (103.87794696723554, 1.2760156588913745),
            (103.87575342472901, 1.2741523385668412), (103.87308676397144, 1.2716440212842248),
            (103.8713933579632, 1.270202428765887), (103.86904321654605, 1.2689752881102039),
            (103.86837268573561, 1.2672288316912699), (103.86619237872098, 1.2656609981916789),
            (103.86455962695321, 1.2639206107820578), (103.8614998081595, 1.2617259746892604),
            
            (114.14777057386121, 22.290280268826518), (114.14829589869862, 22.29047561502098),
            (114.14860351525628, 22.290729554396183), (114.14922262160651, 22.2910641699659),
            (114.14970330544563, 22.291378204717798), (114.15037155231036, 22.291970778220204),
            (114.15093555172338, 22.29216985321056), (114.15159526926666, 22.292421565985826),
            (114.15236682416118, 22.292582773494058), (114.15377569781468, 22.292986299347422)
            
            # (114.15688347119361, 22.28919149789496), (114.15668547748032, 22.288995039690267),
            # (114.15769462617133, 22.288776882257377), (114.15856102252239, 22.288500532946433),
            # (114.15961770049839, 22.288326764888826), (114.16058564284428, 22.28807947546982),
            # (114.1614820401903, 22.287694221142093), (114.1622957006458, 22.2873551265032),
            # (114.15272202398735, 22.288748502591698), (114.1547441780238, 22.288715623749738)
            
        ],
        [          
            (103.88006355279514, 1.2819461138952182), (103.87944100064286, 1.2812707972167048),
            (103.87893424639861, 1.2807013259980384), (103.87824111338695, 1.2801031890921017),
            (103.87758492524148, 1.2795033981529949), (103.87685153849064, 1.278833043573983),
            (103.87612476948577, 1.2783104324074526), (103.87504895232033, 1.2774333490082799),
            (103.87390751760496, 1.2767442529410322), (103.87241207141956, 1.2755898770817835),
            
            (103.87072528315727, 1.2742960279759168), (103.86995164266729, 1.2735534557238506),
            (103.86884549838162, 1.2728257697703196), (103.86811211163078, 1.2721554151913077),
            (103.86695909586, 1.2712077681522407), (103.86609336683625, 1.270062209582215),
            (103.86465802762788, 1.2694232816334445), (103.86321441623706, 1.2685996744190904),
            (103.86204596327444, 1.2681338475037198), (103.86149895529103, 1.2674921586119885),
            
            (103.86212756527964, 1.2666497972463264), (103.86285873768436, 1.265617794515569),
            (103.86239774775218, 1.264417105727554), (103.86115540505719, 1.2639545868785247),
            (103.8591675472925, 1.2633774052804363), (103.858045965815, 1.263131539450602),
            (103.85652571572845, 1.2639027900529893), (103.85542565457148, 1.2666167545720888),
            (103.85495531057616, 1.2693395363804107), (103.856451304025, 1.2696796694378976),
            
            (114.14938219033935, 22.28999432587223), (114.14988124315754, 22.290353540817247),
            (114.15065070486428, 22.290463902498118), (114.1515590587931, 22.291007277249918),
            (114.15249980773416, 22.291401843315537), (114.15331422357984, 22.291668471274377),
            (114.1543605303088, 22.291819994590885), (114.15546581720987, 22.291933795306065),
            (114.15647166931609, 22.292305698834724), (114.15783380878351, 22.292777067418164)
        ],
        [
            (103.85450679249378, 1.2700702071349093), (103.85479793537725, 1.2691319245084243),
            (103.85459886838134, 1.2679935291944608), (103.85496059212421, 1.2669780667953991),
            (103.85435513160026, 1.265857865846302), (103.85492694348483, 1.264573928276408),
            (103.85403474347726, 1.2628373960564976), (103.85461648198076, 1.26177507360531),
            (103.85426136333767, 1.2604586151584245), (103.85566912438917, 1.2596553908024362),
            
            (103.85669145232353, 1.2601647697036018), (103.85709729858816, 1.2609608181404868),
            (103.85861700141126, 1.2610038103398609), (103.85899582942862, 1.2620231279286336),
            (103.86016372248164, 1.2608235333873579), (103.86056902148282, 1.2624338246260045),
            (103.86182734863308, 1.2616002805495756), (103.86220893825998, 1.2635077126463352),
            (103.86401978396657, 1.2626124222192348), (103.8638516166467, 1.265469715174663),
            
            (103.86564481081534, 1.2633538875615), (103.86670905957153, 1.2664520842472615),
            (103.86808207745678, 1.2648732069757946), (103.86866384125263, 1.2687702130414023),
            (103.8695819396882, 1.2669526331961531), (103.8702613025907, 1.2705491903373065),
            (103.87196186096216, 1.2688446192079563), (103.87361171700444, 1.272619786605391),
            (103.87516945922061, 1.2710326391680713), (103.87600542103374, 1.2739930166403748),
            
            #(114.15114384903337, 22.289209174193687), (114.1520136849569, 22.289886297567293),
            #(114.15279376917287, 22.290388365802663), (114.15349634766731, 22.290745499028542),
            #(114.15448367422414, 22.29093474494638), (114.15537350260357, 22.29129546208736),
            #(114.15679702857153, 22.292054908732233), (114.15831324006005, 22.291991282828437),
            #(114.15984944400445, 22.291611250692014), (114.1610675654523, 22.290723050043596)   
            (114.1510100742275, 22.29298499127046), (114.1548295512543, 22.28930895811859),
            (114.15302024082543, 22.29393372231469), (114.14735881280731, 22.292491479678617),
            (114.15776294282321, 22.293863497311115), (114.15754416062774, 22.294772387137),
            (114.15449845853122, 22.28940028992637), (114.15811867680121, 22.28901248913465),
            (114.15551984441231, 22.290277273683614), (114.15185630475926, 22.295422204932247)       
        ],
        [
            (103.85390884051397, 1.2707705582537185), (103.85411120373665, 1.2695031610155305),
            (103.85391213674076, 1.2683647657015567), (103.85407093735131, 1.2669512790840576),
            (103.85425344733615, 1.2652406516084458), (103.85412330676316, 1.2639881406687827),
            (103.85420821764133, 1.2625779621176247), (103.85425618365332, 1.2611694375996376),
            (103.85424680429783, 1.2601335796791693), (103.85420544338561, 1.2592101833512301),
            
            (103.8556611830953, 1.2594780987356522), (103.85676952908088, 1.2594284777405444),
            (103.85776704046792, 1.259383818844938), (103.85906507409392, 1.2594367352433358),
            (103.86073090194529, 1.259436175456915), (103.86295752053542, 1.2595585485853957),
            (103.86428257240867, 1.2593881958319164), (103.8661430512099, 1.259600980998349),
            (103.86868728366731, 1.25937604515025), (103.87081133984132, 1.259688059643837),
            
            (103.87287363460182, 1.2594476903738328), (103.87512892587569, 1.259383730203559),
            (103.87683500746621, 1.2594553880902033), (103.8793957842885, 1.2595998107732715),
            (103.88021244012721, 1.2613027152064105), (103.880205287764, 1.2644488788540986),
            (103.88030069781689, 1.267405401136691), (103.87989320976197, 1.2690520811050912),
            (103.88008952779451, 1.2717820261694652), (103.8799130882922, 1.2744546398593097),
            
            #(114.15290226077374, 22.289147195366645), (114.15361289887868, 22.289432757066553),
            #(114.15462262421981, 22.289631397414524), (114.15593758474522, 22.28989569983531),
            #(114.15730537760601, 22.289969696230013), (114.15840422846286, 22.289793482045415),
            #(114.15974915282649, 22.289445651756875), (114.16090892047788, 22.289145083380596),
            #(114.16197072023293, 22.28860355397435), (114.16302833361208, 22.287960332913386)     
            (114.15290226077374, 22.289147195366645), (114.15361289887868, 22.289432757066553),
            (114.15462262421981, 22.289631397414524), (114.15593758474522, 22.28989569983531),
            (114.15730537760601, 22.289969696230013), (114.15840422846286, 22.289793482045415),
            (114.15377462290056, 22.29534851677131), (114.15821914977435, 22.292815794521996),
            (114.14935166467032, 22.295003371183927), (114.14955551321859, 22.291588122186866)      
        ],
        [
            (103.85449852032396, 1.2698855281511163), (103.85534494337327, 1.2697736136819568),
            (103.85640476343727, 1.2694670957482006), (103.85469592023719, 1.2685073233535593),
            (103.85564159682964, 1.2681318958130792), (103.85693301270967, 1.2680370687989955),
            (103.85671298530288, 1.2656042645927341), (103.85868208435656, 1.2641097303498963),
            (103.85940498457884, 1.2628930483535552), (103.86081605450333, 1.262163695703792),
            
            (103.86319046520437, 1.2647591169579984), (103.8637529230258, 1.2673986499844414),
            (103.86665669017232, 1.2694152229443272), (103.86842398591293, 1.2708535073963236),
            (103.8701626089698, 1.272478125147085), (103.8729152877786, 1.2744274425334177),
            (103.87539942590516, 1.2761667230332483), (103.87798888805737, 1.2786044766759599),
            (103.87961998538866, 1.2803079282324685), (103.88035998988545, 1.281126026223941),
            
            (103.88015595958008, 1.279876823350619), (103.87794696723554, 1.2760156588913745),
            (103.87575342472901, 1.2741523385668412), (103.87308676397144, 1.2716440212842248),
            (103.8713933579632, 1.270202428765887), (103.86904321654605, 1.2689752881102039),
            (103.86837268573561, 1.2672288316912699), (103.86619237872098, 1.2656609981916789),
            (103.86455962695321, 1.2639206107820578), (103.8614998081595, 1.2617259746892604),
            
            (114.15375981023712, 22.288470538067088), (114.15593233677556, 22.28924432037236),
            (114.15669939251775, 22.28902888139047), (114.15772073804816, 22.288707732196375),
            (114.158567236167, 22.28855059663363), (114.1595887382678, 22.28836692485724), 
            (114.15053917283947, 22.289391373646676), (114.15032553865947, 22.29061227505921),
            (114.14682306988259, 22.292691212710697), (114.15870453128642, 22.290641573354254)
        ]
    ]


    # start_time = time.time()
    i = int(sys.argv[1])

#     print('coordinate index', i)
    ship_index = 0;
    
    for ship_iri in list_ship_iri:
        write_ship_coordinates_fuseki(ship_iri, list_ship_coordinates[i][ship_index])
        
        
#         print(i)
#         print(ship_index)
        ship_name = ship_iri[ship_iri.rfind('#') + 1:]
        file_destination = "{0}\\{1}.owl".format(ship_owl_files_dir, ship_name)
#         print(file_destination)
        graph = Graph().parse(file_destination)
#         write_ship_coordinates_file(ship_iri, list_ship_coordinates[i][ship_index], graph, file_destination)
        
#         print('ship index', ship_index)
        print(read_ship_coordinates_fuseki(ship_iri))
#         print(read_ship_coordinates_file(ship_iri, graph))
        
        ship_index = ship_index + 1
    
    print("DONE")