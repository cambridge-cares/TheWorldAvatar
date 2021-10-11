##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 06 Oct 2021          #
##########################################

"""This module lists out the SPARQL queries used in generating the UK Grid Topology A-boxes"""

import os, sys, json
from rdflib.graph import ConjunctiveGraph
from rdflib.store import NO_STORE
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.queryInterface import performQuery, performUpdate, performFederatedQuery
from collections import Counter

qres = []

# query the GPS location of both from bus node and to bus node of a branch
def queryConnectedBusGPS(remoteEndPoint, SleepycatPath, FromBus_iri, ToBus_iri, localQuery):
    queryStr = """
    PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX space_and_time_extended:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    SELECT DISTINCT ?FromBus_latitude ?FromBus_longitude ?ToBus_latitude ?ToBus_longitude 
    WHERE
    {
    <%s> space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem_FromBus .
    <%s> rdf:type ontopowsys_PowSysFunction:PowerEquipmentConnection .
    ?CoordinateSystem_FromBus  space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate_FromBus .
    ?CoordinateSystem_FromBus  space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate_FromBus .
    ?x_coordinate_FromBus  system:hasValue ?GPS_x_coordinate_FromBus .
    ?y_coordinate_FromBus  system:hasValue ?GPS_y_coordinate_FromBus . 
    ?GPS_x_coordinate_FromBus  system:numericalValue ?FromBus_latitude .
    ?GPS_y_coordinate_FromBus  system:numericalValue ?FromBus_longitude .
    
    <%s> space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem_ToBus .
    <%s> rdf:type ontopowsys_PowSysFunction:PowerEquipmentConnection .
    ?CoordinateSystem_ToBus  space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate_ToBus .
    ?CoordinateSystem_ToBus  space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate_ToBus .
    ?x_coordinate_ToBus  system:hasValue ?GPS_x_coordinate_ToBus .
    ?y_coordinate_ToBus  system:hasValue ?GPS_y_coordinate_ToBus . 
    ?GPS_x_coordinate_ToBus  system:numericalValue ?ToBus_latitude .
    ?GPS_y_coordinate_ToBus  system:numericalValue ?ToBus_longitude .
    }
    """ % (FromBus_iri, FromBus_iri, ToBus_iri, ToBus_iri)
        
    global qres
    
    if localQuery == False and remoteEndPoint != None: 
        print('remoteQuery queryConnectedBusGPS')
        res = json.loads(performQuery(remoteEndPoint, queryStr))
        print('queryConnectedBusGPS is done')
        qres = [[ float(r['FromBus_latitude']), float(r['FromBus_longitude']), float(r['ToBus_latitude']), float(r['ToBus_longitude'])] for r in res]
        return qres
    elif SleepycatPath != None and localQuery == True:  
        print('localQuery queryConnectedBusGPS')
        pp_cg = ConjunctiveGraph('Sleepycat')
        sl = pp_cg.open(SleepycatPath, create = False)
        if sl == NO_STORE:
            print('Cannot find the UK grid topology store')
            return None
        qres = list(pp_cg.query(queryStr))
        pp_cg.close()
        return qres

############################################################################################################################
# query the bus node iri, its located region and latitude and longitude
def queryBusTopologicalInformation(numOfBus, ConjunctiveGraph, localQuery, endPoint):
    label = "_" + str(numOfBus) + "_"
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX space_and_time_extended:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    SELECT DISTINCT ?Bus_node ?Region ?Bus_lat ?Bus_lon
    WHERE
    {
    ?Topology rdf:type ontocape_network_system:NetworkSystem .
    ?Topology rdfs:label ?label .
    FILTER regex(?label, "%s") .
    ?Topology ontocape_upper_level_system:isComposedOfSubsystem ?Bus_node .
    ?Bus_node rdf:type ontopowsys_PowSysFunction:PowerEquipmentConnection .
    ?Bus_node ontocape_upper_level_system:hasAddress ?Region .  
    ?Region rdf:type <https://dbpedia.org/ontology/Region> .   
    
    ?Bus_node space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem_Bus .   
    ?CoordinateSystem_Bus  space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate_Bus .
    ?CoordinateSystem_Bus  space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate_Bus .
    ?x_coordinate_Bus  system:hasValue ?GPS_x_coordinate_Bus .
    ?y_coordinate_Bus  system:hasValue ?GPS_y_coordinate_Bus . 
    ?GPS_x_coordinate_Bus  system:numericalValue ?Bus_lat .
    ?GPS_y_coordinate_Bus  system:numericalValue ?Bus_lon .    
    }
    """ %label 
    #GRAPH ?g { ?Location_region rdf:type <https://dbpedia.org/ontology/Region> .}

    global qres
    
    if localQuery == False and endPoint != None: 
        print('remoteQuery queryBusLocatedRegion')
        res = json.loads(performQuery(endPoint, queryStr))
        print('queryBusLocatedRegion is done')
        return res
        # qres = [[ str(r['Bus_node']), str(r['Region']), float(r['Bus_lat']), float(r['Bus_lon'])] for r in res]
       
    elif ConjunctiveGraph != None and localQuery == True:  
        print('localQuery queryBusLocatedRegion')
        print('##################WARNING: The returen will be an array instead of a dictionary.###################')
        res = ConjunctiveGraph.query(queryStr)
        qres = [[ str(r[0]), str(r[1]), float(r[2]), float(r[3]) ] for r in res]  
        return qres

# query all PowerGenerators and their located region, latitude, longitude, PrimaryFuel and GenerationTechnology
def queryPowerPlantAttributes(ConjunctiveGraph, localQuery, endPoint_label):
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    PREFIX ontocape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
    SELECT DISTINCT ?PowerGenerator ?Region ?lat ?lon ?PrimaryFuel ?GenerationTechnology
    WHERE
    {      	
    ?powerPlant a ontoeip_powerplant:PowerPlant .
    ?powerPlant ontocape_upper_level_system:hasAddress ?Region .
    ?powerPlant ontocape_technical_system:hasRealizationAspect ?PowerGenerator . 
    ?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:consumesPrimaryFuel ?PrimaryFuel .
    ?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:usesGenerationTechnology ?GenerationTechnology .  
    
    ?powerPlant space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem .
    ?CoordinateSystem space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate .
    ?CoordinateSystem space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate .
    ?x_coordinate ontocape_upper_level_system:hasValue ?GPS_x_coordinate .
    ?y_coordinate ontocape_upper_level_system:hasValue ?GPS_y_coordinate . 
    ?GPS_x_coordinate ontocape_upper_level_system:numericalValue ?lon . # longitude is east/west
    ?GPS_y_coordinate ontocape_upper_level_system:numericalValue ?lat . # latitude is north/south
    }
    """
    global qres    
    if localQuery == False and endPoint_label != None: 
        print('remoteQuery queryPowerPlantsLocatedInGB')
        res = json.loads(performQuery(endPoint_label, queryStr))
        print('queryPowerPlantsLocatedInGB is done')
        # qres = [[ str(r['PowerGenerator']), str(r['Region']), float(r['lat']), float(r['lon']), str(r['PrimaryFuel'].split('#')[1]), \
        #          str(r['GenerationTechnology'].split('#')[1]) ] for r in res]
        return res
    
    elif ConjunctiveGraph != None and localQuery == True:  
        print('localQuery queryPowerPlantsLocatedInGB')
        print('##################WARNING: The returen will be an array instead of a dictionary.###################')
        res = ConjunctiveGraph.query(queryStr)
        qres = [[ str(r[0]), str(r[1]), float(r[2]), float(r[3]), str(r[4]), str(r[5])] for r in res]   
        return qres
###########################ENDENDEND#################################################################################################

# query the bus node iri and its located region
def queryBusLocatedRegion(numOfBus, ConjunctiveGraph, localQuery, endPoint):
    label = "_" + str(numOfBus) + "_"
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT DISTINCT ?Bus_node ?Location_region
    WHERE
    {
    ?Topology rdf:type ontocape_network_system:NetworkSystem .
    ?Topology rdfs:label ?label .
    FILTER regex(?label, "%s") .
    ?Topology ontocape_upper_level_system:isComposedOfSubsystem ?Bus_node .
    ?Bus_node rdf:type ontopowsys_PowSysFunction:PowerEquipmentConnection .
    ?Bus_node ontocape_upper_level_system:hasAddress ?Location_region . 
    
    ?Location_region rdf:type <https://dbpedia.org/ontology/Region> .   
    }
    """%label 
    #GRAPH ?g { ?Location_region rdf:type <https://dbpedia.org/ontology/Region> .}

    global qres
    
    if localQuery == False and endPoint != None: 
        print('remoteQuery queryBusLocatedRegion')
        res = json.loads(performQuery(endPoint, queryStr))
        print('queryBusLocatedRegion is done')
        qres = [[ str(r['Bus_node']), str(r['Location_region'])] for r in res]
       
    elif ConjunctiveGraph != None and localQuery == True:  
        print('localQuery queryBusLocatedRegion')
        res = ConjunctiveGraph.query(queryStr)
        qres = [[ str(r[0]), str(r[1])] for r in res]   
    
    # check one region has at most one bus
    region = [ r[1] for r in qres]
    # duplicatesOfRegion = [ region[i] for i in range(len(region)) if i == region.index(region[i])]
    duplicatesOfRegion = [k for k, v in Counter(region).items() if v > 1]
    
    if len(duplicatesOfRegion) > 0:
        print("The duplicatesOfRegion are: ", duplicatesOfRegion)
        raise Exception("Sorry, more than one buses located in the same region. This cluster principle cannot deal with this situation.")
    
    return qres
    

# query the iri of PowerGenerator of a power plant located in a specified location as well as its PrimaryFuel and GenerationTechnology
def queryPowerPlantLocatedInSameRegion(remoteEndPoint, SleepycatPath, location_iri, localQuery):
    queryStr = """
    PREFIX ontocape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
    SELECT DISTINCT ?PowerGenerator ?PrimaryFuel ?GenerationTechnology
    WHERE
    {
    ?powerPlant ontocape_upper_level_system:hasAddress <%s> .
    ?powerPlant ontocape_technical_system:hasRealizationAspect ?PowerGenerator . 
    ?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:consumesPrimaryFuel ?PrimaryFuel .
    ?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:usesGenerationTechnology ?GenerationTechnology .  
    }
    """ % location_iri  
    
    global qres
    
    if localQuery == False and remoteEndPoint != None: 
        print('remoteQuery queryPowerPlantLocatedInSameRegion')
        res = json.loads(performQuery(remoteEndPoint, queryStr))
        print('queryPowerPlantLocatedInSameRegion is done')
        qres = [[ str(r["PowerGenerator"]), str(r["PrimaryFuel"]), str(r["GenerationTechnology"])] for r in res]
        return qres
    elif SleepycatPath != None and localQuery == True:  
        print('localQuery queryPowerPlantLocatedInSameRegion')
        pp_cg = ConjunctiveGraph('Sleepycat')
        sl = pp_cg.open(SleepycatPath, create = False)
        if sl == NO_STORE:
            print('Cannot find the UK PowerPlant sleepycat store')
            return None
        qres = list(pp_cg.query(queryStr))
        pp_cg.close()
        return qres
    
def queryBusGPSLocation(numOfBus, ConjunctiveGraph, localQuery, endPoint_label):
    label = "_" + str(numOfBus) + "_"
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
    PREFIX mathematical_model: <http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#>
    PREFIX ontopowsys_PowerSystemModel: <http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#>
    PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    SELECT DISTINCT ?EquipmentConnection_EBus ?lat ?lon 
    WHERE
    {    
    ?Topology rdf:type ontocape_network_system:NetworkSystem .
    ?Topology rdfs:label ?label .
    FILTER regex(?label, "%s") .
    ?Topology ontocape_upper_level_system:isComposedOfSubsystem ?EquipmentConnection_EBus .
    
    ?EquipmentConnection_EBus rdf:type ontopowsys_PowSysFunction:PowerEquipmentConnection .      
    ?EquipmentConnection_EBus space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem_Bus .    
    ?CoordinateSystem_Bus  space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate_Bus .
    ?CoordinateSystem_Bus  space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate_Bus .
    ?x_coordinate_Bus  ontocape_upper_level_system:hasValue ?GPS_x_coordinate_Bus .
    ?y_coordinate_Bus  ontocape_upper_level_system:hasValue ?GPS_y_coordinate_Bus . 
    ?GPS_x_coordinate_Bus  ontocape_upper_level_system:numericalValue ?lon .
    ?GPS_y_coordinate_Bus  ontocape_upper_level_system:numericalValue ?lat .
    }
    """% label
    
    global qres
    
    if localQuery == False and endPoint_label != None: 
        print('remoteQuery queryBusGPSLocation')
        res = json.loads(performQuery(endPoint_label, queryStr))
        print('queryBusGPSLocation is done')
        qres = [[ str(r['EquipmentConnection_EBus']), float(r['lat']), float(r['lon'])] for r in res]
        return qres
    elif ConjunctiveGraph != None and localQuery == True:  
        print('localQuery queryBusGPSLocation')
        res = ConjunctiveGraph.query(queryStr)
        qres = [[ str(r[0]), float(r[1]), float(r[2])] for r in res]   
        return qres

def queryPowerPlantsLocatedInGB(ConjunctiveGraph, localQuery, endPoint_label):
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    PREFIX ontocape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
    SELECT DISTINCT ?Region ?PowerGenerator ?PrimaryFuel ?GenerationTechnology ?lat ?lon ?powerPlant 
    WHERE
    {      	
    ?powerPlant a ontoeip_powerplant:PowerPlant .
    ?powerPlant ontocape_upper_level_system:hasAddress ?Region .
    ?powerPlant ontocape_technical_system:hasRealizationAspect ?PowerGenerator . 
    ?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:consumesPrimaryFuel ?PrimaryFuel .
    ?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:usesGenerationTechnology ?GenerationTechnology .  
    
    ?powerPlant space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem .
    ?CoordinateSystem space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate .
    ?CoordinateSystem space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate .
    ?x_coordinate ontocape_upper_level_system:hasValue ?GPS_x_coordinate .
    ?y_coordinate ontocape_upper_level_system:hasValue ?GPS_y_coordinate . 
    ?GPS_x_coordinate ontocape_upper_level_system:numericalValue ?lon . # longitude is east/west
    ?GPS_y_coordinate ontocape_upper_level_system:numericalValue ?lat . # latitude is north/south
    }
    """   
    global qres
    
    if localQuery == False and endPoint_label != None: 
        print('remoteQuery queryPowerPlantsLocatedInGB')
        res = json.loads(performQuery(endPoint_label, queryStr))
        print('queryPowerPlantsLocatedInGB is done')
        names_ = []
        for r in res:
            if str(r['Region']).replace('\n', '').replace(' ', '') == "https://dbpedia.org/page/Northern_Ireland": 
            # if "Northern_Ireland" in str(r['Region']).replace('\n', '').replace(' ', ''): 
                names_.append(r['powerPlant'].split('#')[1])                
                res.remove(r) 
                if r['powerPlant'].split('#')[1] == 'Gruig':
                    print(names_)
                    
        print(names_, len(names_))      
        qres = [[ str(r['PowerGenerator']), str(r['PrimaryFuel'].split('#')[1]), str(r['GenerationTechnology'].split('#')[1]), \
                 float(r['lat']), float(r['lon']) ] for r in res]
        return qres
    elif ConjunctiveGraph != None and localQuery == True:  
        print('localQuery queryPowerPlantsLocatedInGB')
        res = ConjunctiveGraph.query(queryStr)
        qres = [[ str(r[1]), float(r[2]), float(r[3]), float(r[4]), float(r[5])] for r in res]   
        return qres

if __name__ == '__main__':
    sl_pp = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Plant\\Sleepycat_UKpp"
    # sl_topo = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Energy_Consumption\\Sleepycat_UKec_UKtopo"
    iri = "http://dbpedia.org/resource/West_Midlands_(county)"  
    test_region = "http://dbpedia.org/resource/North_West_England"
    # scot_iri = 'http://dbpedia.org/resource/Scotland'
    # res = queryPowerPlantLocatedInSameRegion('ukdigitaltwin', sl_pp, test_region, False) 
    # res = queryBusLocatedRegion(29, None, False, 'ukdigitaltwin')
    # res = queryBusTopologicalInformation(10, None, False, 'ukdigitaltwin')
    # print(res)
    res = queryPowerPlantAttributes(None, False, 'ukdigitaltwin')
    print(res)
    # res = queryBusGPSLocation(29, None, False, 'ukdigitaltwin')
    # res = queryPowerPlantsLocatedInGB(None, False, 'ukdigitaltwin')

    # FromBus_iri = "http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-006"
    # ToBus_iri = "http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-001"
    # res = queryBusGPS('ukpowergridtopology', None, FromBus_iri, ToBus_iri, False)
    #print(res, len(res))
    
   
   
   
   
   
   
   
   
   