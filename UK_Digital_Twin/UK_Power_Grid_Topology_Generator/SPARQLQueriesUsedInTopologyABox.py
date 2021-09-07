##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 23 June 2021         #
##########################################

"""This module lists out the SPARQL queries used in generating the UK Grid Topology A-boxes"""

import os, sys, json
from rdflib.graph import ConjunctiveGraph
from rdflib.store import NO_STORE
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.queryInterface import performQuery, performUpdate, performFederatedQuery

qres = []

# query the GPD location of both from bus node and to bus node of a branch
def queryBusGPS(remoteEndPoint, SleepycatPath, FromBus_iri, ToBus_iri, localQuery):
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
        print('remoteQuery')
        res = json.loads(performQuery(remoteEndPoint, queryStr))
        print('query is done')
        qres = [[ str(r['FromBus_latitude']), str(r['FromBus_longitude']), str(r['ToBus_latitude']), str(r['ToBus_longitude'])] for r in res]
        return qres
    elif SleepycatPath != None and localQuery == True:  
        print('localQuery')
        pp_cg = ConjunctiveGraph('Sleepycat')
        sl = pp_cg.open(SleepycatPath, create = False)
        if sl == NO_STORE:
            print('Cannot find the UK grid topology store')
            return None
        qres = list(pp_cg.query(queryStr))
        pp_cg.close()
        return qres

# query the bus node iri and its located region
def queryBusLocation(ConjunctiveGraph, localQuery, *endPoints):
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    SELECT DISTINCT ?Bus_node ?Location_region
    WHERE
    {
    ?Bus_node rdf:type ontopowsys_PowSysFunction:PowerEquipmentConnection .
    ?Bus_node ontocape_upper_level_system:hasAddress ?Location_region . 
    
    ?Location_region rdf:type <https://dbpedia.org/ontology/Region> .   
    }
    """    
    #GRAPH ?g { ?Location_region rdf:type <https://dbpedia.org/ontology/Region> .}
    
    global qres
    
    if localQuery == False and len(endPoints) > 0: 
        print('remoteQuery')
        res = json.loads(performFederatedQuery(queryStr, *endPoints))
        print('query is done')
        qres = [[ str(r['Bus_node']), str(r['Location_region'])] for r in res]
        return qres
    elif ConjunctiveGraph != None and localQuery == True:  
        print('localQuery')
        res = ConjunctiveGraph.query(queryStr)
        qres = [[ str(r[0]), str(r[1])] for r in res]   
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
        print('remoteQuery')
        # print('############################',queryStr)
        res = json.loads(performQuery(remoteEndPoint, queryStr))
        print('query is done')
        qres = [[ str(r["PowerGenerator"]), str(r["PrimaryFuel"]), str(r["GenerationTechnology"])] for r in res]
        return qres
    elif SleepycatPath != None and localQuery == True:  
        print('localQuery')
        pp_cg = ConjunctiveGraph('Sleepycat')
        sl = pp_cg.open(SleepycatPath, create = False)
        if sl == NO_STORE:
            print('Cannot find the UK PowerPlant sleepycat store')
            return None
        qres = list(pp_cg.query(queryStr))
        pp_cg.close()
        return qres

if __name__ == '__main__':
    sl_pp = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Plant\\Sleepycat_UKpp"
    # sl_topo = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Energy_Consumption\\Sleepycat_UKec_UKtopo"
    iri = "http://dbpedia.org/resource/West_Midlands_(county)"  
    test_region = "http://dbpedia.org/resource/North_West_England"
    # scot_iri = 'http://dbpedia.org/resource/Scotland'
    res = queryPowerPlantLocatedInSameRegion('ukdigitaltwin', sl_pp, test_region, False) 
    
    # FromBus_iri = "http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-006"
    # ToBus_iri = "http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-001"
    # res = queryBusGPS('ukpowergridtopology', None, FromBus_iri, ToBus_iri, False)
    
    # topologyQueryEndPoint = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridTopology"
    # energyconsumptionQueryEndPoint = "	https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKEnergyConsumptionKG"
    # res =  queryBusLocation(None, False, 'https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridTopology', 'https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKEnergyConsumptionKG')
    #res = queryBusLocation(topologyQueryEndPoint, energyconsumptionQueryEndPoint, 'ukpowergridtopology', None, False)
    print(res)
   
   
   
   
   
   
   
   
   