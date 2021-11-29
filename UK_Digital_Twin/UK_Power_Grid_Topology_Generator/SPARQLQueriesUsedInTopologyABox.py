##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 22 Nov 2021          #
##########################################

"""This module lists out the SPARQL queries used in generating the UK Grid Topology A-boxes"""

import os, sys, json
from rdflib.graph import ConjunctiveGraph
from rdflib.store import NO_STORE
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.queryInterface import performQuery, performUpdate, performFederatedQuery
from collections import Counter
from shapely.wkt import loads

qres = []

# query the GPS location of both from bus node and to bus node of a branch
def queryConnectedBusGPS(remoteEndPoint, SleepycatPath, numOfBus, numOfBranch, FromBus_iri, ToBus_iri, localQuery):
    label = "UK_Topology_" + str(numOfBus) + "_Bus_" + str(numOfBranch) + "_Branch"
    queryStr = """
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    PREFIX ontoenergysystem: <http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#>
    SELECT DISTINCT ?FromBus_lat_lon ?ToBus_lat_lon
    WHERE
    {
    ?topology rdf:type ontocape_network_system:NetworkSystem .
    ?topology rdfs:label ?label .
    FILTER regex(?label, "%s") .
    ?topology ontocape_upper_level_system:isComposedOfSubsystem <%s> .
    <%s> ontoenergysystem:hasWGS84LatitudeLongitude ?FromBus_lat_lon .
    <%s> rdf:type ontopowsys_PowSysFunction:PowerEquipmentConnection .
    
    ?topology ontocape_upper_level_system:isComposedOfSubsystem <%s> .
    <%s> ontoenergysystem:hasWGS84LatitudeLongitude ?ToBus_lat_lon .
    <%s> rdf:type ontopowsys_PowSysFunction:PowerEquipmentConnection .
    }
    """ % (label, FromBus_iri, FromBus_iri, FromBus_iri, ToBus_iri, ToBus_iri, ToBus_iri)
        
    global qres
    
    if localQuery == False and remoteEndPoint != None: 
        print('remoteQuery queryConnectedBusGPS')
        res = json.loads(performQuery(remoteEndPoint, queryStr))
        print('queryConnectedBusGPS is done')
        qres = [[ float(r['FromBus_lat_lon'].split('#')[0]), float(r['FromBus_lat_lon'].split('#')[1]), \
                 float(r['ToBus_lat_lon'].split('#')[0]), float(r['ToBus_lat_lon'].split('#')[1])] for r in res]
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
def queryBusTopologicalInformation(numOfBus, numOfBranch, ConjunctiveGraph, localQuery, endPoint):
    label = "UK_Topology_" + str(numOfBus) + "_Bus_" + str(numOfBranch) + "_Branch"
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX space_and_time_extended:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontoenergysystem: <http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#>
    SELECT DISTINCT ?Bus_node ?EBus ?Bus_lat_lon
    WHERE
    {
    ?Topology rdf:type ontocape_network_system:NetworkSystem .
    ?Topology rdfs:label ?label .
    FILTER regex(?label, "%s") .
    ?Topology ontocape_upper_level_system:isComposedOfSubsystem ?Bus_node .
    ?Bus_node rdf:type ontopowsys_PowSysFunction:PowerEquipmentConnection .
    ?Bus_node ontoecape_technical_system:isRealizedBy ?EBus . 
    
    ?Bus_node ontoenergysystem:hasWGS84LatitudeLongitude ?Bus_lat_lon .   
    }
    """ %label 
    #GRAPH ?g { ?Location_region rdf:type <https://dbpedia.org/ontology/Region> .}

    global qres
    
    if localQuery == False and endPoint != None: 
        print('remoteQuery queryBusLocatedRegion')
        res = json.loads(performQuery(endPoint, queryStr))
        print('queryBusLocatedRegion is done')
        for r in res:
            r['Bus_lat_lon'] = [float(r['Bus_lat_lon'].split('#')[0]), float(r['Bus_lat_lon'].split('#')[1])]        
        return res
        
    elif ConjunctiveGraph != None and localQuery == True:  
        print('localQuery queryBusLocatedRegion')
        print('##################WARNING: The returen will be an array instead of a dictionary.###################')
        res = ConjunctiveGraph.query(queryStr)
        qres = [[ str(r[0]), str(r[1]), [float(r[2].split('#')[0]), float(r[2].split('#')[1])]] for r in res]  
        return qres

# query all PowerGenerators and their located region, latitude, longitude, PrimaryFuel and GenerationTechnology
def queryPowerPlantAttributes(ConjunctiveGraph, localQuery, endPoint_label):
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    PREFIX ontocape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
    PREFIX ontoenergysystem: <http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#>
    SELECT DISTINCT ?PowerGenerator ?LACode_PP ?PP_lat_lon ?PrimaryFuel ?GenerationTechnology
    WHERE
    {      	
    ?powerPlant a ontoeip_powerplant:PowerPlant .
    ?powerPlant ontoenergysystem:hasRelevantPlace ?LocatedPlace .
    ?LocatedPlace rdf:type ontoenergysystem:AdministrativeDivision .
    ?LocatedPlace ontoenergysystem:hasLocalAuthorityCode ?LACode_PP .
    
    ?powerPlant ontocape_technical_system:hasRealizationAspect ?PowerGenerator . 
    ?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:consumesPrimaryFuel ?PrimaryFuel .
    ?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:usesGenerationTechnology ?GenerationTechnology .  
    
    ?powerPlant ontoenergysystem:hasWGS84LatitudeLongitude ?PP_lat_lon .
    }
    # LIMIT 200
    """
    global qres    
    if localQuery == False and endPoint_label != None: 
        print('remoteQuery queryPowerPlantAttributes')
        res = json.loads(performQuery(endPoint_label, queryStr))
        print('queryPowerPlantAttributes is done')
        for r in res:
            r['PP_lat_lon'] = [float(r['PP_lat_lon'].split('#')[0]), float(r['PP_lat_lon'].split('#')[1])]
        # qres = [[ str(r['PowerGenerator']), str(r['Region']), float(r['lat']), float(r['lon']), str(r['PrimaryFuel'].split('#')[1]), \
        #          str(r['GenerationTechnology'].split('#')[1]) ] for r in res]
        return res
    
    elif ConjunctiveGraph != None and localQuery == True:  
        print('localQuery queryPowerPlantAttributes')
        print('##################WARNING: The returen will be an array instead of a dictionary###################')
        res = ConjunctiveGraph.query(queryStr)
        qres = [[ str(r[0]), str(r[1]), [float(r[2].split('#')[0]), float(r[2].split('#')[1])], float(r[3]), str(r[4])] for r in res]   
        return qres

# The query for the Region Boundaries returned from ONS
def queryRegionBoundaries(ONS_Endpoint_label):
    queryStr_england_region = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
    PREFIX ons_geosparql: <http://www.opengis.net/ont/geosparql#>
    SELECT DISTINCT ?LACode_area (GROUP_CONCAT(?areaBoundary;SEPARATOR = '***') AS ?Geo_InfoList)
    WHERE
    {
    ?area ons:status "live" .
    ?area rdf:type ons:Statistical-Geography .
    ?area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/E12> .
    ?area <http://publishmydata.com/def/ontology/foi/code> ?LACode_area .
    ?area ons_geosparql:hasGeometry ?geometry .
    ?geometry ons_geosparql:asWKT ?areaBoundary .
    } GROUP BY ?LACode_area
    """
    # Due to the limitation of the returned result exerted on the ONS endpoint, the query has to be splited into two parts
    queryStr_SWN = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
    PREFIX ons_geosparql: <http://www.opengis.net/ont/geosparql#>
    SELECT DISTINCT ?LACode_area (GROUP_CONCAT(?areaBoundary;SEPARATOR = '***') AS ?Geo_InfoList)
    WHERE
    {
    ?area ons:status "live" .
    ?area rdf:type ons:Statistical-Geography .
    { ?area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/W92> .} UNION 
    { ?area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/S92> .} UNION
    { ?area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/N92> .} 
   
    ?area <http://publishmydata.com/def/ontology/foi/code> ?LACode_area .
    ?area ons_geosparql:hasGeometry ?geometry .
    ?geometry ons_geosparql:asWKT ?areaBoundary .
    } GROUP BY ?LACode_area   
    """
    print('remoteQuery Region Boundaries')
    res_england_region = json.loads(performQuery(ONS_Endpoint_label, queryStr_england_region))
    res_SWN = json.loads(performQuery(ONS_Endpoint_label, queryStr_SWN))
    print('queryRegionBoundaries is done')
    for swn in res_SWN:
        res_england_region.append(swn)
    
    if len(res_england_region) != 12:
        raise Exception('The number of the region should be 12 in total but the number of queried is ' + str(len(res)))
    # clear the symbols in the query results
    for r in res_england_region:
      for key in r.keys():
          if '\"^^' in  r[key] :
            r[key] = (r[key].split('\"^^')[0]).replace('\"','') 
            
    # Check the availability of the geometry of each area
    for r in res_england_region:
      if len(r["Geo_InfoList"]) == 0: 
          raise Exception('There is one place does not have geometry information which is', r["LACode_area"], ', please check the query string and the place status in ONS.')
      elif "***" in r['Geo_InfoList']:
          r['Geo_InfoList'] = r['Geo_InfoList'].split("***")[0]
      r['Geo_InfoList'] = loads(r['Geo_InfoList']) # convert wkt into shapely polygons
    return res_england_region

# This function is designed to find the region which the given area within in 
def queryWithinRegion(LACode, ONS_Endpoint_label):
    LACode = str(LACode)
    typeCode = int(LACode[1] + LACode[2])
    if LACode[0] == 'E':
        if not typeCode >= 11: # E11, E12 and other places whose code is larger than 11 are not included in any areas
            queryStr = """
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
            PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
            PREFIX ons_geosparql: <http://www.opengis.net/ont/geosparql#>
            PREFIX foi: <http://publishmydata.com/def/ontology/foi/>
            SELECT DISTINCT ?LACode_Region
            WHERE
            {
            ?area <http://publishmydata.com/def/ontology/foi/code> "%s" .
            ?area foi:within+ ?Region .
            ?Region ons:status "live" .
            ?Region ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/E12> .
            ?Region <http://publishmydata.com/def/ontology/foi/code> ?LACode_Region .
            }
            """%LACode
            print('remoteQuery WithinRegion of a given LA code')
            res = json.loads(performQuery(ONS_Endpoint_label, queryStr))
            print('queryWithinRegion is done')
            if len(res) != 1:
                # raise Exception('The within region of the given LA code cannot be found, please check if the given LA code is in the hierarchy.')
                print('The within region of the given LA code cannot be found, please check if the given LA code is in the hierarchy.')
                return None
            RegionOrCountry = [str(res[0]['LACode_Region'])]
        else :
            # raise Exception('The given LA coed is ', LACode,' which is not within any region of England.')
            print('The given LA coed is ', LACode,' which is not within any region of England.')
            return None
    elif LACode[0] == 'W':
        RegionOrCountry = ['W92000004', 'W08000001']    
    elif LACode[0] == 'S':
        RegionOrCountry = ['S92000003', 'S04000001']      
    elif LACode[0] == 'N':
        RegionOrCountry = ['N92000002', 'N07000001']
    else:     
        # raise Exception('The given area does not have a within region, please check the given LA code.')   
        print('The given area does not have a within region, please check the given LA code.')
        return None
    return RegionOrCountry

# This query is used to query the boundary of the GB and Northern Ireland
def queryGBOrNIBoundary(ONS_Endpoint_label):
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
    PREFIX ons_geosparql: <http://www.opengis.net/ont/geosparql#>
    SELECT DISTINCT ?LACode_area (GROUP_CONCAT(?areaBoundary;SEPARATOR = '***') AS ?Geo_InfoList)
    WHERE
    {
    ?area ons:status "live" .
    ?area rdf:type ons:Statistical-Geography .
    { ?area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/K03> .} UNION 
    { ?area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/N92> .}
    ?area <http://publishmydata.com/def/ontology/foi/code> ?LACode_area .
    ?area ons_geosparql:hasGeometry ?geometry .
    ?geometry ons_geosparql:asWKT ?areaBoundary .
    } GROUP BY ?LACode_area
    """
    print('query GBOrNIBoundary')
    res = json.loads(performQuery(ONS_Endpoint_label, queryStr))  
    print('queryGBOrNIBoundary is done')
    # clear the symbols in the query results
    for r in res:
      for key in r.keys():
          if '\"^^' in  r[key]:
            r[key] = (r[key].split('\"^^')[0]).replace('\"','') 
    for r in res:
      if len(r["Geo_InfoList"]) == 0: 
          raise Exception('There is one place does not have geometry information which is', r["LACode_area"], ', please check the query string and the place status in ONS.')
      elif "***" in r['Geo_InfoList']:
          r['Geo_InfoList'] = r['Geo_InfoList'].split("***")[0]
      r['Geo_InfoList'] = loads(r['Geo_InfoList']) # convert wkt into shapely polygons
    return res         

def queryifWithin(LACode_toBeCheck, givenLACode, ONS_Endpoint_label):
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
    PREFIX ons_geosparql: <http://www.opengis.net/ont/geosparql#>
    PREFIX foi: <http://publishmydata.com/def/ontology/foi/>
    ASK  
    {
    ?areaToBeChecked <http://publishmydata.com/def/ontology/foi/code> "%s" .
    ?areaGiven <http://publishmydata.com/def/ontology/foi/code> "%s" .
    ?areaToBeChecked foi:within ?areaGiven .
    }
    """%(str(LACode_toBeCheck), str(givenLACode))
    print('query ifWithin condition')
    res = json.loads(performQuery(ONS_Endpoint_label, queryStr))  
    print('queryifWithin is done')
    res = res[0]['ASK']
    return res
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
    #res = queryConnectedBusGPS('ukdigitaltwin', None, FromBus_iri, ToBus_iri, localQuery)
    # res = queryPowerPlantLocatedInSameRegion('ukdigitaltwin', sl_pp, test_region, False) 
    # res = queryBusLocatedRegion(29, None, False, 'ukdigitaltwin')
    # res = queryBusTopologicalInformation(29, 99, None, False, 'ukdigitaltwin')
    res = queryBusTopologicalInformation(10, 14, None, False, 'ukdigitaltwin')
    # res = queryRegionBoundaries('ons')
    # print(res)
    # res = queryPowerPlantAttributes(None, False, 'ukdigitaltwin')
    # res = queryBusGPSLocation(29, None, False, 'ukdigitaltwin')
    # res = queryPowerPlantsLocatedInGB(None, False, 'ukdigitaltwin')
    # res = queryGBOrNIBoundary('ons')
    # res = queryWithinRegion('E12000007', 'ons')
    # FromBus_iri = "http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-001"
    # ToBus_iri = "http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-002"
    # FromBus_iri = "http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-006"
    # ToBus_iri = "http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-001"
    # res = queryConnectedBusGPS('ukdigitaltwin', None, 10, 14, FromBus_iri, ToBus_iri, False)
    # res = queryConnectedBusGPS('ukdigitaltwin', None, 29, 99, FromBus_iri, ToBus_iri, False)
    
    # res = queryifWithin('E12000007', 'K03000001', 'ons')
    # print(res, len(res), type(res))
    print(res)
    
   
   
   
   
   
   
   
   
   