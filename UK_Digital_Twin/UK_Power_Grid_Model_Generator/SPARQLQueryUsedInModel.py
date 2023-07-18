##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 19 April 2022        #
##########################################

"""This module lists out the SPARQL queries used in generating the UK Grid Model A-boxes"""
import os, sys, json
from rdflib.graph import ConjunctiveGraph
from rdflib.store import NO_STORE
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.queryInterface import performQuery, performUpdate, performFederatedQuery
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel as endpointList
# import numpy as np 
from shapely.wkt import loads
from shapely.geometry import mapping
import geojson
import ast
from UK_Power_Grid_Topology_Generator.SPARQLQueriesUsedInTopologyABox import queryGBOrNIBoundary
import shapely.geometry
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel
from rfc3987 import parse
from logging import raiseExceptions

qres = []
qres_ = []
capa_PrimaryFuel = []
qres_capa = []
allCapacity = []

#####UPDATED#####

####Bus information query####
def queryBusTopologicalInformation(topologyNodeIRI, endpoint):
    if endpoint == str(EndPointConfigAndBlazegraphRepoLabel.ukdigitaltwin['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.ukdigitaltwin['endpoint_iri'])
    elif parse(endpoint, rule='IRI'):
        endPointIRI = endpoint
    else:
        raiseExceptions("!!!!Please provide a valid ONS_Endpoint!!!!")
    
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX meta_model_topology: <http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>
    PREFIX ontoenergysystem: <http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>  
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
    PREFIX ontocape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontoeip_system_requirement: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_requirement.owl#>
    SELECT DISTINCT ?BusNodeIRI ?BusLatLon (GROUP_CONCAT(?Capacity;SEPARATOR = '***') AS ?GenerationLinkedToBusNode)
    WHERE
    {
    <%s> ontocape_upper_level_system:isComposedOfSubsystem ?BusNodeIRI . 
    ?BusNodeIRI rdf:type ontopowsys_PowSysRealization:BusNode . 

    ?PowerGenerator meta_model_topology:hasOutput ?BusNodeIRI .
    ?PowerPlant ontocape_technical_system:hasRealizationAspect ?PowerGenerator .
    ?PowerPlant ontocape_technical_system:hasRequirementsAspect ?pp_capa .
    ?pp_capa rdf:type ontoeip_system_requirement:DesignCapacity .
    ?pp_capa ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Capacity .
    
    ?BusNodeIRI ontoenergysystem:hasWGS84LatitudeLongitude ?BusLatLon .  

    } GROUP BY ?BusNodeIRI ?BusLatLon
    """ % (topologyNodeIRI)
    
    print('...remoteQuery queryBusTopologicalInformation...')
    res = json.loads(performQuery(endPointIRI, queryStr))
    print('...queryBusTopologicalInformation is done...')
    
    for r in res:
        r['BusLatLon'] = [float(r['BusLatLon'].split('#')[0]), float(r['BusLatLon'].split('#')[1])] 
        generationOfBusNode = 0
        for capa in r['GenerationLinkedToBusNode'].split('***'):
            generationOfBusNode += float(capa)
        r['GenerationLinkedToBusNode'] = generationOfBusNode    
    return int(len(res)), res 

def queryBusGPSLocation(topologyNodeIRI, endpoint):
    if endpoint == str(EndPointConfigAndBlazegraphRepoLabel.ukdigitaltwin['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.ukdigitaltwin['endpoint_iri'])
    elif parse(endpoint, rule='IRI'):
        endPointIRI = endpoint
    else:
        raiseExceptions("!!!!Please provide a valid ONS_Endpoint!!!!")
    
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX meta_model_topology: <http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>
    PREFIX ontoenergysystem: <http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>  
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
    PREFIX ontocape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontoeip_system_requirement: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_requirement.owl#>
    SELECT DISTINCT ?BusNodeIRI ?BusLatLon
    WHERE
    {
    <%s> ontocape_upper_level_system:isComposedOfSubsystem ?BusNodeIRI . 
    ?BusNodeIRI rdf:type ontopowsys_PowSysRealization:BusNode . 
    ?BusNodeIRI ontoenergysystem:hasWGS84LatitudeLongitude ?BusLatLon .  

    }
    """ % (topologyNodeIRI)
    
    print('...remoteQuery queryBusTopologicalInformation...')
    res = json.loads(performQuery(endPointIRI, queryStr))
    print('...queryBusTopologicalInformation is done...')
    
    for r in res:
        r['BusLatLon'] = [float(r['BusLatLon'].split('#')[0]), float(r['BusLatLon'].split('#')[1])]    
    return res 

####EGen information query####
def queryEGenInfo(topologyNodeIRI, endPoint, eliminateClosedPlantIRIList:list):
    if endPoint == str(EndPointConfigAndBlazegraphRepoLabel.ukdigitaltwin['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.ukdigitaltwin['endpoint_iri'])
    elif parse(endPoint, rule='IRI'):
        endPointIRI = endPoint
    else:
        raiseExceptions("!!!!Please provide a valid endpoint!!!!")

    if len(eliminateClosedPlantIRIList) > 0:
        NotIncludeStr = "FILTER NOT EXISTS { "
        for ppiri in eliminateClosedPlantIRIList:
            if '<' and '>' not in ppiri:
               ppiri = '<' + ppiri + '>'
            conditionStr = "{ %s ontocape_technical_system:hasRealizationAspect ?PowerGenerator } UNION"%ppiri
            NotIncludeStr += conditionStr
        NotIncludeStr = NotIncludeStr[:-5]
        NotIncludeStr += "}"                                         
    else:
        NotIncludeStr = ""

    queryStr = """
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
    PREFIX ontopowsys_PowSysPerformance: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
    PREFIX meta_model_topology: <http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontoeip_system_requirement: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_requirement.owl#>
    PREFIX ontocape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontoenergysystem: <http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#>
    SELECT DISTINCT ?PowerGenerator ?FixedMO ?VarMO ?FuelCost ?CO2EmissionFactor ?Bus ?Capacity ?PrimaryFuel ?LatLon ?PowerPlant_LACode ?GenerationTech
    WHERE
    {
    ?GBElectricitySystemIRI ontocape_upper_level_system:contains ?PowerPlant .
    ?GBElectricitySystemIRI ontoenergysystem:hasRelevantPlace/owl:sameAs <https://dbpedia.org/page/Great_Britain> .

    <%s> ontocape_upper_level_system:isComposedOfSubsystem ?PowerGenerator . 
    <%s> ontocape_upper_level_system:isComposedOfSubsystem ?Bus . 
    
    ?PowerGenerator meta_model_topology:hasOutput ?Bus .
    ?Bus rdf:type ontopowsys_PowSysRealization:BusNode .  
    ?PowerGenerator rdf:type ontoeip_powerplant:PowerGenerator . 
    
    ?PowerGenerator ontopowsys_PowSysPerformance:hasFixedMaintenanceCost/ontocape_upper_level_system:hasValue ?v_FixedMO .
    ?v_FixedMO ontocape_upper_level_system:numericalValue ?FixedMO .
    
    ?PowerGenerator ontopowsys_PowSysPerformance:hasCost/ontocape_upper_level_system:hasValue ?v_VarMO .
    ?v_VarMO ontocape_upper_level_system:numericalValue ?VarMO .
    
    ?PowerGenerator ontopowsys_PowSysPerformance:hasFuelCost/ ontocape_upper_level_system:hasValue ?v_FuelCost .
    ?v_FuelCost ontocape_upper_level_system:numericalValue ?FuelCost .
    
    ?PowerGenerator ontoeip_powerplant:hasEmissionFactor/ontocape_upper_level_system:hasValue ?v_CO2EmissionFactor .
    ?v_CO2EmissionFactor ontocape_upper_level_system:numericalValue ?CO2EmissionFactor .
    
    ?PowerPlant ontocape_technical_system:hasRealizationAspect ?PowerGenerator .
    ?PowerPlant ontocape_technical_system:hasRequirementsAspect ?pp_capa .
    ?pp_capa rdf:type ontoeip_system_requirement:DesignCapacity .
    ?pp_capa ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Capacity .
    
    ?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:consumesPrimaryFuel/rdf:type ?PrimaryFuel .

    ?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:usesGenerationTechnology/rdf:type ?GenerationTech .

    ?PowerPlant ontocape_technical_system:hasRealizationAspect ?PowerGenerator . 
    ?PowerPlant ontoenergysystem:hasWGS84LatitudeLongitude ?LatLon .

    ?PowerPlant ontoenergysystem:hasRelevantPlace/ontoenergysystem:hasLocalAuthorityCode ?PowerPlant_LACode .

    %s

    }
    """% (topologyNodeIRI, topologyNodeIRI, NotIncludeStr)
    
    counterBusNumber = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
    SELECT (COUNT(?Bus) AS ?count)
    WHERE
    {
    <%s> ontocape_upper_level_system:isComposedOfSubsystem ?Bus . 
    ?Bus rdf:type ontopowsys_PowSysRealization:BusNode .
    }
    """% topologyNodeIRI

    print('...starts queryEGenInfo...')
    res = json.loads(performQuery(endPointIRI, queryStr))
    qres = [[ str(r['PowerGenerator']), float((r['FixedMO'].split('\"^^')[0]).replace('\"','')), float((r['VarMO'].split('\"^^')[0]).replace('\"','')), \
                float((r['FuelCost'].split('\"^^')[0]).replace('\"','')), float((r['CO2EmissionFactor'].split('\"^^')[0]).replace('\"','')), str(r['Bus']), \
                float((r['Capacity'].split('\"^^')[0]).replace('\"','')), (str(r['PrimaryFuel']).split('#'))[1], \
                [float(r['LatLon'].split('#')[0]), float(r['LatLon'].split('#')[1])], str(r['PowerPlant_LACode']), str(r['GenerationTech'])] for r in res]
    print('...finishes queryEGenInfo...')
    # print('...starts querying counterBusNumber...')
    # numOfBus = json.loads(performQuery(endPoint_label, counterBusNumber))
    # print('...finishes querying counterBusNumber...')
    return qres #, int(numOfBus[0]["count"])
    
# query the total electricity consumption of a UK official region 
def queryTotalElecConsumptionofGBOrUK(endPoint_label, topologyNodeIRI, startTime_of_EnergyConsumption):
    if endPoint_label == str(EndPointConfigAndBlazegraphRepoLabel.ukdigitaltwin['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.ukdigitaltwin['endpoint_iri'])
    elif parse(endPoint_label, rule='IRI'):
        endPointIRI = endPoint_label
    else:
        raiseExceptions("!!!!Please provide a valid endpoint!!!!")

    queryStr_BusAndLatlon = """
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontoenergysystem: <http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#>
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
    SELECT DISTINCT ?Bus_node ?Bus_lat_lon
    WHERE 
    {   
    <%s> ontocape_upper_level_system:isComposedOfSubsystem ?Bus_node .
    ?Bus_node rdf:type ontopowsys_PowSysRealization:BusNode . 
    ?Bus_node ontoenergysystem:hasWGS84LatitudeLongitude ?Bus_lat_lon .
    }"""% topologyNodeIRI
    
    ons_label = endpointList.ONS['label']

    print('remoteQuery BusAndLatlon and GBOrNIBoundary')
    res_BusAndLatlon = json.loads(performQuery(endPointIRI, queryStr_BusAndLatlon))
    boundaries = queryGBOrNIBoundary(ons_label)
    print('query of BusAndLatlon and GBOrNIBoundary is done')
    
    # Query the boundaries of GB and NI
    countryBoundaryDict = {}
    for boundary in boundaries:
        countryBoundaryDict.update({boundary['LACode_area']: boundary['Geo_InfoList']})
    
    # Check which area, GB or NI, being located with buses
    GBAndNI = ['K03000001', 'N92000002']
    for bus in res_BusAndLatlon:
        bus['Bus_lat_lon'] = [float(bus['Bus_lat_lon'].split('#')[0]), float(bus['Bus_lat_lon'].split('#')[1])]
        bus_lonlat_point = shapely.geometry.Point(bus['Bus_lat_lon'][1], bus['Bus_lat_lon'][0])
        interior_GB = countryBoundaryDict['K03000001'].intersects(bus_lonlat_point)
        interior_NI = countryBoundaryDict['N92000002'].intersects(bus_lonlat_point)
        if interior_GB == True:
            if 'K03000001' in GBAndNI:
                GBAndNI.remove('K03000001')
            elif len(GBAndNI) == 0: break
        elif interior_NI == True:
            if 'N92000002' in GBAndNI:
                GBAndNI.remove('N92000002')
            elif len(GBAndNI) == 0: break
    # Based on the bus location, decide the electricity consumption area
    query_Area = ''
    if len(GBAndNI) == 0:
        query_Area = 'K02000001'
    elif len(GBAndNI) == 1 and 'N92000002' in GBAndNI:
        query_Area = 'K03000001'
    elif len(GBAndNI) == 1 and 'K03000001' in GBAndNI:
        query_Area = 'N92000002'
    if len(query_Area) == 0:
        raise Exception('The queried buses do not located in the UK, please check the bus query result.')
    
    queryStr_electricity_consumption = """
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_space_and_time: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#>
    PREFIX ontocape_derived_SI_units: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#>
    PREFIX ontocape_coordinate_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#>    
    PREFIX ontoenergysystem: <http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#>
    SELECT DISTINCT  ?v_TotalELecConsumption
    WHERE 
    {   
    ?Total_ele_consumption ontocape_derived_SI_units:hasTimePeriod/ontocape_upper_level_system:hasValue ?TimePeriod .
    ?TimePeriod ontoecape_space_and_time:hasStartingTime ?startTime .
    ?startTime rdf:type ontocape_coordinate_system:CoordinateValue . 
    ?startTime ontocape_upper_level_system:numericalValue "%s"^^xsd:dateTime .
    
    ?Total_ele_consumption ontoenergysystem:isObservedIn/ontoenergysystem:hasLocalAuthorityCode "%s" .
    ?Total_ele_consumption ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?v_TotalELecConsumption .
    }
    """% (startTime_of_EnergyConsumption, query_Area)
    
    print('remoteQuery electricity_consumption')
    res_electricity_consumption = json.loads(performQuery(endPointIRI, queryStr_electricity_consumption))
    print('query of electricity_consumption is done')
    if str(res_electricity_consumption) == '[]':
        raise Exception('Cannot find the total consumtion of the area', query_Area)
    res = float(res_electricity_consumption[0]['v_TotalELecConsumption']) 
    return res

# Query the located country of the Power System
def queryPowerSystemLocation(endpoint_label, topologyNodeIRI):
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoenergysystem: <http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#>
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    SELECT DISTINCT ?Location
    WHERE
    {
    <%s> ontoenergysystem:isTopologyOf/ontocape_upper_level_system:isDirectSubsystemOf ?ElectricityPowerSystem .
    ?ElectricityPowerSystem ontoenergysystem:hasRelevantPlace/owl:sameAs ?Location .
    }
    """% topologyNodeIRI
    print('...starts queryPowerSystemLocation...')
    res = json.loads(performQuery(endpoint_label, queryStr))
    qres = str(res[0]['Location'])
    print('...finishes queryPowerSystemLocation...')
    return qres

###############EBus#############
# Query the total consumption of the regions
def queryElectricityConsumption_Region(startTime_of_EnergyConsumption, UKDigitalTwinEndPoint_iri, ONSEndPoint_iri):
    queryStr = """
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_space_and_time: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#>
    PREFIX ontocape_derived_SI_units: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#>
    PREFIX ontocape_coordinate_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#>    
    PREFIX ontoenergysystem: <http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#>
    PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
    SELECT DISTINCT ?RegionOrCountry_LACode ?v_TotalELecConsumption
    WHERE 
    {   
    ?Total_ele_consumption ontocape_derived_SI_units:hasTimePeriod/ontocape_upper_level_system:hasValue ?TimePeriod .
    ?TimePeriod ontoecape_space_and_time:hasStartingTime ?startTime .
    ?startTime rdf:type ontocape_coordinate_system:CoordinateValue . 
    ?startTime ontocape_upper_level_system:numericalValue "%s"^^xsd:dateTime .
    
    ?Total_ele_consumption ontoenergysystem:isObservedIn/ontoenergysystem:hasLocalAuthorityCode ?RegionOrCountry_LACode .
    ?RegionOrCountry <http://publishmydata.com/def/ontology/foi/code> ?RegionOrCountry_LACode .
    {?RegionOrCountry ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/E12> .} UNION 
    {?RegionOrCountry ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/W92> .} UNION
    {?RegionOrCountry ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/S92> .} UNION
    {?RegionOrCountry ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/N92> .}
    ?Total_ele_consumption ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?v_TotalELecConsumption .
    }
    """% (startTime_of_EnergyConsumption)
     
    print('...starts queryElectricityConsumption_Region...')   
    res = json.loads(performFederatedQuery(queryStr, [UKDigitalTwinEndPoint_iri, ONSEndPoint_iri]))
    print('...queryElectricityConsumption_Region is done...') 
    for r in res:
        for key in r.keys():
           if '\"^^' in  r[key] :
             r[key] = (r[key].split('\"^^')[0]).replace('\"','') 
        r['v_TotalELecConsumption'] = float(r['v_TotalELecConsumption'])
    return res                  
         
# query the total electricity consumption of each address area
def queryElectricityConsumption_LocalArea(startTime_of_EnergyConsumption, UKDigitalTwinEndPoint_iri, ONSEndPoint_iri):    
    queryStr = """
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_space_and_time: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#>
    PREFIX ontocape_derived_SI_units: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#>
    PREFIX ontocape_coordinate_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#>    
    PREFIX ontoenergysystem: <http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#>
    PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
    PREFIX ont: <http://www.opengis.net/ont/geosparql#>
    SELECT DISTINCT ?Area_LACode ?v_TotalELecConsumption (GROUP_CONCAT(?Geo_Info;SEPARATOR = '***') AS ?Geo_InfoList)
    WHERE 
    {   
    ?Total_ele_consumption ontocape_derived_SI_units:hasTimePeriod/ontocape_upper_level_system:hasValue ?TimePeriod .
    ?TimePeriod ontoecape_space_and_time:hasStartingTime ?startTime .
    ?startTime rdf:type ontocape_coordinate_system:CoordinateValue . 
    ?startTime ontocape_upper_level_system:numericalValue "%s"^^xsd:dateTime .
    
    ?Total_ele_consumption ontoenergysystem:isObservedIn/ontoenergysystem:hasLocalAuthorityCode ?Area_LACode .
    ?Area <http://publishmydata.com/def/ontology/foi/code> ?Area_LACode . 
    ?Total_ele_consumption ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?v_TotalELecConsumption .
    FILTER NOT EXISTS { ?Area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/E12> . }  
    FILTER NOT EXISTS { ?Area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/E13> . }  
    FILTER NOT EXISTS { ?Area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/W92> . }
    FILTER NOT EXISTS { ?Area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/S92> . }
    FILTER NOT EXISTS { ?Area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/N92> . }
    FILTER NOT EXISTS { ?Area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/K03> . }
    ## FILTER NOT EXISTS { ?Area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/K02> . }
    FILTER NOT EXISTS { ?Area rdfs:label 'K02000001' . }
    
    OPTIONAL { ?Area a <http://statistics.data.gov.uk/def/statistical-geography#Statistical-Geography> .
    ?Area ont:hasGeometry ?geometry . 
    ?geometry ont:asWKT ?Geo_Info . }      
    
    }GROUP BY ?Area_LACode ?v_TotalELecConsumption
    """% (startTime_of_EnergyConsumption)
    
    print('...Query ElectricityConsumption_LocalArea...')
    res = json.loads(performFederatedQuery(queryStr, [UKDigitalTwinEndPoint_iri, ONSEndPoint_iri])) 
    print('...Query ElectricityConsumption_LocalArea is done...')

    toBeDeletedIndex = []      
    for r in res:
        for key in r.keys():
            if '\"^^' in  r[key] :
                r[key] = (r[key].split('\"^^')[0]).replace('\"','') 
        r['v_TotalELecConsumption'] = float(r['v_TotalELecConsumption'])
        if r["Area_LACode"] in ["K03000001", "K02000001", "W92000004","S92000003", "E12000001", "E12000002", "E12000003", "E12000004", "E12000005", 
                                "E12000006", "E12000007", "E12000008", "E12000009", "E13000001", "E13000002"]:
            toBeDeletedIndex.append(res.index(r))
    for i in toBeDeletedIndex:
        del res[i]         
    for r in res:
        if len(r["Geo_InfoList"]) == 0:
            raise Exception(r["Area_LACode"], "does't have the geographical attributes.")
        elif "***" in r['Geo_InfoList']:
            r['Geo_InfoList'] = r['Geo_InfoList'].split("***")[0]            
        r['Geo_InfoList'] = loads(r['Geo_InfoList'])
    return res            
    
###############ELine#############
# branchGeometryQueryCreator is developed to constuct a query string used to retrieve the branch's geometry information according to its parallel connection of each branch
def branchGeometryQueryCreator(topologyNodeIRI, branch_voltage_level): 
    PREFIX = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>    
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX ontocape_geometry: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    """
    
    SELECT_CLAUSE = """
    SELECT DISTINCT ?ELineNode ?From_Bus ?To_Bus ?Value_Length_ELine """
    #TODO: change the SELECT_CLAUSE, 20220608why?
    for voltage in branch_voltage_level:
       SELECT_CLAUSE += "?Num_OHL_" + str(voltage) + " "
        
    WHERE_CLAUSE = """
    WHERE
    {
    <%s> ontocape_upper_level_system:isComposedOfSubsystem ?ELineNode .
    ?ELineNode rdf:type ontopowsys_PowSysRealization:ElectricalLine .   
    
    
    ?ELineNode ontocape_network_system:leaves ?From_Bus .
    ?ELineNode ontocape_network_system:enters ?To_Bus .

    ?ELineNode ontocape_geometry:hasShapeRepresentation/ontocape_geometry:has_length ?Length_ELine .
    ?Length_ELine ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Value_Length_ELine .
    """% topologyNodeIRI 
    
    for voltage in branch_voltage_level: 
        OHL = "?OHL_" + str(voltage)
        Num_OHL = "?Num_OHL_" + str(voltage)
        
        WHERE_CLAUSE += """?ELineNode ontocape_upper_level_system:isComposedOfSubsystem %s . 
    %s rdf:type ontopowsys_PowSysRealization:OverheadLine .
    %s ontopowsys_PowSysRealization:hasVoltageLevel "%s" .
    %s ontopowsys_PowSysRealization:hasNumberOfParallelLine %s .    
    """% (OHL, OHL, OHL, voltage, OHL, Num_OHL)

    WHERE_CLAUSE += "} " #ORDER BY ASC(?ELineNode)
        
    queryStr =  PREFIX + SELECT_CLAUSE + WHERE_CLAUSE
    return queryStr    
    
# queryELineTopologicalInformation is developed to perform the query for branch topological information and its geometry information
def queryELineTopologicalInformation(topologyNodeIRI, endpoint):
    #  label = "UK_Topology_" + str(numOfBus) + "_Bus_" + str(numOfBranch) + "_Branch"
    if endpoint == str(EndPointConfigAndBlazegraphRepoLabel.ukdigitaltwin['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.ukdigitaltwin['endpoint_iri'])
    elif parse(endpoint, rule='IRI'):
        endPointIRI = endpoint
    else:
        raiseExceptions("!!!!Please provide a valid endpoint!!!!")
    
    query_branch_voltage_level = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>    
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX ontocape_geometry: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    SELECT DISTINCT ?OHL_voltage_level
    WHERE
    {
    <%s> ontocape_upper_level_system:isComposedOfSubsystem ?ELineNode .
    ?ELineNode rdf:type ontopowsys_PowSysRealization:ElectricalLine .   
    ?ELineNode ontocape_upper_level_system:isComposedOfSubsystem ?OHL . 
    ?OHL rdf:type ontopowsys_PowSysRealization:OverheadLine .
    ?OHL ontopowsys_PowSysRealization:hasVoltageLevel ?OHL_voltage_level .   
    }
    """ %topologyNodeIRI
    
    print('...Query the branch_voltage_level...')
    res = json.loads(performQuery(endPointIRI, query_branch_voltage_level))
    print('...Branch_voltage_level query is done...')
    branch_voltage_level =  [str(r['OHL_voltage_level']) for r in res]
    queryStr = branchGeometryQueryCreator(topologyNodeIRI, branch_voltage_level)
    print('...Query Branch Geometry...')
    res = json.loads(performQuery(endPointIRI, queryStr))
    print('...branchGeometryQuery is done...')
    for r in res:
        for key in r.keys():
            if '\"^^' in  r[key] :
                r[key] = (r[key].split('\"^^')[0]).replace('\"','') 
    return res, branch_voltage_level 

if __name__ == '__main__': 
    ONS_json = "http://statistics.data.gov.uk/sparql.json"
    ukdigitaltwinendpoint = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test2/sparql"
    ukdigitaltwinendpointLable = "ukdigitaltwin_test2"
    topologyNodeIRI = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerGridTopology_6017554a-98bb-4896-bc21-e455cb6b3958" 
    res = queryBusTopologicalInformation(topologyNodeIRI, "http://localhost:3838/blazegraph/namespace/ukdigitaltwin_test1/sparql")
    #res = queryTotalElecConsumptionofGBOrUK( ukdigitaltwinendpointLable, topologyNodeIRI, "2017-01-31")
    #res = queryPowerSystemLocation(ukdigitaltwinendpointLable, topologyNodeIRI)

    # res = queryRegionalElecConsumption('ukdigitaltwin', 10, "2017-01-31", None, False)
    res = queryElectricityConsumption_Region("2017-01-31", ukdigitaltwinendpoint, ONS_json)
    # res = queryElectricityConsumption_LocalArea("2017-01-31", ukdigitaltwinendpoint, ONS_json)
    # res, a = queryELineTopologicalInformation(29, 99, 'ukdigitaltwin', None, False)
    # res, a = queryELineTopologicalInformation(10, 14, 'ukdigitaltwin', None, False)
    # res = branchGeometryQueryCreator('10', ['275kV', '400kV'])
    # SleepycatStoragePath = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\Top_node\\Sleepycat_topnode"
    
    # res = queryEBusandRegionalDemand(10, None, False, "ukdigitaltwin")
    # geo = res[0]['Geo_InfoList']
    #print(geo.geom_type)   
    
    print(res)
    # print(len(res)) 