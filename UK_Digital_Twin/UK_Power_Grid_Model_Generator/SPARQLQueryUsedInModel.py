"""This module lists out the SPARQL queries used in generating the UK Grid Model A-boxes"""
import os, sys, json
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.queryInterface import performQuery, performFederatedQuery
from UK_Digital_Twin_Package.iris import *
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel as endpointList
from shapely.wkt import loads
from UK_Power_Grid_Topology_Generator.SPARQLQueriesUsedInTopologyABox import queryGBOrNIBoundary
import shapely.geometry
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel
from rfc3987 import parse
from logging import raiseExceptions

#####UPDATED#####

####Bus information query####
def queryBusTopologicalInformation(topologyNodeIRI, endpoint):
    if endpoint == str(EndPointConfigAndBlazegraphRepoLabel.UKPowerSystemBaseWorld['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.UKPowerSystemBaseWorld['endpoint_iri'])
    elif parse(endpoint, rule='IRI'):
        endPointIRI = endpoint
    else:
        raiseExceptions("!!!!Please provide a valid ONS_Endpoint!!!!")
    
    queryStr = f"""
                SELECT DISTINCT ?BusNodeIRI ?BusLatLon (GROUP_CONCAT(?Capacity;SEPARATOR = '***') AS ?GenerationLinkedToBusNode)
                WHERE{{
                <{topologyNodeIRI}> <{ONTOCAPE_UPPER_LEVEL_SYSTEM_ISCOMPOSEDOFSUBSYSTEM}> ?BusNodeIRI .
                ?BusNodeIRI <{RDF_TYPE}> <{ONTOPOWSYS_POWSYSREALIZATION_BUSNODE}> .
                ?PowerGenerator <{META_MEDOL_TOPOLOGY_HASOUTPUT}> ?BusNodeIRI .
                ?PowerPlant <{ONTOECAPE_TECHNICAL_SYSTEM_HASREALIZATIONASPECT}> ?PowerGenerator .
                ?PowerPlant <{ONTOECAPE_TECHNICAL_SYSTEM_HASREQUIREMENTSASPECT}> ?pp_capa .
                ?pp_capa <{RDF_TYPE}> <{ONTOEIP_SYSTEM_REQUIREMENT_DESIGNCAPACITY}> .
                ?pp_capa <{ONTOCAPE_UPPER_LEVEL_SYSTEM_HASVALUE}>/<{ONTOCAPE_UPPER_LEVEL_SYSTEM_NUMERICALVALUE}> ?Capacity .
                ?BusNodeIRI <{ONTOENERGYSYSTEM_HASWGS84LATITUDELONGITUDE}> ?BusLatLon .  
                }} GROUP BY ?BusNodeIRI ?BusLatLon

                """
    
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
    if endpoint == str(EndPointConfigAndBlazegraphRepoLabel.UKPowerSystemBaseWorld['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.UKPowerSystemBaseWorld['endpoint_iri'])
    elif parse(endpoint, rule='IRI'):
        endPointIRI = endpoint
    else:
        raiseExceptions("!!!!Please provide a valid ONS_Endpoint!!!!")
    
    queryStr = f"""
        SELECT DISTINCT ?BusNodeIRI ?BusLatLon (GROUP_CONCAT(?Capacity;SEPARATOR = '***') AS ?GenerationLinkedToBusNode)
        WHERE{{
        <{topologyNodeIRI}> <{ONTOCAPE_UPPER_LEVEL_SYSTEM_ISCOMPOSEDOFSUBSYSTEM}> ?BusNodeIRI .
        ?BusNodeIRI <{RDF_TYPE}> <{ONTOPOWSYS_POWSYSREALIZATION_BUSNODE}> .
        ?BusNodeIRI <{ONTOENERGYSYSTEM_HASWGS84LATITUDELONGITUDE}> ?BusLatLon .
        }} 
        """
    
    print('...remoteQuery queryBusTopologicalInformation...')
    res = json.loads(performQuery(endPointIRI, queryStr))
    print('...queryBusTopologicalInformation is done...')
    
    for r in res:
        r['BusLatLon'] = [float(r['BusLatLon'].split('#')[0]), float(r['BusLatLon'].split('#')[1])]    
    return res 

####EGen information query####
def queryEGenInfo(topologyNodeIRI, endPoint, eliminateClosedPlantIRIList:list):
    if endPoint == str(EndPointConfigAndBlazegraphRepoLabel.UKPowerSystemBaseWorld['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.UKPowerSystemBaseWorld['endpoint_iri'])
    elif parse(endPoint, rule='IRI'):
        endPointIRI = endPoint
    else:
        raiseExceptions("!!!!Please provide a valid endpoint!!!!")

    if len(eliminateClosedPlantIRIList) > 0:
        NotIncludeStr = "FILTER NOT EXISTS { "
        for ppiri in eliminateClosedPlantIRIList:
            if '<' and '>' not in ppiri:
               ppiri = '<' + ppiri + '>'
            conditionStr = f"{{ {ppiri} <{ONTOECAPE_TECHNICAL_SYSTEM_HASREALIZATIONASPECT}> ?PowerGenerator }} UNION"
            NotIncludeStr += conditionStr
        NotIncludeStr = NotIncludeStr[:-5]
        NotIncludeStr += "}"                                         
    else:
        NotIncludeStr = ""

    queryStr = f"""
    SELECT DISTINCT ?PowerGenerator ?FixedMO ?VarMO ?FuelCost ?CO2EmissionFactor ?Bus ?Capacity ?PrimaryFuel ?LatLon ?PowerPlant_LACode ?GenerationTech
    WHERE
    {{
    ?GBElectricitySystemIRI <{ONTOCAPE_UPPER_LEVEL_SYSTEM_CONTAINS}> ?PowerPlant .
    ?GBElectricitySystemIRI <{ONTOENERGYSYSTEM_HASRELEVANTPLACE}>/<{OWL_SAMEAS}> <https://dbpedia.org/page/Great_Britain> .

    <{topologyNodeIRI}> <{ONTOCAPE_UPPER_LEVEL_SYSTEM_ISCOMPOSEDOFSUBSYSTEM}> ?PowerGenerator . 
    <{topologyNodeIRI}> <{ONTOCAPE_UPPER_LEVEL_SYSTEM_ISCOMPOSEDOFSUBSYSTEM}> ?Bus . 
    
    ?PowerGenerator <{META_MEDOL_TOPOLOGY_HASOUTPUT}> ?Bus .
    ?Bus <{RDF_TYPE}> <{ONTOPOWSYS_POWSYSREALIZATION_BUSNODE}> .  
    ?PowerGenerator <{RDF_TYPE}> <{ONTOEIP_POWERPLANT_POWERGENERATOR}> . 
    
    ?PowerGenerator <{ONTOPOWSYS_POWSYSPERFORMANCE_HASFIXEDMAINTENANCECOST}>/<{ONTOCAPE_UPPER_LEVEL_SYSTEM_HASVALUE}> ?v_FixedMO .
    ?v_FixedMO <{ONTOCAPE_UPPER_LEVEL_SYSTEM_NUMERICALVALUE}> ?FixedMO .
    
    ?PowerGenerator <{ONTOPOWSYS_POWSYSPERFORMANCE_HASCOST}>/<{ONTOCAPE_UPPER_LEVEL_SYSTEM_HASVALUE}> ?v_VarMO .
    ?v_VarMO <{ONTOCAPE_UPPER_LEVEL_SYSTEM_NUMERICALVALUE}> ?VarMO .
    
    ?PowerGenerator <{ONTOPOWSYS_POWSYSPERFORMANCE_HASFUELCOST}>/ <{ONTOCAPE_UPPER_LEVEL_SYSTEM_HASVALUE}> ?v_FuelCost .
    ?v_FuelCost <{ONTOCAPE_UPPER_LEVEL_SYSTEM_NUMERICALVALUE}> ?FuelCost .
    
    ?PowerGenerator <{ONTOEIP_POWERPLANT_HASEMISSIONFACTOR}>/<{ONTOCAPE_UPPER_LEVEL_SYSTEM_HASVALUE}> ?v_CO2EmissionFactor .
    ?v_CO2EmissionFactor <{ONTOCAPE_UPPER_LEVEL_SYSTEM_NUMERICALVALUE}> ?CO2EmissionFactor .
    
    ?PowerPlant <{ONTOECAPE_TECHNICAL_SYSTEM_HASREALIZATIONASPECT}> ?PowerGenerator .
    ?PowerPlant <{ONTOECAPE_TECHNICAL_SYSTEM_HASREQUIREMENTSASPECT}> ?pp_capa .
    ?pp_capa <{RDF_TYPE}> <{ONTOEIP_SYSTEM_REQUIREMENT_DESIGNCAPACITY}> .
    ?pp_capa <{ONTOCAPE_UPPER_LEVEL_SYSTEM_HASVALUE}>/<{ONTOCAPE_UPPER_LEVEL_SYSTEM_NUMERICALVALUE}> ?Capacity .
    
    ?PowerGenerator <{ONTOECAPE_TECHNICAL_SYSTEM_REALIZES}>/<{ONTOEIP_POWERPLANT_CONSUMESPRIMARYFUEL}>/<{RDF_TYPE}> ?PrimaryFuel .

    ?PowerGenerator <{ONTOECAPE_TECHNICAL_SYSTEM_REALIZES}>/<{ONTOEIP_POWERPLANT_USESGENERATIONTECHNOLOGY}>/<{RDF_TYPE}> ?GenerationTech .

    ?PowerPlant <{ONTOECAPE_TECHNICAL_SYSTEM_HASREALIZATIONASPECT}> ?PowerGenerator . 
    ?PowerPlant <{ONTOENERGYSYSTEM_HASWGS84LATITUDELONGITUDE}> ?LatLon .

    ?PowerPlant <{ONTOENERGYSYSTEM_HASRELEVANTPLACE}>/<{ONTOENERGYSYSTEM_HASLOCALAUTHORITYCODE}> ?PowerPlant_LACode .

    {NotIncludeStr}

    }}
    """
    
    print('...starts queryEGenInfo...')
    res = json.loads(performQuery(endPointIRI, queryStr))
    qres = [[ str(r['PowerGenerator']), float((r['FixedMO'].split('\"^^')[0]).replace('\"','')), float((r['VarMO'].split('\"^^')[0]).replace('\"','')), \
                float((r['FuelCost'].split('\"^^')[0]).replace('\"','')), float((r['CO2EmissionFactor'].split('\"^^')[0]).replace('\"','')), str(r['Bus']), \
                float((r['Capacity'].split('\"^^')[0]).replace('\"','')), (str(r['PrimaryFuel']).split('#'))[1], \
                [float(r['LatLon'].split('#')[0]), float(r['LatLon'].split('#')[1])], str(r['PowerPlant_LACode']), str(r['GenerationTech'])] for r in res]
    print('...finishes queryEGenInfo...')
    return qres 
    
# query the total electricity consumption of a UK official region 
def queryTotalElecConsumptionofGBOrUK(endPoint_label, topologyNodeIRI, startTime_of_EnergyConsumption):
    if endPoint_label == str(EndPointConfigAndBlazegraphRepoLabel.UKPowerSystemBaseWorld['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.UKPowerSystemBaseWorld['endpoint_iri'])
    elif parse(endPoint_label, rule='IRI'):
        endPointIRI = endPoint_label
    else:
        raiseExceptions("!!!!Please provide a valid endpoint!!!!")

    queryStr_BusAndLatlon = f"""
    SELECT DISTINCT ?Bus_node ?Bus_lat_lon
    WHERE 
    {{   
    <{topologyNodeIRI}> <{ONTOCAPE_UPPER_LEVEL_SYSTEM_ISCOMPOSEDOFSUBSYSTEM}> ?Bus_node .
    ?Bus_node   <{RDF_TYPE}> <{ONTOPOWSYS_POWSYSREALIZATION_BUSNODE}> ; 
                <{ONTOENERGYSYSTEM_HASWGS84LATITUDELONGITUDE}> ?Bus_lat_lon .
    }}"""
    
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
    
    queryStr_electricity_consumption = f"""
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    SELECT DISTINCT  ?v_TotalELecConsumption
    WHERE 
    {{   
    ?Total_ele_consumption <{ONTOCAPE_DERIVED_SI_UNITS_HASTIMEPERIOD}>/<{ONTOCAPE_UPPER_LEVEL_SYSTEM_HASVALUE}> ?TimePeriod .
    ?TimePeriod <{ONTOECAPE_SPACE_AND_TIME_HASSTARTINGTIME}> ?startTime .
    ?startTime <{RDF_TYPE}> <{ONTOCAPE_COORDINATE_SYSTEM_COORDINATEVALUE}> . 
    ?startTime <{ONTOCAPE_UPPER_LEVEL_SYSTEM_NUMERICALVALUE}> "{startTime_of_EnergyConsumption}"^^xsd:dateTime .
    
    ?Total_ele_consumption <{ONTOENERGYSYSTEM_ISOBSERVEDIN}>/<{ONTOENERGYSYSTEM_HASLOCALAUTHORITYCODE}> "{query_Area}" .
    ?Total_ele_consumption <{ONTOCAPE_UPPER_LEVEL_SYSTEM_HASVALUE}>/<{ONTOCAPE_UPPER_LEVEL_SYSTEM_NUMERICALVALUE}> ?v_TotalELecConsumption .
    }}
    """
    
    print('remoteQuery electricity_consumption')
    res_electricity_consumption = json.loads(performQuery(endPointIRI, queryStr_electricity_consumption))
    print('query of electricity_consumption is done')
    if str(res_electricity_consumption) == '[]':
        raise Exception('Cannot find the total consumtion of the area', query_Area)
    res = float(res_electricity_consumption[0]['v_TotalELecConsumption']) 
    return res

# Query the located country of the Power System
def queryPowerSystemLocation(endpoint_label, topologyNodeIRI):
    queryStr = f"""
    SELECT DISTINCT ?Location
    WHERE
    {{
    <{topologyNodeIRI}> <{ONTOENERGYSYSTEM_ISTOPOLOGYOF}>/<{ONTOCAPE_UPPER_LEVEL_SYSTEM_ISDIRECTSUBSYSTEMOF}> ?ElectricityPowerSystem .
    ?ElectricityPowerSystem <{ONTOENERGYSYSTEM_HASRELEVANTPLACE}>/<{OWL_SAMEAS}> ?Location .
    }}
    """
    print('...starts queryPowerSystemLocation...')
    res = json.loads(performQuery(endpoint_label, queryStr))
    qres = str(res[0]['Location'])
    print('...finishes queryPowerSystemLocation...')
    return qres

###############EBus#############
# Query the total consumption of the regions
## `ifQueryONSOrigionalEndpoint` determines the endpoint of querying ONS data. 
## If `ifQueryONSOrigionalEndpoint` set as `True`, the original ONS endpoint is used; when set as `False`, the subset of ONS deployed on CMCL Blazegraph is used. 
def queryElectricityConsumption_Region(startTime_of_EnergyConsumption, UKPowerSystemBaseWorldEndPoint_iri, ONSEndPoint_iri, ifQueryONSOrigionalEndpoint:bool = False):
    if ONSEndPoint_iri == str(EndPointConfigAndBlazegraphRepoLabel.ONS['label']):
        ONSEndPoint_iri = str(EndPointConfigAndBlazegraphRepoLabel.ONS['endpoint_iri'])
    elif not parse(ONSEndPoint_iri, rule='IRI'):
        raiseExceptions("!!!!Please provide a valid query endpoint!!!!")

    if ifQueryONSOrigionalEndpoint is False: 
        queryStr = f"""
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        SELECT DISTINCT ?RegionOrCountry_LACode ?v_TotalELecConsumption
        WHERE 
        {{   
        ?Total_ele_consumption <{ONTOCAPE_DERIVED_SI_UNITS_HASTIMEPERIOD}>/<{ONTOCAPE_UPPER_LEVEL_SYSTEM_HASVALUE}> ?TimePeriod .
        ?TimePeriod <{ONTOECAPE_SPACE_AND_TIME_HASSTARTINGTIME}> ?startTime .
        ?startTime <{RDF_TYPE}> <{ONTOCAPE_COORDINATE_SYSTEM_COORDINATEVALUE}> . 
        ?startTime <{ONTOCAPE_UPPER_LEVEL_SYSTEM_NUMERICALVALUE}> "{startTime_of_EnergyConsumption}"^^xsd:dateTime .
        ?Total_ele_consumption <{ONTOENERGYSYSTEM_ISOBSERVEDIN}>/<{ONTOENERGYSYSTEM_HASLOCALAUTHORITYCODE}> ?RegionOrCountry_LACode .
        ?Total_ele_consumption <{ONTOCAPE_UPPER_LEVEL_SYSTEM_HASVALUE}>/<{ONTOCAPE_UPPER_LEVEL_SYSTEM_NUMERICALVALUE}> ?v_TotalELecConsumption .
        
        SERVICE <{ONSEndPoint_iri}> {{
            ?RegionOrCountry <http://publishmydata.com/def/ontology/foi/code> ?RegionOrCountry_LACode .
            {{ ?RegionOrCountry <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/E12> .}} UNION 
            {{ ?RegionOrCountry <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/W92> .}} UNION
            {{ ?RegionOrCountry <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/S92> .}} UNION
            {{ ?RegionOrCountry <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/N92> .}}     
            }}
        }}"""

        print('...starts queryElectricityConsumption_Region...')   
        res = json.loads(performQuery(UKPowerSystemBaseWorldEndPoint_iri, queryStr))     
        print('...queryElectricityConsumption_Region is done...') 
    else:
        queryStr = f"""
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        SELECT DISTINCT ?RegionOrCountry_LACode ?v_TotalELecConsumption
        WHERE 
        {{   
        ?Total_ele_consumption <{ONTOCAPE_DERIVED_SI_UNITS_HASTIMEPERIOD}>/<{ONTOCAPE_UPPER_LEVEL_SYSTEM_HASVALUE}> ?TimePeriod .
        ?TimePeriod <{ONTOECAPE_SPACE_AND_TIME_HASSTARTINGTIME}> ?startTime .
        ?startTime <{RDF_TYPE}> <{ONTOCAPE_COORDINATE_SYSTEM_COORDINATEVALUE}> . 
        ?startTime <{ONTOCAPE_UPPER_LEVEL_SYSTEM_NUMERICALVALUE}> "{startTime_of_EnergyConsumption}"^^xsd:dateTime .
        
        ?Total_ele_consumption <{ONTOENERGYSYSTEM_ISOBSERVEDIN}>/<{ONTOENERGYSYSTEM_HASLOCALAUTHORITYCODE}> ?RegionOrCountry_LACode .
        ?RegionOrCountry <http://publishmydata.com/def/ontology/foi/code> ?RegionOrCountry_LACode .
        {{ ?RegionOrCountry <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/E12> .}} UNION 
        {{ ?RegionOrCountry <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/W92> .}} UNION
        {{ ?RegionOrCountry <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/S92> .}} UNION
        {{ ?RegionOrCountry <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/N92> .}}
        ?Total_ele_consumption <{ONTOCAPE_UPPER_LEVEL_SYSTEM_HASVALUE}>/<{ONTOCAPE_UPPER_LEVEL_SYSTEM_NUMERICALVALUE}> ?v_TotalELecConsumption .
        }}
        """
     
        print('...starts queryElectricityConsumption_Region...')   
        res = json.loads(performFederatedQuery(queryStr, [UKPowerSystemBaseWorldEndPoint_iri, ONSEndPoint_iri]))
        print('...queryElectricityConsumption_Region is done...') 

    for r in res:
        for key in r.keys():
           if '\"^^' in  r[key] :
             r[key] = (r[key].split('\"^^')[0]).replace('\"','') 
        r['v_TotalELecConsumption'] = float(r['v_TotalELecConsumption'])
    return res                  
         
# query the total electricity consumption of each address area
def queryElectricityConsumption_LocalArea(startTime_of_EnergyConsumption, UKPowerSystemBaseWorldEndPoint_iri, ONSEndPoint_iri):    
    if ONSEndPoint_iri == str(EndPointConfigAndBlazegraphRepoLabel.UKPowerSystemBaseWorld['label']):
        ONSEndPoint_iri = str(EndPointConfigAndBlazegraphRepoLabel.UKPowerSystemBaseWorld['endpoint_iri'])
    elif not parse(ONSEndPoint_iri, rule='IRI'):
        raiseExceptions("!!!!Please provide a valid query endpoint!!!!")

    queryStr = f"""
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    SELECT DISTINCT ?Area_LACode ?v_TotalELecConsumption (GROUP_CONCAT(?Geo_Info;SEPARATOR = '***') AS ?Geo_InfoList)
    WHERE 
    {{   
    ?Total_ele_consumption <{ONTOCAPE_DERIVED_SI_UNITS_HASTIMEPERIOD}>/<{ONTOCAPE_UPPER_LEVEL_SYSTEM_HASVALUE}> ?TimePeriod .
    ?TimePeriod <{ONTOECAPE_SPACE_AND_TIME_HASSTARTINGTIME}> ?startTime .
    ?startTime <{RDF_TYPE}> <{ONTOCAPE_COORDINATE_SYSTEM_COORDINATEVALUE}> . 
    ?startTime <{ONTOCAPE_UPPER_LEVEL_SYSTEM_NUMERICALVALUE}> "{startTime_of_EnergyConsumption}"^^xsd:dateTime .
    
    ?Total_ele_consumption <{ONTOENERGYSYSTEM_ISOBSERVEDIN}>/<{ONTOENERGYSYSTEM_HASLOCALAUTHORITYCODE}> ?Area_LACode .
    ?Area <http://publishmydata.com/def/ontology/foi/code> ?Area_LACode . 
    ?Total_ele_consumption <{ONTOCAPE_UPPER_LEVEL_SYSTEM_HASVALUE}>/<{ONTOCAPE_UPPER_LEVEL_SYSTEM_NUMERICALVALUE}> ?v_TotalELecConsumption .
    FILTER NOT EXISTS {{ ?Area <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/E12> . }}  
    FILTER NOT EXISTS {{ ?Area <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/E13> . }}  
    FILTER NOT EXISTS {{ ?Area <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/W92> . }}
    FILTER NOT EXISTS {{ ?Area <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/S92> . }}
    FILTER NOT EXISTS {{ ?Area <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/N92> . }}
    FILTER NOT EXISTS {{ ?Area <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/K03> . }}
    ## FILTER NOT EXISTS {{ ?Area <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/K02> . }}
    FILTER NOT EXISTS {{ ?Area <{RDFS_LABEL}> 'K02000001' . }}
    
    OPTIONAL {{ ?Area a <http://statistics.data.gov.uk/def/statistical-geography#Statistical-Geography> .
    ?Area <{ONS_GEOSPARQL_HASGEOMETRY}> ?geometry . 
    ?geometry <{ONS_GEOSPARQL_ASWKT}> ?Geo_Info . }}      
    
    }}GROUP BY ?Area_LACode ?v_TotalELecConsumption
    """
    
    print('...Query ElectricityConsumption_LocalArea...')
    res = json.loads(performFederatedQuery(queryStr, [UKPowerSystemBaseWorldEndPoint_iri, ONSEndPoint_iri])) 
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
    ADDED = f""""""
    for voltage in branch_voltage_level: 
        OHL = "?OHL_" + str(voltage)
        Num_OHL = "?Num_OHL_" + str(voltage)
        
        ADDED += f"""?ELineNode <{ONTOCAPE_UPPER_LEVEL_SYSTEM_ISCOMPOSEDOFSUBSYSTEM}> {OHL} . 
    {OHL} <{RDF_TYPE}> <{ONTOPOWSYS_POWSYSREALIZATION_OVERHEADLINE}> .
    {OHL} <{ONTOPOWSYS_POWSYSREALIZATION_HASVOLTAGELEVEL}> "{voltage}" .
    {OHL} <{ONTOPOWSYS_POWSYSREALIZATION_HASNUMBEROFPARALLELLINE}> {Num_OHL} .    
    """
    SELECT_CLAUSE = f"""
    SELECT DISTINCT ?ELineNode ?From_Bus ?To_Bus ?Value_Length_ELine """
    for voltage in branch_voltage_level:
       SELECT_CLAUSE += "?Num_OHL_" + str(voltage) + " "
        
    WHERE_CLAUSE = f"""
    WHERE
    {{
    <{topologyNodeIRI}> <{ONTOCAPE_UPPER_LEVEL_SYSTEM_ISCOMPOSEDOFSUBSYSTEM}> ?ELineNode .
    ?ELineNode <{RDF_TYPE}> <{ONTOPOWSYS_POWSYSREALIZATION_ELECTRICALLINE}> .   
    
    
    ?ELineNode <{ONTOCAPE_NETWORK_SYSTEM_LEAVES}> ?From_Bus .
    ?ELineNode <{ONTOCAPE_NETWORK_SYSTEM_ENTERS}> ?To_Bus .

    ?ELineNode <{ONTOCAPE_GEOMETRY_HASSHAPEREPRESENTATION}>/<{ONTOCAPE_GEOMETRY_HAS_LENGTH}> ?Length_ELine .
    ?Length_ELine <{ONTOCAPE_UPPER_LEVEL_SYSTEM_HASVALUE}>/<{ONTOCAPE_UPPER_LEVEL_SYSTEM_NUMERICALVALUE}> ?Value_Length_ELine .

    {ADDED}
    }}""" 
    queryStr =  SELECT_CLAUSE + WHERE_CLAUSE
    print(queryStr)
    return queryStr    
    
# queryELineTopologicalInformation is developed to perform the query for branch topological information and its geometry information
def queryELineTopologicalInformation(topologyNodeIRI, endpoint):
    #  label = "UK_Topology_" + str(numOfBus) + "_Bus_" + str(numOfBranch) + "_Branch"
    if endpoint == str(EndPointConfigAndBlazegraphRepoLabel.UKPowerSystemBaseWorld['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.UKPowerSystemBaseWorld['endpoint_iri'])
    elif parse(endpoint, rule='IRI'):
        endPointIRI = endpoint
    else:
        raiseExceptions("!!!!Please provide a valid endpoint!!!!")
    
    query_branch_voltage_level = f"""
    SELECT DISTINCT ?OHL_voltage_level
    WHERE
    {{
    <{topologyNodeIRI}> <{ONTOCAPE_UPPER_LEVEL_SYSTEM_ISCOMPOSEDOFSUBSYSTEM}> ?ELineNode .
    ?ELineNode <{RDF_TYPE}> <{ONTOPOWSYS_POWSYSREALIZATION_ELECTRICALLINE}> .   
    ?ELineNode <{ONTOCAPE_UPPER_LEVEL_SYSTEM_ISCOMPOSEDOFSUBSYSTEM}> ?OHL . 
    ?OHL <{RDF_TYPE}> <{ONTOPOWSYS_POWSYSREALIZATION_OVERHEADLINE}> .
    ?OHL <{ONTOPOWSYS_POWSYSREALIZATION_HASVOLTAGELEVEL}> ?OHL_voltage_level .   
    }}
    """
    
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