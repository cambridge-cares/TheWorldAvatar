##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 11 Sept 2023         #
##########################################

import os, sys, json
BASE = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.queryInterface import performQuery, performFederatedQuery
from UK_Digital_Twin_Package.OWLfileStorer import readFile
from UK_Digital_Twin_Package import CO2FactorAndGenCostFactor as ModelFactor
from shapely.wkt import loads
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel
from rfc3987 import parse
from logging import raiseExceptions

"""Create an object of Class CO2FactorAndGenCostFactor"""
ukmf = ModelFactor.ModelFactor()

"""Model Parameter Array"""
modelFactorArrays = readFile(ukmf.CO2EmissionFactorAndCostFactor)

def queryEliminatePowerPlant(plantNameList:list, endpoint):
    ppList = []
    for ppName in plantNameList:
        queryStr = """
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
        SELECT DISTINCT ?powerPlantIRI
        WHERE
        {
        ?powerPlantIRI rdfs:label "%s" .
        ?powerPlantIRI rdf:type ontoeip_powerplant:PowerPlant .
        }
        """%ppName

        print('...starts queryEliminatePowerPlant...')
        ppList.append(json.loads(performQuery(endpoint, queryStr))[0]['powerPlantIRI'])
        print('...finishes queryEliminatePowerPlant...')
    return ppList

def queryTopologyIRI(numOfBus, numOfBranch, endpoint):

    label = "UK_Topology_" + str(numOfBus) + "_Bus_" + str(numOfBranch) + "_Branch"

    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ontoenergysystem: <http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#>
    SELECT DISTINCT ?topologyIRI
    WHERE
    {
    ?topologyIRI rdfs:label "%s" .
    ?topologyIRI rdf:type ontoenergysystem:PowerGridTopology .
    }
    """% label

    print('...starts queryTopologyIRI...')
    res = json.loads(performQuery(endpoint, queryStr))[0]['topologyIRI']
    print('...finishes queryTopologyIRI...')
    return res

def querySlackBusNode(slackBusLocation:str, endpoint):

    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
    PREFIX ontoenergysystem: <http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#>
    SELECT DISTINCT ?slackBusNodeIRI ?latlon
    WHERE
    {
    ?slackBusNodeIRI ontoenergysystem:hasWGS84LatitudeLongitude ?latlon .
    ?slackBusNodeIRI rdf:type ontopowsys_PowSysRealization:BusNode .
    }
    """

    print('...starts querySlackBusNode...')
    res = json.loads(performQuery(endpoint, queryStr))
    print('...finishes querySlackBusNode...')
    for r in res:
        if r['latlon'] in slackBusLocation:
            slackBus = r['slackBusNodeIRI']
            break
    return slackBus

def queryGeneratorToBeRetrofitted_AllPowerPlant(topologyNodeIRI:str, endPoint_label):
    results = []
    queryStr = """
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
    SELECT DISTINCT ?PowerGenerator ?Bus ?Capacity ?LatLon ?FuelType ?GenerationTechnology ?place
    WHERE
    {
    ?GBElectricitySystemIRI ontocape_upper_level_system:contains ?PowerPlant .
    ?GBElectricitySystemIRI ontoenergysystem:hasRelevantPlace/owl:sameAs <https://dbpedia.org/page/Great_Britain> .

    <%s> ontocape_upper_level_system:isComposedOfSubsystem ?PowerGenerator . 
    <%s> ontocape_upper_level_system:isComposedOfSubsystem ?Bus . 
    
    ?PowerGenerator meta_model_topology:hasOutput ?Bus .
    ?Bus rdf:type ontopowsys_PowSysRealization:BusNode .  
    ?PowerGenerator rdf:type ontoeip_powerplant:PowerGenerator . 

    ?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:usesGenerationTechnology ?GenerationTechnologyIRI .
    ?GenerationTechnologyIRI rdf:type ?GenerationTechnology .
    
    ?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:consumesPrimaryFuel ?FuelTypeIRI .
    ?FuelTypeIRI rdf:type ?FuelType . 
    
    FILTER NOT EXISTS { ?GenerationTechnologyIRI rdf:type <http://www.theworldavatar.com/kb/ontoeip/WindOffshore> .}  
    
    ?PowerPlant ontocape_technical_system:hasRealizationAspect ?PowerGenerator .
    ?PowerPlant ontocape_technical_system:hasRequirementsAspect ?pp_capa .
    ?pp_capa rdf:type ontoeip_system_requirement:DesignCapacity .
    ?pp_capa ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Capacity .

    ?PowerPlant ontocape_technical_system:hasRealizationAspect ?PowerGenerator . 
    ?PowerPlant ontoenergysystem:hasWGS84LatitudeLongitude ?LatLon .  
    ?PowerPlant ontoenergysystem:hasRelevantPlace/owl:sameAs ?place . 
    }
    """% (topologyNodeIRI, topologyNodeIRI)

    print('...starts queryGeneratorToBeRetrofitted_AllPowerPlant...')
    res = json.loads(performQuery(endPoint_label, queryStr))
    print('...finishes queryGeneratorToBeRetrofitted_AllPowerPlant...')

    for r in res: 
        fuelType = r["GenerationTechnology"] if "Wind" in r["FuelType"] else r["FuelType"]       
        if "#" in fuelType:
            fuelOrGenType = str(type.split('#')[1])
        else:
            fuelOrGenType = str(type) 

        if fuelOrGenType in ukmf.Nuclear:
            CO2EmissionFactor = float(modelFactorArrays[2][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.Bio:
            CO2EmissionFactor = float(modelFactorArrays[3][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.Coal: 
            CO2EmissionFactor = float(modelFactorArrays[4][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.Natural: 
            CO2EmissionFactor = float(modelFactorArrays[5][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.Oil: 
            CO2EmissionFactor = float(modelFactorArrays[7][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.Solar:  
            CO2EmissionFactor = float(modelFactorArrays[8][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.Hydro:  
            CO2EmissionFactor = float(modelFactorArrays[9][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.PumpHydro:  
            CO2EmissionFactor = float(modelFactorArrays[10][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.WindOnshore:  
            CO2EmissionFactor = float(modelFactorArrays[11][4].replace('\n', ''))  
        elif fuelOrGenType in ukmf.WindOffshore:  
            CO2EmissionFactor = float(modelFactorArrays[12][4].replace('\n', ''))       
        elif fuelOrGenType in ukmf.Waste:  
            CO2EmissionFactor = float(modelFactorArrays[13][4].replace('\n', ''))       
        else:
            CO2EmissionFactor = float(modelFactorArrays[14][4].replace('\n', ''))  

        arranged_res = {
                    "PowerGenerator": r["PowerGenerator"],
                    "Bus": r["Bus"],
                    "Capacity": r["Capacity"],
                    "LatLon": [float(r['LatLon'].split('#')[0]), float(r['LatLon'].split('#')[1])],
                    "fuelOrGenType": fuelType,
                    "CO2EmissionFactor": CO2EmissionFactor,
                    "place": r["place"]
                    }
        results.append(arranged_res)
    return results 

def queryGeneratorToBeRetrofitted_SelectedGenerator(retrofitGenerator:list, endPoint_label):
    results = []
    for gen in retrofitGenerator:
        queryStr = """
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
        SELECT DISTINCT ?Bus ?Capacity ?LatLon ?FuelType ?GenerationTechnology ?place
        WHERE
        {
        <%s> meta_model_topology:hasOutput ?Bus .
        ?Bus rdf:type ontopowsys_PowSysRealization:BusNode .  
        <%s> rdf:type ontoeip_powerplant:PowerGenerator . 
        
        ?PowerPlant ontocape_technical_system:hasRealizationAspect <%s> .
        ?PowerPlant ontocape_technical_system:hasRequirementsAspect ?pp_capa .
        ?pp_capa rdf:type ontoeip_system_requirement:DesignCapacity .
        ?pp_capa ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Capacity .
        
        ?PowerPlant ontocape_technical_system:hasRealizationAspect ?PowerGenerator . 
        ?PowerPlant ontoenergysystem:hasWGS84LatitudeLongitude ?LatLon .

        ?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:usesGenerationTechnology ?GenerationTechnologyIRI .
        ?GenerationTechnologyIRI rdf:type ?GenerationTechnology .
        
        ?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:consumesPrimaryFuel ?FuelTypeIRI .
        ?FuelTypeIRI rdf:type ?FuelType . 
        ?PowerPlant ontoenergysystem:hasRelevantPlace/owl:sameAs ?place . 
        
        }
        """% (gen, gen, gen)

        print('...starts queryGeneratorToBeRetrofitted_SelectedGenerator...')
        res = json.loads(performQuery(endPoint_label, queryStr))[0]
        print('...finishes queryGeneratorToBeRetrofitted_SelectedGenerator...')

        fuelType = r["GenerationTechnology"] if "Wind" in r["FuelType"] else r["FuelType"]       
        if "#" in fuelType:
            fuelOrGenType = str(type.split('#')[1])
        else:
            fuelOrGenType = str(type) 

        if fuelOrGenType in ukmf.Nuclear:
            CO2EmissionFactor = float(modelFactorArrays[2][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.Bio:
            CO2EmissionFactor = float(modelFactorArrays[3][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.Coal: 
            CO2EmissionFactor = float(modelFactorArrays[4][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.Natural: 
            CO2EmissionFactor = float(modelFactorArrays[5][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.Oil: 
            CO2EmissionFactor = float(modelFactorArrays[7][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.Solar:  
            CO2EmissionFactor = float(modelFactorArrays[8][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.Hydro:  
            CO2EmissionFactor = float(modelFactorArrays[9][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.PumpHydro:  
            CO2EmissionFactor = float(modelFactorArrays[10][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.WindOnshore:  
            CO2EmissionFactor = float(modelFactorArrays[11][4].replace('\n', ''))  
        elif fuelOrGenType in ukmf.WindOffshore:  
            CO2EmissionFactor = float(modelFactorArrays[12][4].replace('\n', ''))       
        elif fuelOrGenType in ukmf.Waste:  
            CO2EmissionFactor = float(modelFactorArrays[13][4].replace('\n', ''))       
        else:
            CO2EmissionFactor = float(modelFactorArrays[14][4].replace('\n', ''))  
        arranged_res = {
                        "PowerGenerator" : gen,
                        "Bus": res["Bus"],
                        "Capacity": res["Capacity"],
                        "LatLon": [float(res['LatLon'].split('#')[0]), float(res['LatLon'].split('#')[1])],
                        "fuelOrGenType": fuelType,
                        "CO2EmissionFactor": CO2EmissionFactor,
                        "place": r["place"]                   
                        }
        results.append(arranged_res)
    return results 

def queryGeneratorToBeRetrofitted_SelectedFuelOrGenerationTechnologyType(retrofitGenerationOrFuelType:list, topologyNodeIRI:str, endPoint_label):  
    if endPoint_label == str(EndPointConfigAndBlazegraphRepoLabel.UKPowerSystemBaseWorld['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.UKPowerSystemBaseWorld['endpoint_iri'])
    elif parse(endPoint_label, rule='IRI'):
        endPointIRI = endPoint_label
    else:
        raiseExceptions("!!!!Please provide a valid query endpoint!!!!")
    
    ## Assign CO2 emission factors
    results = []
    for type in retrofitGenerationOrFuelType:
        if "#" in type:
            fuelOrGenType = str(type.split('#')[1])
        else:
            fuelOrGenType = str(type) 

        if fuelOrGenType in ukmf.Nuclear:
            CO2EmissionFactor = float(modelFactorArrays[2][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.Bio:
            CO2EmissionFactor = float(modelFactorArrays[3][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.Coal: 
            CO2EmissionFactor = float(modelFactorArrays[4][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.Natural: 
            CO2EmissionFactor = float(modelFactorArrays[5][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.Oil: 
            CO2EmissionFactor = float(modelFactorArrays[7][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.Solar:  
            CO2EmissionFactor = float(modelFactorArrays[8][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.Hydro:  
            CO2EmissionFactor = float(modelFactorArrays[9][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.PumpHydro:  
            CO2EmissionFactor = float(modelFactorArrays[10][4].replace('\n', ''))
        elif fuelOrGenType in ukmf.WindOnshore:  
            CO2EmissionFactor = float(modelFactorArrays[11][4].replace('\n', ''))  
        elif fuelOrGenType in ukmf.WindOffshore:  
            CO2EmissionFactor = float(modelFactorArrays[12][4].replace('\n', ''))       
        elif fuelOrGenType in ukmf.Waste:  
            CO2EmissionFactor = float(modelFactorArrays[13][4].replace('\n', ''))       
        else:
            CO2EmissionFactor = float(modelFactorArrays[14][4].replace('\n', ''))  

        queryStr_1 = """
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
        SELECT DISTINCT ?PowerGenerator ?Bus ?Capacity ?LatLon ?place 
        WHERE
        {
        ?GBElectricitySystemIRI ontocape_upper_level_system:contains ?PowerPlant .
        ?GBElectricitySystemIRI ontoenergysystem:hasRelevantPlace/owl:sameAs <https://dbpedia.org/page/Great_Britain> .

        <%s> ontocape_upper_level_system:isComposedOfSubsystem ?PowerGenerator . 
        <%s> ontocape_upper_level_system:isComposedOfSubsystem ?Bus . 
        
        ?PowerGenerator meta_model_topology:hasOutput ?Bus .
        ?Bus rdf:type ontopowsys_PowSysRealization:BusNode .  
        ?PowerGenerator rdf:type ontoeip_powerplant:PowerGenerator . 

        {?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:consumesPrimaryFuel ?FuelType .
        ?FuelType rdf:type <%s> . } UNION 
        {?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:usesGenerationTechnology ?TechType .
        ?TechType rdf:type <%s> . }

        ?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:usesGenerationTechnology ?GenerationTechnologyIRI .

        FILTER NOT EXISTS { ?GenerationTechnologyIRI rdf:type <http://www.theworldavatar.com/kb/ontoeip/WindOffshore> .}   
        
        ?PowerPlant ontocape_technical_system:hasRealizationAspect ?PowerGenerator .
        ?PowerPlant ontocape_technical_system:hasRequirementsAspect ?pp_capa .
        ?pp_capa rdf:type ontoeip_system_requirement:DesignCapacity .
        ?pp_capa ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Capacity .
        
        ?PowerPlant ontoenergysystem:hasWGS84LatitudeLongitude ?LatLon .
        ?PowerPlant ontoenergysystem:hasRelevantPlace/owl:sameAs ?place .
        }
        """% (topologyNodeIRI, topologyNodeIRI, type, type)

        print('...starts queryGeneratorToBeRetrofitted_SelectedFuelOrGenerationTechnologyType...')
        res = json.loads(performQuery(endPointIRI, queryStr_1))
        print('...finishes queryGeneratorToBeRetrofitted_SelectedFuelOrGenerationTechnologyType...')  
                
        for r in res:
            arranged_res = {
                            "PowerGenerator" : r["PowerGenerator"],
                            "Bus": r["Bus"],
                            "Capacity": r["Capacity"],
                            "LatLon": [float(r['LatLon'].split('#')[0]), float(r['LatLon'].split('#')[1])],
                            "fuelOrGenType": type,
                            "CO2EmissionFactor": CO2EmissionFactor,
                            "place": r["place"]
                            }
            results.append(arranged_res) 
        
    return results

## Query the boundaries of the consumption areas
## Direct Http Request without using the query client from the py4jps
def queryAreaBoundaries(LA_code:str):
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
    PREFIX ons_geosparql: <http://www.opengis.net/ont/geosparql#>
    SELECT DISTINCT ?area (GROUP_CONCAT(?areaBoundary;SEPARATOR = '***') AS ?Geo_InfoList)
    WHERE
    {
    # ?area ons:status "live" .
    ?area rdf:type ons:Statistical-Geography .
    ?area <http://publishmydata.com/def/ontology/foi/code> "%s" .
    ?area ons_geosparql:hasGeometry ?geometry .
    ?geometry ons_geosparql:asWKT ?areaBoundary .
    } GROUP BY ?area
    """% (LA_code)

    ###-- The previous way of send http request to the ONS endpoint as the query interfeace in Py4jps does not work--###
    # encodedString = urllib.parse.quote(queryStr)
    # getString = "http://statistics.data.gov.uk/sparql.json?query=" + str(encodedString)

    # print('... HTTP GET demanding Area Boundaries...')
    # r = requests.get(getString, timeout=60)
    # res = json.loads(r.text)['results']['bindings'][0]['Geo_InfoList']['value']
    # print('...HTTP GET demanding Area Boundaries is done...')

    ###-- with the deployment of the ONS subset on digital ocean --###
    endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.ONS['queryendpoint_iri'])
    print('...starts queryAreaBoundaries...')
    res = json.loads(performQuery(endPointIRI, queryStr))[0]['Geo_InfoList']
    print('...finishes queryAreaBoundaries...')  
    
    ## clear the symbols in the query results
    res = (res.split('\"^^')[0]).replace('\"','') 

    ## Check the availability of the geometry of each area
    if res == 0: 
        raise Exception('There is one place does not have geometry information which is', r["LACode_area"], ', please check the query string and the place status in ONS.')
    elif "***" in res:
        res = res.split("***")[0]
    res = loads(res) # convert wkt into shapely polygons
    return res
     
def queryifWithin(LACode_toBeCheck:str, givenLACode:str, ONS_Endpoint_label:str):
    if ONS_Endpoint_label == str(EndPointConfigAndBlazegraphRepoLabel.ONS['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.ONS['endpoint_iri'])
    elif parse(ONS_Endpoint_label, rule='IRI'):
        endPointIRI = ONS_Endpoint_label
    else:
        raiseExceptions("!!!!Please provide a valid query endpoint!!!!")

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

    print('...query ifWithin condition...')
    res = json.loads(performQuery(endPointIRI, queryStr))  
    print('...queryifWithin is done...')
    res = res[0]['ASK']
    return res

if __name__ == '__main__':   
    # retrofitGenerationFuelOrTechType = ["http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas", 
    #     "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal", 
    #     "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Oil",
    #     "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#SourGas"]

    # res = queryAreaBoundaries("E92000001") # ("E07000066")

    res = queryifWithin ("E07000066", "K02000001", "ons")


    qsrt= """
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
	PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
    PREFIX ons_geosparql: <http://www.opengis.net/ont/geosparql#>
    SELECT DISTINCT ?PowerPlant ?LatLon ?PowerPlant_LACode (GROUP_CONCAT(?areaBoundary;SEPARATOR = '***') AS ?Geo_InfoList)
    WHERE
    {
    ?GBElectricitySystemIRI ontocape_upper_level_system:contains ?PowerPlant .
    ?GBElectricitySystemIRI ontoenergysystem:hasRelevantPlace/owl:sameAs <https://dbpedia.org/page/Great_Britain> .
    ?PowerPlant ontoenergysystem:hasWGS84LatitudeLongitude ?LatLon .
    ?PowerPlant ontoenergysystem:hasRelevantPlace/ontoenergysystem:hasLocalAuthorityCode ?PowerPlant_LACode .
	
	?area ons:status "live" .
    ?area rdf:type ons:Statistical-Geography .
    ?area <http://publishmydata.com/def/ontology/foi/code> ?PowerPlant_LACode .
    ?area ons_geosparql:hasGeometry ?geometry .
    ?geometry ons_geosparql:asWKT ?areaBoundary .


    }GROUP BY ?PowerPlant_LACode
    """
    res = json.loads(performFederatedQuery(qsrt, ["http://statistics.data.gov.uk/sparql.json","http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/UKPowerSystemBaseWorld_test2/sparql"]))


    
    retrofitGenerationFuelOrTechType = ["http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas"]
    ocgt = "http://www.theworldavatar.com/kb/ontoeip/OpenCycleGasTurbine"
    
    topologyNodeIRI_29Bus = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerGridTopology_6017554a-98bb-4896-bc21-e455cb6b3958" 
    queryEndpointLabel = "UKPowerSystemBaseWorld_test2"
    
    
    res, _ = queryGeneratorToBeRetrofitted_SelectedFuelOrGenerationTechnologyType(retrofitGenerationFuelOrTechType, topologyNodeIRI_29Bus, queryEndpointLabel)  

    def colorPicker(genType):
        if "NaturalGas" in str(genType) or "SourGas" in str(genType):
            return "#B71C1C"
        elif "Coal" in str(genType):
            return "#5C3291"
        else:
            return "#0C0C0C" 

    geojson_file = """
        {
            "type": "FeatureCollection",
            "features": ["""
    for r in res:
        if "Coal" in r["fuelOrGenType"]:
            colorPicker(r["fuelOrGenType"])
            
        feature = """{
            "type": "Feature",
            "properties": {
            "PowerGenerator": "%s",
            "Bus": "%s",
            "Capacity": %s,
            "fuelOrGenType": "%s",
            "marker-color": "%s"
            },
            "geometry": {
            "type": "Point",
            "coordinates": [
                %s,
                %s
            ]
            }
            },"""%(r["PowerGenerator"], r["Bus"], r["Capacity"], r["fuelOrGenType"], colorPicker(r["fuelOrGenType"]), r["LatLon"][1], r["LatLon"][0])
        # adding new line 
        geojson_file += '\n'+feature
    
    # removing last comma as is last line
    geojson_file = geojson_file[:-1]
    # finishing file end 
    end_geojson = """
        ]
    }
    """
    geojson_file += end_geojson
    # saving as geoJSON
    geojson_written = open('/mnt/d/wx243/FromAW/candidateSites.geojson','w')
    geojson_written.write(geojson_file)
    geojson_written.close() 
    print('---GeoJSON written successfully: candidate sites---')

