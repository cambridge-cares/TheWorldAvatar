##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 11 Oct 2022          #
##########################################

import os, sys, json
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.queryInterface import performQuery, performFederatedQuery
from SMRSitePreSelection.AnnualElectricityProduction import ElectricityProductionDistribution
from UK_Digital_Twin_Package.OWLfileStorer import readFile
from UK_Digital_Twin_Package import CO2FactorAndGenCostFactor as ModelFactor
from shapely.wkt import loads
from shapely.geometry import mapping
import urllib.parse
import requests
import time
import gc
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel
from rfc3987 import parse
from logging import raiseExceptions
import geojson
import ast

"""Create an object of Class CO2FactorAndGenCostFactor"""
ukmf = ModelFactor.ModelFactor()

"""Model Parameter Array"""
modelFactorArrays = readFile(ukmf.CO2EmissionFactorAndCostFactor)

##FIXME: add method to calculate the operation hours
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
    SELECT DISTINCT ?PowerGenerator ?Bus ?Capacity ?LatLon ?FuelType ?GenerationTechnology
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
    }
    """% (topologyNodeIRI, topologyNodeIRI)

    print('...starts queryGeneratorToBeRetrofitted_AllPowerPlant...')
    res = json.loads(performQuery(endPoint_label, queryStr))
    print('...finishes queryGeneratorToBeRetrofitted_AllPowerPlant...')

    for r in res:
            arranged_res = {
                        "PowerGenerator": r["PowerGenerator"],
                        "Bus": r["Bus"],
                        "Capacity": r["Capacity"],
                        "LatLon": [float(r['LatLon'].split('#')[0]), float(r['LatLon'].split('#')[1])],
                        "fuelOrGenType": r["GenerationTechnology"] if "Wind" in r["FuelType"] else r["FuelType"]
                        }
            results.append(arranged_res)
    return results 

##FIXME: add method to calculate the operation hours
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
        SELECT DISTINCT ?Bus ?Capacity ?LatLon ?FuelType ?GenerationTechnology
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
        
        }
        """% (gen, gen, gen)

        print('...starts queryGeneratorToBeRetrofitted_SelectedGenerator...')
        res = json.loads(performQuery(endPoint_label, queryStr))[0]
        print('...finishes queryGeneratorToBeRetrofitted_SelectedGenerator...')
        arranged_res = {
                        "PowerGenerator" : gen,
                        "Bus": res["Bus"],
                        "Capacity": res["Capacity"],
                        "LatLon": [float(res['LatLon'].split('#')[0]), float(res['LatLon'].split('#')[1])],
                        "fuelOrGenType": res["GenerationTechnology"] if "Wind" in res["FuelType"] else res["FuelType"],
                        "annualOperatingHours": 0
                        }
        results.append(arranged_res)
    return results 

def queryGeneratorToBeRetrofitted_SelectedFuelOrGenerationTechnologyType(retrofitGenerationOrFuelType:list, topologyNodeIRI:str, endPoint_label):  
    if endPoint_label == str(EndPointConfigAndBlazegraphRepoLabel.ukdigitaltwin['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.ukdigitaltwin['endpoint_iri'])
    elif parse(endPoint_label, rule='IRI'):
        endPointIRI = endPoint_label
    else:
        raiseExceptions("!!!!Please provide a valid endpoint!!!!")
    
    results = []
    genTypeSummary = []
    keys = ElectricityProductionDistribution.keys()
    for type in retrofitGenerationOrFuelType:
        if type not in keys:
            raise ValueError("!!!The given type of the generator should be in the ElectricityProductionDistribution!!!")       
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

        annualGenerationOfGivenType = ElectricityProductionDistribution[type]
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
       
        queryStr_totalGeneration = """
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
        SELECT (SUM(?Capacity) as ?Total_Capacity)
        WHERE
        {
        {?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:consumesPrimaryFuel ?FuelType .
        ?FuelType rdf:type <%s> . } UNION 
        {?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:usesGenerationTechnology ?TechType .
        ?TechType rdf:type <%s> . }

        ?PowerPlant ontocape_technical_system:hasRealizationAspect ?PowerGenerator .
        ?PowerPlant ontocape_technical_system:hasRequirementsAspect ?pp_capa .
        ?pp_capa rdf:type ontoeip_system_requirement:DesignCapacity .
        ?pp_capa ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Capacity .
        }
        """% (type, type)

        print('...starts queryGeneratorToBeRetrofitted_SelectedFuelOrGenerationTechnologyType...')
        res = json.loads(performQuery(endPointIRI, queryStr_1))
        print('...finishes queryGeneratorToBeRetrofitted_SelectedFuelOrGenerationTechnologyType...')  
        print('...starts query total capacity...')
        Total_Capacity = json.loads(performQuery(endPointIRI, queryStr_totalGeneration))[0]["Total_Capacity"]
        print('...finishes queryGeneratorToBeRetrofitted_SelectedPowerPlant...')   
        annualOperatingHours = round(float(annualGenerationOfGivenType)/float(Total_Capacity), 2)
        # print("The operating hours of the", type, " is", annualOperatingHours)
                
        for r in res:
            arranged_res = {
                            "PowerGenerator" : r["PowerGenerator"],
                            "Bus": r["Bus"],
                            "Capacity": r["Capacity"],
                            "LatLon": [float(r['LatLon'].split('#')[0]), float(r['LatLon'].split('#')[1])],
                            "fuelOrGenType": type,
                            "annualOperatingHours": float(annualOperatingHours),
                            "CO2EmissionFactor": CO2EmissionFactor,
                            "place": r["place"]
                            }
            results.append(arranged_res) 

        genTypeSummary.append({'fuelOrGenType': type,
                               'Total_Capacity': Total_Capacity,
                               'Total_Number_of_Generators':len(res),
                               'Replaced_Capacity': 0,
                               'Replaced_number_of_Generators': 0
                            })
        
    return results, genTypeSummary

if __name__ == '__main__':   
    # retrofitGenerationFuelOrTechType = ["http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas", 
    #     "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal", 
    #     "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Oil",
    #     "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#SourGas"]


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
    res = json.loads(performFederatedQuery(qsrt, ["http://statistics.data.gov.uk/sparql.json","http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test2/sparql"]))


    
    retrofitGenerationFuelOrTechType = ["http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas"]
    ocgt = "http://www.theworldavatar.com/kb/ontoeip/OpenCycleGasTurbine"
    
    topologyNodeIRI_29Bus = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerGridTopology_6017554a-98bb-4896-bc21-e455cb6b3958" 
    queryEndpointLabel = "ukdigitaltwin_test2"
    
    
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
    ?area ons:status "live" .
    ?area rdf:type ons:Statistical-Geography .
    ?area <http://publishmydata.com/def/ontology/foi/code> "%s" .
    ?area ons_geosparql:hasGeometry ?geometry .
    ?geometry ons_geosparql:asWKT ?areaBoundary .
    } GROUP BY ?area
    """% (LA_code)

    encodedString = urllib.parse.quote(queryStr)
    getString = "http://statistics.data.gov.uk/sparql.json?query=" + str(encodedString)

    print('... HTTP GET demanding Area Boundaries...')
    r = requests.get(getString, timeout=60)
    res = json.loads(r.text)['results']['bindings'][0]['Geo_InfoList']['value']
    print('...HTTP GET demanding Area Boundaries is done...')
    
    ## clear the symbols in the query results

    res = (res.split('\"^^')[0]).replace('\"','') 

    # Check the availability of the geometry of each area
    if res == 0: 
        raise Exception('There is one place does not have geometry information which is', r["LACode_area"], ', please check the query string and the place status in ONS.')
    elif "***" in res:
        res = res.split("***")[0]
    res = loads(res) # convert wkt into shapely polygons
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
    print('...query ifWithin condition...')
    res = json.loads(performQuery(ONS_Endpoint_label, queryStr))  
    print('...queryifWithin is done...')
    res = res[0]['ASK']
    return res

if __name__ == '__main__':
    i = 0
    area = ['E07000066', 'E07000128', 'W06000021', 'S12000048', 'E07000066', 'E07000128', 'W06000021', 'S12000048' ,'E07000066', 'E07000128', 'W06000021', 'S12000048']
    for a in area:
        r = queryAreaBoundaries(a)
        print(r)
        print(i)
        i += 1     

############################################OLD####################################

def queryBusModelInput(powerSystemModelIRI:str, endPointLabel, splitCharacter):
    queryStr = """
	PREFIX ontocape_upper_level_system:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontopowsys_PowerSystemModel:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#>
    PREFIX ontocape_network_system:<http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX ontoderivation:<https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#>
	SELECT DISTINCT ?BusNumberValue ?BusTypeValue ?PdValue ?GdValue ?GsValue ?BsValue ?AreaValue ?VoltMagValue ?VoltAngleValue ?BaseKVValue ?ZoneValue ?VMaxValue ?VMinValue

    WHERE {

    ?ElectricalBusModelIRI ontocape_upper_level_system:isExclusivelySubsystemOf <%s> . 
    ?ElectricalBusModelIRI a ontopowsys_PowerSystemModel:ElectricalBusModel .

    ?BusNode ontocape_upper_level_system:isModeledBy ?ElectricalBusModelIRI . 
    
    ?ElectricalBusModelIRI ontocape_network_system:hasInput ?BusNumber .
    ?BusNumber a ontopowsys_PowerSystemModel:BusNumber . 
    ?BusNumber ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BusNode . 
    ?BusNumber ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?BusNumberValue .
        
    ?ElectricalBusModelIRI ontocape_network_system:hasInput ?BusType .
    ?BusType a ontopowsys_PowerSystemModel:BusType . 
    ?BusType ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BusNode . 
    ?BusType ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?BusTypeValue .

    ?ElectricalBusModelIRI ontocape_network_system:hasInput ?Pd .
    ?Pd a ontopowsys_PowerSystemModel:PdBus . 
    ?Pd ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BusNode . 
    ?Pd ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?PdValue .

    ?ElectricalBusModelIRI ontocape_network_system:hasInput ?Gd .
    ?Gd a ontopowsys_PowerSystemModel:GdBus . 
    ?Gd ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BusNode . 
    ?Gd ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?GdValue .

    ?ElectricalBusModelIRI ontocape_network_system:hasInput ?Gs .
    ?Gs a ontopowsys_PowerSystemModel:Gs . 
    ?Gs ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BusNode . 
    ?Gs ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?GsValue .

    ?ElectricalBusModelIRI ontocape_network_system:hasInput ?Bs .
    ?Bs a ontopowsys_PowerSystemModel:Bs . 
    ?Bs ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BusNode . 
    ?Bs ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?BsValue .

    ?ElectricalBusModelIRI ontocape_network_system:hasInput ?Area .
    ?Area a ontopowsys_PowerSystemModel:Area . 
    ?Area ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BusNode . 
    ?Area ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?AreaValue .

    ?ElectricalBusModelIRI ontocape_network_system:hasInput ?VM .
    ?VM a ontopowsys_PowerSystemModel:Vm . 
    ?VM ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BusNode . 
    ?VM ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?VoltMagValue .

    ?ElectricalBusModelIRI ontocape_network_system:hasInput ?VA .
    ?VA a ontopowsys_PowerSystemModel:Va . 
    ?VA ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BusNode . 
    ?VA ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?VoltAngleValue .

    ?ElectricalBusModelIRI ontocape_network_system:hasInput ?BKV .
    ?BKV a ontopowsys_PowerSystemModel:baseKV . 
    ?BKV ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BusNode . 
    ?BKV ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?BaseKVValue .

    ?ElectricalBusModelIRI ontocape_network_system:hasInput ?Zone .
    ?Zone a ontopowsys_PowerSystemModel:Zone . 
    ?Zone ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BusNode . 
    ?Zone ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?ZoneValue .

    ?ElectricalBusModelIRI ontocape_network_system:hasInput ?VmMax .
    ?VmMax a ontopowsys_PowerSystemModel:VmMax . 
    ?VmMax ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BusNode . 
    ?VmMax ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?VMaxValue .

    ?ElectricalBusModelIRI ontocape_network_system:hasInput ?VmMin .
    ?VmMin a ontopowsys_PowerSystemModel:VmMin . 
    ?VmMin ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BusNode . 
    ?VmMin ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?VMinValue .
        }
    ORDER BY ASC(?BusNumberValue)
    """ %powerSystemModelIRI 
    
    # print(queryStr)
    print('remoteQuery queryBusModelInput')
    res = json.loads(performQuery(endPointLabel, queryStr))
    print('queryBusModelInput is done')
    
    # print(res, type(res))
    
    # print(len(res))
    for r in res:
      for key in r.keys():
          if '\"^^' in  r[key]:
            r[key] = (r[key].split('\"^^')[0]).replace('\"','')   

    ret_array = [ str(r['BusNumberValue']) + splitCharacter + str(r['BusTypeValue']) + splitCharacter + str(r['PdValue']) + splitCharacter + str(r['GdValue']) + \
                 splitCharacter + str(r['GsValue']) + splitCharacter + str(r['BsValue']) + splitCharacter + str(r['AreaValue']) + splitCharacter + str(r['VoltMagValue'])+ \
                  splitCharacter + str(r['VoltAngleValue']) + splitCharacter + str(r['BaseKVValue']) + splitCharacter + str(r['ZoneValue']) + splitCharacter + str(r['VMaxValue']) + \
                      splitCharacter + str(r['VMinValue']) for r in res ]    
    
    textfile = open("bus.txt", "w")
    for r in ret_array:
        textfile.write(r + "\n")
    textfile.close()
    return ret_array


def queryBranchModelInput(powerSystemModelIRI:str, endPointLabel, splitCharacter):
    queryStr = """
    PREFIX ontocape_upper_level_system:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontopowsys_PowerSystemModel:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#>
    PREFIX ontocape_network_system:<http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX ontoderivation:<https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#>
	SELECT DISTINCT ?BusFromValue ?BusToValue ?ResistanceValue ?ReactanceValue ?SusceptanceValue ?RateAValue ?RateBValue ?RateCValue ?RatioCoefficientValue ?AngleValue ?BranchStatusValue ?AngleMinValue ?AngleMaxValue

    WHERE {
    ?ElectricalELineModelIRI ontocape_upper_level_system:isExclusivelySubsystemOf <%s> . 
    ?ElectricalELineModelIRI a ontopowsys_PowerSystemModel:ElectricalBranchModel .

    ?BranchNode ontocape_upper_level_system:isModeledBy ?ElectricalELineModelIRI . 
    
    ?ElectricalELineModelIRI ontocape_network_system:hasInput ?BusFrom .
    ?BusFrom a ontopowsys_PowerSystemModel:BusFrom . 
    ?BusFrom ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BranchNode . 
    ?BusFrom ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?BusFromValue .
     
    ?ElectricalELineModelIRI ontocape_network_system:hasInput ?BusTo .
    ?BusTo a ontopowsys_PowerSystemModel:BusTo . 
    ?BusTo ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BranchNode . 
    ?BusTo ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?BusToValue .

    ?ElectricalELineModelIRI ontocape_network_system:hasInput ?R .
    ?R a ontopowsys_PowerSystemModel:R . 
    ?R ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BranchNode . 
    ?R ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?ResistanceValue .

    ?ElectricalELineModelIRI ontocape_network_system:hasInput ?X .
    ?X a ontopowsys_PowerSystemModel:X . 
    ?X ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BranchNode . 
    ?X ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?ReactanceValue .

    ?ElectricalELineModelIRI ontocape_network_system:hasInput ?B .
    ?B a ontopowsys_PowerSystemModel:B . 
    ?B ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BranchNode . 
    ?B ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?SusceptanceValue .

    ?ElectricalELineModelIRI ontocape_network_system:hasInput ?RateA .
    ?RateA a ontopowsys_PowerSystemModel:RateA . 
    ?RateA ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BranchNode . 
    ?RateA ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?RateAValue .

    ?ElectricalELineModelIRI ontocape_network_system:hasInput ?RateB .
    ?RateB a ontopowsys_PowerSystemModel:RateB . 
    ?RateB ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BranchNode . 
    ?RateB ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?RateBValue .

    ?ElectricalELineModelIRI ontocape_network_system:hasInput ?RateC .
    ?RateC a ontopowsys_PowerSystemModel:RateC . 
    ?RateC ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BranchNode . 
    ?RateC ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?RateCValue .

    ?ElectricalELineModelIRI ontocape_network_system:hasInput ?RatioCoefficient .
    ?RatioCoefficient a ontopowsys_PowerSystemModel:RatioCoefficient . 
    ?RatioCoefficient ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BranchNode . 
    ?RatioCoefficient ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?RatioCoefficientValue .

    ?ElectricalELineModelIRI ontocape_network_system:hasInput ?Angle .
    ?Angle a ontopowsys_PowerSystemModel:Angle . 
    ?Angle ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BranchNode . 
    ?Angle ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?AngleValue .

    ?ElectricalELineModelIRI ontocape_network_system:hasInput ?BranchStatus .
    ?BranchStatus a ontopowsys_PowerSystemModel:BranchStatus . 
    ?BranchStatus ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BranchNode . 
    ?BranchStatus ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?BranchStatusValue .

    ?ElectricalELineModelIRI ontocape_network_system:hasInput ?AngleMin .
    ?AngleMin a ontopowsys_PowerSystemModel:AngleMin . 
    ?AngleMin ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BranchNode . 
    ?AngleMin ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?AngleMinValue .

    ?ElectricalELineModelIRI ontocape_network_system:hasInput ?AngleMax .
    ?AngleMax a ontopowsys_PowerSystemModel:AngleMax . 
    ?AngleMax ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom ?BranchNode . 
    ?AngleMax ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?AngleMaxValue .
    }
    ORDER BY ASC(?BusFromValue)
    """ %powerSystemModelIRI 
    
    print(queryStr)

    print('remoteQuery queryBranchModelInput')
    res = json.loads(performQuery(endPointLabel, queryStr))
    print('queryBranchModelInput is done')
    for r in res:
      for key in r.keys():
          if '\"^^' in  r[key]:
            r[key] = (r[key].split('\"^^')[0]).replace('\"','')   

    ret_array = [ str(r['BusFromValue']) + splitCharacter + str(r['BusToValue']) + splitCharacter + str(r['ResistanceValue']) + splitCharacter + str(r['ReactanceValue']) + \
                 splitCharacter + str(r['SusceptanceValue']) + splitCharacter + str(r['RateAValue']) + splitCharacter + str(r['RateBValue']) + splitCharacter + str(r['RateCValue'])+ \
                  splitCharacter + str(r['RatioCoefficientValue']) + splitCharacter + str(r['AngleValue']) + splitCharacter + str(r['BranchStatusValue']) + splitCharacter + str(r['AngleMinValue']) + \
                      splitCharacter + str(r['AngleMaxValue']) for r in res ]    
    
    textfile = open("branch.txt", "w")
    
    for r in ret_array:
        textfile.write(r + "\n")
    textfile.close()
    return ret_array


def queryGeneratorModelInput(powerSystemModelIRI:str, endPointLabel, splitCharacter):  
    # queryStr = """
    # PREFIX ontocape_upper_level_system:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    # PREFIX ontopowsys_PowerSystemModel:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#>
    # PREFIX ontocape_network_system:<http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    # PREFIX ontoderivation:<https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#>
	# SELECT DISTINCT ?BusNumberValue ?PgValue ?QgValue ?QMaxValue ?QMinValue ?VgValue ?mBaseValue ?StatusValue ?PMaxValue ?PMinValue ?Pc1Value ?Pc2Value ?QC1MinValue ?QC1MaxValue ?QC2MinValue ?QC2MaxValue ?RampagcValue ?Ramp10Value ?Ramp30Value ?RampqValue ?APFValue ?CostModelValue ?StartCostValue ?StopCostValue ?genCostnValue ?FirstOrderCoefficientValue ?ZeroOrderCoefficientValue
    # WHERE {
    # ?ElectricalGeneratorModelIRI ontocape_upper_level_system:isExclusivelySubsystemOf <%s> . 
    # ?ElectricalGeneratorModelIRI a ontopowsys_PowerSystemModel:ElectricalGeneratorModel .

    # ?GeneratorNode ontocape_upper_level_system:isModeledBy ?ElectricalGeneratorModelIRI . 
    
    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?BusNumber .
    # ?BusNumber a ontopowsys_PowerSystemModel:BusNumber . 
    # ?BusNumber ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?BusNumber ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?BusNumberValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Pg .
    # ?Pg a ontopowsys_PowerSystemModel:Pg . 
    # ?Pg ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?Pg ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?PgValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Qg .
    # ?Qg a ontopowsys_PowerSystemModel:Qg . 
    # ?Qg ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?Qg ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?QgValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?QMax .
    # ?QMax a ontopowsys_PowerSystemModel:QMax . 
    # ?QMax ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?QMax ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?QMaxValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?QMin .
    # ?QMin a ontopowsys_PowerSystemModel:QMin . 
    # ?QMin ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?QMin ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?QMinValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Vg .
    # ?Vg a ontopowsys_PowerSystemModel:Vg . 
    # ?Vg ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?Vg ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?VgValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?mBase .
    # ?mBase a ontopowsys_PowerSystemModel:mBase . 
    # ?mBase ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?mBase ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?mBaseValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Status .
    # ?Status a ontopowsys_PowerSystemModel:Status . 
    # ?Status ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?Status ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?StatusValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?PMax .
    # ?PMax a ontopowsys_PowerSystemModel:PMax . 
    # ?PMax ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?PMax ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?PMaxValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?PMin .
    # ?PMin a ontopowsys_PowerSystemModel:PMin . 
    # ?PMin ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?PMin ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?PMinValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Pc1 .
    # ?Pc1 a ontopowsys_PowerSystemModel:Pc1 . 
    # ?Pc1 ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?Pc1 ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Pc1Value .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Pc2 .
    # ?Pc2 a ontopowsys_PowerSystemModel:Pc2 . 
    # ?Pc2 ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?Pc2 ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Pc2Value .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?QC1Min .
    # ?QC1Min a ontopowsys_PowerSystemModel:QC1Min . 
    # ?QC1Min ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?QC1Min ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?QC1MinValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?QC1Max .
    # ?QC1Max a ontopowsys_PowerSystemModel:QC1Max . 
    # ?QC1Max ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?QC1Max ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?QC1MaxValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?QC2Min .
    # ?QC2Min a ontopowsys_PowerSystemModel:QC2Min . 
    # ?QC2Min ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?QC2Min ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?QC2MinValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?QC2Max .
    # ?QC2Max a ontopowsys_PowerSystemModel:QC2Max . 
    # ?QC2Max ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?QC2Max ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?QC2MaxValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Rampagc .
    # ?Rampagc a ontopowsys_PowerSystemModel:Rampagc . 
    # ?Rampagc ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?Rampagc ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?RampagcValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Ramp10 .
    # ?Ramp10 a ontopowsys_PowerSystemModel:Ramp10 . 
    # ?Ramp10 ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?Ramp10 ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Ramp10Value .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Ramp30 .
    # ?Ramp30 a ontopowsys_PowerSystemModel:Ramp30 . 
    # ?Ramp30 ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?Ramp30 ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Ramp30Value .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Rampq .
    # ?Rampq a ontopowsys_PowerSystemModel:Rampq . 
    # ?Rampq ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?Rampq ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?RampqValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?APF .
    # ?APF a ontopowsys_PowerSystemModel:APF . 
    # ?APF ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?APF ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?APFValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?CostModel .
    # ?CostModel a ontopowsys_PowerSystemModel:CostModel . 
    # ?CostModel ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?CostModel ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?CostModelValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?StartCost .
    # ?StartCost a ontopowsys_PowerSystemModel:StartCost . 
    # ?StartCost ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?StartCost ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?StartCostValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?StopCost .
    # ?StopCost a ontopowsys_PowerSystemModel:StopCost . 
    # ?StopCost ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?StopCost ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?StopCostValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?genCostn .
    # ?genCostn a ontopowsys_PowerSystemModel:genCostn . 
    # ?genCostn ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?genCostn ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?genCostnValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?FirstOrderCoefficient .
    # ?FirstOrderCoefficient a ontopowsys_PowerSystemModel:FirstOrderCoefficient . 
    # ?FirstOrderCoefficient ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?FirstOrderCoefficient ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?FirstOrderCoefficientValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?ZeroOrderCoefficient .
    # ?ZeroOrderCoefficient a ontopowsys_PowerSystemModel:ZeroOrderCoefficient . 
    # ?ZeroOrderCoefficient ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?ZeroOrderCoefficient ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?ZeroOrderCoefficientValue .
    # }
    # ORDER BY ASC(?BusNumberValue)
    # """ %powerSystemModelIRI 
    

    # queryStr = """
    # PREFIX ontocape_upper_level_system:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    # PREFIX ontopowsys_PowerSystemModel:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#>
    # PREFIX ontocape_network_system:<http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    # PREFIX ontoderivation:<https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#>
	# SELECT DISTINCT ?BusNumberValue ?PgValue ?QgValue ?QMaxValue ?QMinValue ?VgValue ?mBaseValue ?StatusValue ?PMaxValue ?PMinValue ?Pc1Value ?Pc2Value ?QC1MinValue ?QC1MaxValue ?QC2MinValue ?QC2MaxValue ?RampagcValue ?Ramp10Value ?Ramp30Value ?RampqValue ?APFValue ?CostModelValue ?StartCostValue ?StopCostValue ?genCostnValue ?FirstOrderCoefficientValue ?ZeroOrderCoefficientValue
    # WHERE {
    
    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?BusNumber .
    # ?BusNumber a ontopowsys_PowerSystemModel:BusNumber . 
    # ?BusNumber ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom  <%s> . 
    # ?BusNumber ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?BusNumberValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Pg .
    # ?Pg a ontopowsys_PowerSystemModel:Pg . 
    # ?Pg ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom <%s> . 
    # ?Pg ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?PgValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Qg .
    # ?Qg a ontopowsys_PowerSystemModel:Qg . 
    # ?Qg ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom <%s> . 
    # ?Qg ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?QgValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?QMax .
    # ?QMax a ontopowsys_PowerSystemModel:QMax . 
    # ?QMax ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom <%s> . 
    # ?QMax ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?QMaxValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?QMin .
    # ?QMin a ontopowsys_PowerSystemModel:QMin . 
    # ?QMin ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom <%s> . 
    # ?QMin ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?QMinValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Vg .
    # ?Vg a ontopowsys_PowerSystemModel:Vg . 
    # ?Vg ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom <%s> . 
    # ?Vg ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?VgValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?mBase .
    # ?mBase a ontopowsys_PowerSystemModel:mBase . 
    # ?mBase ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom <%s> . 
    # ?mBase ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?mBaseValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Status .
    # ?Status a ontopowsys_PowerSystemModel:Status . 
    # ?Status ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom <%s> . 
    # ?Status ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?StatusValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?PMax .
    # ?PMax a ontopowsys_PowerSystemModel:PMax . 
    # ?PMax ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom <%s> . 
    # ?PMax ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?PMaxValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?PMin .
    # ?PMin a ontopowsys_PowerSystemModel:PMin . 
    # ?PMin ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom <%s> . 
    # ?PMin ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?PMinValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Pc1 .
    # ?Pc1 a ontopowsys_PowerSystemModel:Pc1 . 
    # ?Pc1 ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom <%s> . 
    # ?Pc1 ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Pc1Value .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Pc2 .
    # ?Pc2 a ontopowsys_PowerSystemModel:Pc2 . 
    # ?Pc2 ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom <%s> . 
    # ?Pc2 ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Pc2Value .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?QC1Min .
    # ?QC1Min a ontopowsys_PowerSystemModel:QC1Min . 
    # ?QC1Min ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom <%s> . 
    # ?QC1Min ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?QC1MinValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?QC1Max .
    # ?QC1Max a ontopowsys_PowerSystemModel:QC1Max . 
    # ?QC1Max ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom <%s> . 
    # ?QC1Max ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?QC1MaxValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?QC2Min .
    # ?QC2Min a ontopowsys_PowerSystemModel:QC2Min . 
    # ?QC2Min ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom <%s> . 
    # ?QC2Min ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?QC2MinValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?QC2Max .
    # ?QC2Max a ontopowsys_PowerSystemModel:QC2Max . 
    # ?QC2Max ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom <%s> . 
    # ?QC2Max ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?QC2MaxValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Rampagc .
    # ?Rampagc a ontopowsys_PowerSystemModel:Rampagc . 
    # ?Rampagc ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom <%s> . 
    # ?Rampagc ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?RampagcValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Ramp10 .
    # ?Ramp10 a ontopowsys_PowerSystemModel:Ramp10 . 
    # ?Ramp10 ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom <%s> . 
    # ?Ramp10 ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Ramp10Value .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Ramp30 .
    # ?Ramp30 a ontopowsys_PowerSystemModel:Ramp30 . 
    # ?Ramp30 ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom <%s> . 
    # ?Ramp30 ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Ramp30Value .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Rampq .
    # ?Rampq a ontopowsys_PowerSystemModel:Rampq . 
    # ?Rampq ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom <%s> . 
    # ?Rampq ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?RampqValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?APF .
    # ?APF a ontopowsys_PowerSystemModel:APF . 
    # ?APF ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?APF ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?APFValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?CostModel .
    # ?CostModel a ontopowsys_PowerSystemModel:CostModel . 
    # ?CostModel ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?CostModel ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?CostModelValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?StartCost .
    # ?StartCost a ontopowsys_PowerSystemModel:StartCost . 
    # ?StartCost ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?StartCost ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?StartCostValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?StopCost .
    # ?StopCost a ontopowsys_PowerSystemModel:StopCost . 
    # ?StopCost ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?StopCost ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?StopCostValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?genCostn .
    # ?genCostn a ontopowsys_PowerSystemModel:genCostn . 
    # ?genCostn ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?genCostn ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?genCostnValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?FirstOrderCoefficient .
    # ?FirstOrderCoefficient a ontopowsys_PowerSystemModel:FirstOrderCoefficient . 
    # ?FirstOrderCoefficient ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?FirstOrderCoefficient ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?FirstOrderCoefficientValue .

    # ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?ZeroOrderCoefficient .
    # ?ZeroOrderCoefficient a ontopowsys_PowerSystemModel:ZeroOrderCoefficient . 
    # ?ZeroOrderCoefficient ontoderivation:belongsTo ?Derivation .
    # ?Derivation a ontoderivation:DerivationAsyn . 
    # ?Derivation ontoderivation:isDerivedFrom ?GeneratorNode . 
    # ?ZeroOrderCoefficient ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?ZeroOrderCoefficientValue .
    # }
    # ORDER BY ASC(?BusNumberValue)
    # """ %powerSystemModelIRI 

    queryStr = """
    PREFIX ontocape_upper_level_system:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontopowsys_PowerSystemModel:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#>
    PREFIX ontocape_network_system:<http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX ontoderivation:<https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#>
	SELECT DISTINCT ?BusNumberValue ?PgValue ?QgValue ?QMaxValue ?QMinValue ?VgValue ?mBaseValue ?StatusValue ?PMaxValue ?PMinValue ?Pc1Value ?Pc2Value ?QC1MinValue ?QC1MaxValue ?QC2MinValue ?QC2MaxValue ?RampagcValue ?Ramp10Value ?Ramp30Value ?RampqValue ?APFValue ?CostModelValue ?StartCostValue ?StopCostValue ?genCostnValue ?FirstOrderCoefficientValue ?ZeroOrderCoefficientValue
    WHERE {
    
    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?BusNumber .
    ?BusNumber a ontopowsys_PowerSystemModel:BusNumber . 
    ?BusNumber ontoderivation:belongsTo ?Derivation .
    ?Derivation a ontoderivation:DerivationAsyn . 
    ?Derivation ontoderivation:isDerivedFrom  <%s> . 
    ?BusNumber ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?BusNumberValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Pg .
    ?Pg a ontopowsys_PowerSystemModel:Pg . 
    ?Pg ontoderivation:belongsTo ?Derivation .
    ?Pg ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?PgValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Qg .
    ?Qg a ontopowsys_PowerSystemModel:Qg . 
    ?Qg ontoderivation:belongsTo ?Derivation .
    ?Qg ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?QgValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?QMax .
    ?QMax a ontopowsys_PowerSystemModel:QMax . 
    ?QMax ontoderivation:belongsTo ?Derivation .
    ?QMax ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?QMaxValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?QMin .
    ?QMin a ontopowsys_PowerSystemModel:QMin . 
    ?QMin ontoderivation:belongsTo ?Derivation .
    ?QMin ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?QMinValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Vg .
    ?Vg a ontopowsys_PowerSystemModel:Vg . 
    ?Vg ontoderivation:belongsTo ?Derivation .
    ?Vg ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?VgValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?mBase .
    ?mBase a ontopowsys_PowerSystemModel:mBase . 
    ?mBase ontoderivation:belongsTo ?Derivation .
    ?mBase ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?mBaseValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Status .
    ?Status a ontopowsys_PowerSystemModel:Status . 
    ?Status ontoderivation:belongsTo ?Derivation .
    ?Status ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?StatusValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?PMax .
    ?PMax a ontopowsys_PowerSystemModel:PMax . 
    ?PMax ontoderivation:belongsTo ?Derivation .
    ?PMax ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?PMaxValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?PMin .
    ?PMin a ontopowsys_PowerSystemModel:PMin . 
    ?PMin ontoderivation:belongsTo ?Derivation .
    ?PMin ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?PMinValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Pc1 .
    ?Pc1 a ontopowsys_PowerSystemModel:Pc1 . 
    ?Pc1 ontoderivation:belongsTo ?Derivation .
    ?Pc1 ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Pc1Value .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Pc2 .
    ?Pc2 a ontopowsys_PowerSystemModel:Pc2 . 
    ?Pc2 ontoderivation:belongsTo ?Derivation .
    ?Pc2 ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Pc2Value .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?QC1Min .
    ?QC1Min a ontopowsys_PowerSystemModel:QC1Min . 
    ?QC1Min ontoderivation:belongsTo ?Derivation .
    ?QC1Min ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?QC1MinValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?QC1Max .
    ?QC1Max a ontopowsys_PowerSystemModel:QC1Max . 
    ?QC1Max ontoderivation:belongsTo ?Derivation .
    ?QC1Max ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?QC1MaxValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?QC2Min .
    ?QC2Min a ontopowsys_PowerSystemModel:QC2Min . 
    ?QC2Min ontoderivation:belongsTo ?Derivation .
    ?QC2Min ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?QC2MinValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?QC2Max .
    ?QC2Max a ontopowsys_PowerSystemModel:QC2Max . 
    ?QC2Max ontoderivation:belongsTo ?Derivation .
    ?QC2Max ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?QC2MaxValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Rampagc .
    ?Rampagc a ontopowsys_PowerSystemModel:Rampagc . 
    ?Rampagc ontoderivation:belongsTo ?Derivation .
    ?Rampagc ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?RampagcValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Ramp10 .
    ?Ramp10 a ontopowsys_PowerSystemModel:Ramp10 . 
    ?Ramp10 ontoderivation:belongsTo ?Derivation .
    ?Ramp10 ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Ramp10Value .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Ramp30 .
    ?Ramp30 a ontopowsys_PowerSystemModel:Ramp30 . 
    ?Ramp30 ontoderivation:belongsTo ?Derivation . 
    ?Ramp30 ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Ramp30Value .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?Rampq .
    ?Rampq a ontopowsys_PowerSystemModel:Rampq . 
    ?Rampq ontoderivation:belongsTo ?Derivation .
    ?Rampq ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?RampqValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?APF .
    ?APF a ontopowsys_PowerSystemModel:APF . 
    ?APF ontoderivation:belongsTo ?Derivation .
    ?APF ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?APFValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?CostModel .
    ?CostModel a ontopowsys_PowerSystemModel:CostModel . 
    ?CostModel ontoderivation:belongsTo ?Derivation .
    ?CostModel ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?CostModelValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?StartCost .
    ?StartCost a ontopowsys_PowerSystemModel:StartCost . 
    ?StartCost ontoderivation:belongsTo ?Derivation . 
    ?StartCost ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?StartCostValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?StopCost .
    ?StopCost a ontopowsys_PowerSystemModel:StopCost . 
    ?StopCost ontoderivation:belongsTo ?Derivation .
    ?StopCost ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?StopCostValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?genCostn .
    ?genCostn a ontopowsys_PowerSystemModel:genCostn . 
    ?genCostn ontoderivation:belongsTo ?Derivation .
    ?genCostn ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?genCostnValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?FirstOrderCoefficient .
    ?FirstOrderCoefficient a ontopowsys_PowerSystemModel:FirstOrderCoefficient . 
    ?FirstOrderCoefficient ontoderivation:belongsTo ?Derivation . 
    ?FirstOrderCoefficient ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?FirstOrderCoefficientValue .

    ?ElectricalGeneratorModelIRI ontocape_network_system:hasInput ?ZeroOrderCoefficient .
    ?ZeroOrderCoefficient a ontopowsys_PowerSystemModel:ZeroOrderCoefficient . 
    ?ZeroOrderCoefficient ontoderivation:belongsTo ?Derivation .
    ?ZeroOrderCoefficient ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?ZeroOrderCoefficientValue .
    }
    """ %powerSystemModelIRI 

    #print(queryStr)
    print('...remoteQuery queryGeneratorModelInput...')
    res = json.loads(performQuery(endPointLabel, queryStr))
    print('...queryGeneratorModelInput is done...')
    for r in res:
      for key in r.keys():
          if '\"^^' in  r[key]:
            r[key] = (r[key].split('\"^^')[0]).replace('\"','')   
 
    gen_array = [ str(r['BusNumberValue']) + splitCharacter + str(r['PgValue']) + splitCharacter + str(r['QgValue']) + splitCharacter + str(r['QMaxValue']) + \
                 splitCharacter + str(r['QMinValue']) + splitCharacter + str(r['VgValue']) + splitCharacter + str(r['mBaseValue']) + splitCharacter + str(r['StatusValue'])+ \
                  splitCharacter + str(r['PMaxValue']) + splitCharacter + str(r['PMinValue']) + splitCharacter + str(r['Pc1Value']) + splitCharacter + str(r['Pc2Value']) + \
                      splitCharacter + str(r['QC1MinValue']) + splitCharacter + str(r['QC1MaxValue']) + splitCharacter + str(r['QC2MinValue']) + splitCharacter + str(r['QC2MaxValue']) + \
                          splitCharacter + str(r['RampagcValue']) + splitCharacter + str(r['Ramp10Value']) + splitCharacter + str(r['Ramp30Value']) + splitCharacter + \
                              str(r['RampqValue']) + splitCharacter + str(r['APFValue']) for r in res ]    

    genCost_array = [ str(r['CostModelValue']) + splitCharacter + str(r['StartCostValue']) + splitCharacter + str(r['StopCostValue']) + splitCharacter + str(r['genCostnValue']) + \
                 splitCharacter + str(r['FirstOrderCoefficientValue']) + splitCharacter + str(r['ZeroOrderCoefficientValue']) for r in res ]    
    
    print(len(gen_array))
    textfile = open("gen.txt", "w")
    for r in gen_array:
        textfile.write(r + "\n")
    textfile.close()

    textfile = open("genCost.txt", "w")
    for r in genCost_array:
        textfile.write(r + "\n")
    textfile.close()

    return gen_array, genCost_array

if __name__ == '__main__':
    powerSystemModelIRI = "http://www.theworldavatar.com/kb/ontopowsys/PowerSystemModel_458c40d4-e56e-44b5-a160-8f55d1e5a2c5"
    #res = queryBusModelInput(powerSystemModelIRI, 'ukdigitaltwin_test3', " ")
    #resGen, resGenCost = queryGeneratorModelInput(powerSystemModelIRI, 'ukdigitaltwin_test1', " ")
    res = queryBranchModelInput(powerSystemModelIRI, 'ukdigitaltwin_test3', " ")
    
    #print(len(resGen), len(resGenCost))
    