import os, sys, json
BASE = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.queryInterface import performQuery
from UK_Digital_Twin_Package.OWLfileStorer import readFile
from UK_Digital_Twin_Package import CO2FactorAndGenCostFactor as ModelFactor
from shapely.wkt import loads
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel
from rfc3987 import parse
from logging import raiseExceptions
from UK_Digital_Twin_Package.iris import *

"""Create an object of Class CO2FactorAndGenCostFactor"""
ukmf = ModelFactor.ModelFactor()

"""Model Parameter Array"""
modelFactorArrays = readFile(ukmf.CO2EmissionFactorAndCostFactor)

def queryEliminatePowerPlant(plantNameList:list, endpoint):
    ppList = []
    for ppName in plantNameList:
        queryStr = f"""
        
        SELECT DISTINCT ?powerPlantIRI
        WHERE
        {{
        ?powerPlantIRI <{RDFS_LABEL}> "{ppName}" .
        ?powerPlantIRI <{RDF_TYPE}> <{ONTOEIP_POWERPLANT_POWERPLANT}> .
        }}
        """

        print('...starts queryEliminatePowerPlant...')
        ppList.append(json.loads(performQuery(endpoint, queryStr))[0]['powerPlantIRI'])
        print('...finishes queryEliminatePowerPlant...')
    return ppList

def queryTopologyIRI(numOfBus, numOfBranch, endpoint):

    label = "UK_Topology_" + str(numOfBus) + "_Bus_" + str(numOfBranch) + "_Branch"

    queryStr = f"""
    SELECT DISTINCT ?topologyIRI
    WHERE
    {{
    ?topologyIRI <{RDFS_LABEL}> "{label}" .
    ?topologyIRI <{RDF_TYPE}> <{ONTOENERGYSYSTEM_POWERGRIDTOPOLOGY}> .
    }}
    """

    print('...starts queryTopologyIRI...')
    res = json.loads(performQuery(endpoint, queryStr))[0]['topologyIRI']
    print('...finishes queryTopologyIRI...')
    return res

def querySlackBusNode(slackBusLocation:str, endpoint):

    queryStr = f"""
    SELECT DISTINCT ?slackBusNodeIRI ?latlon
    WHERE
    {{
    ?slackBusNodeIRI <{ONTOENERGYSYSTEM_HASWGS84LATITUDELONGITUDE}> ?latlon .
    ?slackBusNodeIRI <{RDF_TYPE}> <{ONTOPOWSYS_POWSYSREALIZATION_BUSNODE}> .
    }}
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
    queryStr = f"""
    SELECT DISTINCT ?PowerGenerator ?Bus ?Capacity ?LatLon ?FuelType ?GenerationTechnology ?place
    WHERE
    {{
    ?GBElectricitySystemIRI <{ONTOCAPE_UPPER_LEVEL_SYSTEM_CONTAINS}> ?PowerPlant .
    ?GBElectricitySystemIRI <{ONTOENERGYSYSTEM_HASRELEVANTPLACE}>/<{OWL_SAMEAS}> <https://dbpedia.org/page/Great_Britain> .

    <{topologyNodeIRI}> <{ONTOCAPE_UPPER_LEVEL_SYSTEM_ISCOMPOSEDOFSUBSYSTEM}> ?PowerGenerator . 
    <{topologyNodeIRI}> <{ONTOCAPE_UPPER_LEVEL_SYSTEM_ISCOMPOSEDOFSUBSYSTEM}> ?Bus . 
    
    ?PowerGenerator <{META_MEDOL_TOPOLOGY_HASOUTPUT}> ?Bus .
    ?Bus <{RDF_TYPE}> <{ONTOPOWSYS_POWSYSREALIZATION_BUSNODE}> .  
    ?PowerGenerator <{RDF_TYPE}> <{ONTOEIP_POWERPLANT_POWERPLANT}> . 

    ?PowerGenerator <{ONTOECAPE_TECHNICAL_SYSTEM_REALIZES}>/<{ONTOEIP_POWERPLANT_USESGENERATIONTECHNOLOGY}> ?GenerationTechnologyIRI .
    ?GenerationTechnologyIRI <{RDF_TYPE}> ?GenerationTechnology .
    
    ?PowerGenerator <{ONTOECAPE_TECHNICAL_SYSTEM_REALIZES}>/<{ONTOEIP_POWERPLANT_CONSUMESPRIMARYFUEL}> ?FuelTypeIRI .
    ?FuelTypeIRI <{RDF_TYPE}> ?FuelType . 
    
    FILTER NOT EXISTS {{ ?GenerationTechnologyIRI <{RDF_TYPE}> <http://www.theworldavatar.com/kb/ontoeip/WindOffshore> .}}  
    
    ?PowerPlant <{ONTOECAPE_TECHNICAL_SYSTEM_HASREALIZATIONASPECT}> ?PowerGenerator .
    ?PowerPlant <{ONTOECAPE_TECHNICAL_SYSTEM_HASREQUIREMENTSASPECT}> ?pp_capa .
    ?pp_capa <{RDF_TYPE}> <{ONTOEIP_SYSTEM_REQUIREMENT_DESIGNCAPACITY}> .
    ?pp_capa <{ONTOCAPE_UPPER_LEVEL_SYSTEM_HASVALUE}>/<{ONTOCAPE_UPPER_LEVEL_SYSTEM_NUMERICALVALUE}> ?Capacity .

    ?PowerPlant <{ONTOECAPE_TECHNICAL_SYSTEM_HASREALIZATIONASPECT}> ?PowerGenerator . 
    ?PowerPlant <{ONTOENERGYSYSTEM_HASWGS84LATITUDELONGITUDE}> ?LatLon .  
    ?PowerPlant <{ONTOENERGYSYSTEM_HASRELEVANTPLACE}>/<{OWL_SAMEAS}> ?place . 
    }}
    """

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
        queryStr = f"""
        SELECT DISTINCT ?Bus ?Capacity ?LatLon ?FuelType ?GenerationTechnology ?place
        WHERE
        {{
        <{gen}> <{META_MEDOL_TOPOLOGY_HASOUTPUT}> ?Bus .
        ?Bus <{RDF_TYPE}> <{ONTOPOWSYS_POWSYSREALIZATION_BUSNODE}> .  
        <{gen}> <{RDF_TYPE}> <{ONTOEIP_POWERPLANT_POWERPLANT}> . 
        
        ?PowerPlant <{ONTOECAPE_TECHNICAL_SYSTEM_HASREALIZATIONASPECT}> <{gen}> .
        ?PowerPlant <{ONTOECAPE_TECHNICAL_SYSTEM_HASREQUIREMENTSASPECT}> ?pp_capa .
        ?pp_capa <{RDF_TYPE}> <{ONTOEIP_SYSTEM_REQUIREMENT_DESIGNCAPACITY}> .
        ?pp_capa <{ONTOCAPE_UPPER_LEVEL_SYSTEM_HASVALUE}>/<{ONTOCAPE_UPPER_LEVEL_SYSTEM_NUMERICALVALUE}> ?Capacity .
        
        ?PowerPlant <{ONTOECAPE_TECHNICAL_SYSTEM_HASREALIZATIONASPECT}> ?PowerGenerator . 
        ?PowerPlant <{ONTOENERGYSYSTEM_HASWGS84LATITUDELONGITUDE}> ?LatLon .

        ?PowerGenerator <{ONTOECAPE_TECHNICAL_SYSTEM_REALIZES}>/<{ONTOEIP_POWERPLANT_USESGENERATIONTECHNOLOGY}> ?GenerationTechnologyIRI .
        ?GenerationTechnologyIRI <{RDF_TYPE}> ?GenerationTechnology .
        
        ?PowerGenerator <{ONTOECAPE_TECHNICAL_SYSTEM_REALIZES}>/<{ONTOEIP_POWERPLANT_CONSUMESPRIMARYFUEL}> ?FuelTypeIRI .
        ?FuelTypeIRI <{RDF_TYPE}> ?FuelType . 
        ?PowerPlant <{ONTOENERGYSYSTEM_HASRELEVANTPLACE}>/<{OWL_SAMEAS}> ?place . 
        
        }}
        """

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

        queryStr_1 = f"""
        SELECT DISTINCT ?PowerGenerator ?Bus ?Capacity ?LatLon ?place 
        WHERE
        {{
        ?GBElectricitySystemIRI <{ONTOCAPE_UPPER_LEVEL_SYSTEM_CONTAINS}> ?PowerPlant .
        ?GBElectricitySystemIRI <{ONTOENERGYSYSTEM_HASRELEVANTPLACE}>/<{OWL_SAMEAS}> <https://dbpedia.org/page/Great_Britain> .

        <{topologyNodeIRI}> <{ONTOCAPE_UPPER_LEVEL_SYSTEM_ISCOMPOSEDOFSUBSYSTEM}> ?PowerGenerator . 
        <{topologyNodeIRI}> <{ONTOCAPE_UPPER_LEVEL_SYSTEM_ISCOMPOSEDOFSUBSYSTEM}> ?Bus . 
        
        ?PowerGenerator <{META_MEDOL_TOPOLOGY_HASOUTPUT}> ?Bus .
        ?Bus <{RDF_TYPE}> <{ONTOPOWSYS_POWSYSREALIZATION_BUSNODE}> .  
        ?PowerGenerator <{RDF_TYPE}> <{ONTOEIP_POWERPLANT_POWERGENERATOR}> . 

        {{?PowerGenerator <{ONTOECAPE_TECHNICAL_SYSTEM_REALIZES}>/<{ONTOEIP_POWERPLANT_CONSUMESPRIMARYFUEL}> ?FuelType .
        ?FuelType <{RDF_TYPE}> <{type}> . }} UNION 
        {{?PowerGenerator <{ONTOECAPE_TECHNICAL_SYSTEM_REALIZES}>/<{ONTOEIP_POWERPLANT_USESGENERATIONTECHNOLOGY}> ?TechType .
        ?TechType <{RDF_TYPE}> <{type}> . }}

        ?PowerGenerator <{ONTOECAPE_TECHNICAL_SYSTEM_REALIZES}>/<{ONTOEIP_POWERPLANT_USESGENERATIONTECHNOLOGY}> ?GenerationTechnologyIRI .

        FILTER NOT EXISTS {{ ?GenerationTechnologyIRI <{RDF_TYPE}> <http://www.theworldavatar.com/kb/ontoeip/WindOffshore> .}}   
        
        ?PowerPlant <{ONTOECAPE_TECHNICAL_SYSTEM_HASREALIZATIONASPECT}> ?PowerGenerator .
        ?PowerPlant <{ONTOECAPE_TECHNICAL_SYSTEM_HASREQUIREMENTSASPECT}> ?pp_capa .
        ?pp_capa <{RDF_TYPE}> <{ONTOEIP_SYSTEM_REQUIREMENT_DESIGNCAPACITY}> .
        ?pp_capa <{ONTOCAPE_UPPER_LEVEL_SYSTEM_HASVALUE}>/<{ONTOCAPE_UPPER_LEVEL_SYSTEM_NUMERICALVALUE}> ?Capacity .
        
        ?PowerPlant <{ONTOENERGYSYSTEM_HASWGS84LATITUDELONGITUDE}> ?LatLon .
        ?PowerPlant <{ONTOENERGYSYSTEM_HASRELEVANTPLACE}>/<{OWL_SAMEAS}> ?place .
        }}
        """

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
    queryStr = f"""
    SELECT DISTINCT ?area (GROUP_CONCAT(?areaBoundary;SEPARATOR = '***') AS ?Geo_InfoList)
    WHERE
    {{
    ?area <{RDF_TYPE}> <{ONS_STATISTICAL_GEOGRAPHY}> .
    ?area <http://publishmydata.com/def/ontology/foi/code> "{LA_code}" .
    ?area <{ONS_GEOSPARQL_HASGEOMETRY}> ?geometry .
    ?geometry <{ONS_GEOSPARQL_ASWKT}> ?areaBoundary .
    }} GROUP BY ?area
    """
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

    queryStr = f"""
    ASK  
    {{
    ?areaToBeChecked <http://publishmydata.com/def/ontology/foi/code> "{LACode_toBeCheck}" .
    ?areaGiven <http://publishmydata.com/def/ontology/foi/code> "{givenLACode}" .
    ?areaToBeChecked <{FOI_WITHIN}> ?areaGiven .
    }}
    """

    print('...query ifWithin condition...')
    res = json.loads(performQuery(endPointIRI, queryStr))  
    print('...queryifWithin is done...')
    res = res[0]['ASK']
    return res