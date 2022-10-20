##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 11 Oct 2022          #
##########################################

"""
This module is used to pre-screen the protential SMR sites 
"""
from logging import raiseExceptions
import os, sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from SMRSitePreSelection.populationDensityCalculator import populationDensityCalculator
import SMRSitePreSelection.demandingAndCentroidList as dclist
import numpy as np
from UK_Digital_Twin_Package.DistanceCalculator import DistanceBasedOnGPSLocation
from pymoo.core.problem import Problem
from testOPFAnalysis import queryOPFInput 
from colour import Color

def sortKey(e):
    return e['OBJ']

def rankingJSON(rankingResult, file_label):
    green = Color("green")
    colors = list(green.range_to(Color("red"),len(rankingResult)))
    geojson_file = """
    {
        "type": "FeatureCollection",
        "features": ["""
    for i in range(len(rankingResult)):
        rank = rankingResult[i]
        feature = """{
            "type": "Feature",
            "properties": {
            "Fuel Type": "%s",
            "Capacity": "%s",
            "SMR cost and risk cost": "%s",
            "Weighed demanding distance": "%s",
            "weighter": "%s", 
            "marker-color": "%s",
            "Rank number": "%s",
            "marker-size": "medium",
            "marker-symbol": ""
            },
            "geometry": {
            "type": "Point",
            "coordinates": [
                %s,
                %s
            ]
            }
            },"""%(rank["fuelOrGenType"], rank["Capacity"], rank["Obj1"], rank["Obj2"], rank["weighter"], str(colors[i]), str(i + 1), rank["latlon"][1], rank["latlon"][0])
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
    geojson_written = open(file_label +'.geojson','w')
    geojson_written.write(geojson_file)
    geojson_written.close() 
    print('---GeoJSON written successfully---', file_label)
    return

class Ranking():

    def __init__(self, 
            numberOfSMRToBeIntroduced:int,
            geospatialQueryEndpointLabel:str,    
            generatorToBeReplacedList:list, ## the list should contain the [0]PowerGenerator, [1]Bus, [2]Capacity, [3]LatLon, [4]fuelOrGenType, [5]annualOperatingHours, [6] CO2EmissionFactor
            discountRate:float, 
            projectLifeSpan:float,
            SMRCapitalCost:float,
            MonetaryValuePerHumanLife:float,
            NeighbourhoodRadiusForSMRUnitOf1MW:float,
            ProbabilityOfReactorFailure:float,
            SMRCapability:float,
            bankRate:float,
            carbonTax:float,
            maxmumSMRUnitAtOneSite:int,
            SMRIntergratedDiscount:float, 
            startTime_of_EnergyConsumption:str,
            population_list:list
        ):
        ##-- Model Parameters --##  
        self.numberOfSMRToBeIntroduced = int(numberOfSMRToBeIntroduced)
        self.generatorToBeReplacedList = generatorToBeReplacedList
        self.D = discountRate
        self.L = projectLifeSpan
        self.Cost_SMR = SMRCapitalCost
        self.Cap_SMR = SMRCapability
        self.Hu = MonetaryValuePerHumanLife
        self.r0 = NeighbourhoodRadiusForSMRUnitOf1MW
        self.FP = ProbabilityOfReactorFailure
        self.geospatialQueryEndpointLabel = geospatialQueryEndpointLabel
        self.i = bankRate
        self.carbonTax = carbonTax
        self.N = maxmumSMRUnitAtOneSite
        self.ir = SMRIntergratedDiscount
        self.population_list = population_list

        self.varSets = locals()  

        self.startTime_of_EnergyConsumption = str(startTime_of_EnergyConsumption)
        
        siteSelectionBinaryVariableNumber = len(self.generatorToBeReplacedList) * self.N

       
    def rankSite(self, weighter:int):
        ## pre-calculate the cost of the SMR with integration discount
        sumUpSMRIntegratedCost = 0
        SMRIntegratedCostForDifferentInterationNumberList = []
        for n in range(self.N):
            sumUpSMRIntegratedCost += (self.ir**n ) * self.Cost_SMR * self.D / (1 - ((1 + self.D)**(-1 * self.L)))
            SMRIntegratedCostForDifferentInterationNumberList.append(sumUpSMRIntegratedCost)

        ranking = []
        for i in range(len(self.generatorToBeReplacedList)):
            totalSMRCapitalCost = 0
            totalLifeMonetaryCost = 0
            gen = self.generatorToBeReplacedList[i]
            for n in range(self.N):
                population = self.population_list[i][n]
                totalLifeMonetaryCost += population * self.FP * self.Hu * self.D / (1 - ((1 + self.D)**(-1 * self.L)))  
                totalSMRCapitalCost += SMRIntegratedCostForDifferentInterationNumberList[n]

            sumUpOfWeightedDemanding = 0
            latlon = gen["LatLon"]
            fuelOrGenType = gen["fuelOrGenType"].split('#')[1]
            capa = float(gen["Capacity"])
            
            for demand in dclist.demandingAndCentroid[self.startTime_of_EnergyConsumption]:
                LA_code  = demand["Area_LACode"]
                if LA_code in ["K03000001", "K02000001", "W92000004","S92000003", "E12000001", "E12000002", "E12000003", "E12000004", "E12000005", 
                                "E12000006", "E12000007", "E12000008", "E12000009", "E13000001", "E13000002"]:
                    continue
                distance = DistanceBasedOnGPSLocation(latlon + demand['Geo_InfoList'])
                weightedDemanding = distance * float(demand['v_TotalELecConsumption'])  
                sumUpOfWeightedDemanding += weightedDemanding

            
            ## ranking.append([latlon, fuelOrGenType, capa, (totalLifeMonetaryCost + totalSMRCapitalCost) / (10E7), sumUpOfWeightedDemanding / (10E7)])

            obj1 = (totalLifeMonetaryCost + totalSMRCapitalCost) / (10E8)
            obj2 = sumUpOfWeightedDemanding / (10E8)

            obj = obj1 * weighter + obj2

            ranking.append({"index": i, "latlon": latlon, "fuelOrGenType": fuelOrGenType, "Capacity": capa, "Obj1": obj1, "Obj2": obj2, "weighter": weighter, "OBJ": obj})

        ranking.sort(key = sortKey)

        index = []
        for r in ranking:
            index.append(r["index"])
        return ranking, index


if __name__ == '__main__':
    topologyNodeIRI_10Bus = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerGridTopology_b22aaffa-fd51-4643-98a3-ff72ee04e21e" 
    topologyNodeIRI_29Bus = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerGridTopology_6017554a-98bb-4896-bc21-e455cb6b3958" 
    
    AgentIRI = "http://www.example.com/triplestore/agents/Service__XXXAgent#Service"
    slackBusNodeIRI = "http://www.theworldavatar.com/kb/ontopowsys/BusNode_1f3c4462-3472-4949-bffb-eae7d3135591"  ## the slack bus is recongnised by its latlon
    slackBusNodeIRI_29Bus = "http://www.theworldavatar.com/kb/ontopowsys/BusNode_bc386bcb-33ab-4569-80c5-00dc9d0bffb8"  
    queryEndpointIRI = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test2/sparql"
    ## queryEndpointLabel = "ukdigitaltwin_test2"
    geospatialQueryEndpointLabel = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_pd/sparql"
    updateEndPointURL = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test3/sparql"
    loadAllocatorName = "regionalDemandLoad"
    loadAllocatorName_29Bus = "closestDemandLoad"

    EBusModelVariableInitialisationMethodName = "defaultInitialisation"
    EBusModelVariableInitialisationMethodName_29Bus = "preSpecified"

    ELineInitialisationMethodName = "defaultBranchInitialiser"
    ELineInitialisationMethodName_29Bus = "preSpecifiedBranchInitialiser"

    CarbonTaxForSMRSiteSelection = 10
    CarbonTaxForOPFList = [60, 80, 100, 150, 0, 20, 40 ]  #[0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 150, 165, 180, 250] 
    piecewiseOrPolynomial = 2
    pointsOfPiecewiseOrcostFuncOrder = 2
    baseMVA = 150
    withRetrofit = True
    newGeneratorType = "SMR"
    retrofitGenerator = []
    retrofitGenerationFuelOrTechType = ["http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas", 
     "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal", 
     "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Oil",
     "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#SourGas"]
  
    pureBackUpAllGenerator = False
    replaceRenewableGenerator = False
    clusterFlag = False

    discountRate = 0.02
    bankRate = 0.0125
    projectLifeSpan = 40
    SMRCapitalCost = 1800000000
    MonetaryValuePerHumanLife = 2400000
    NeighbourhoodRadiusForSMRUnitOf1MW = 200
    ProbabilityOfReactorFailure = 0.002985
    SMRCapability = 470
    maxmumSMRUnitAtOneSite = 1
    SMRIntergratedDiscount = 0.9
    backUpCapacityRatio = 0.7
    ## TODO: modify this for different scenarios
    windOutputRatio = 0.47
    solarOutputRatio = 0.44
    DecommissioningCostEstimatedLevel = 1

    numberOfSMRToBeIntroduced = 25
    pop_size = 400
    n_offsprings = 500 
    numberOfGenerations = 150

    CarbonTaxForOPF = 10

    startTime_of_EnergyConsumption = "2017-01-31" 

    weightList = [0.001, 0.01, 0.1] ##  [20,50,80]# [1, 2, 5, 8, 10, 20, 50, 80, 100, 1000, 10000]

    retrofitListBeforeSelection, _ = queryOPFInput.queryGeneratorToBeRetrofitted_SelectedFuelOrGenerationTechnologyType(retrofitGenerationFuelOrTechType, topologyNodeIRI_29Bus, queryEndpointIRI)
    ## pre-calculation of each site surrounding population density
    rs_List = []
    for n in range(maxmumSMRUnitAtOneSite):
        rs =  (NeighbourhoodRadiusForSMRUnitOf1MW/1000) * ((n + 1) * (SMRCapability**(0.5)))
        rs_List.append(rs)
    population_list = []
    for s in range(len(retrofitListBeforeSelection)):
        latlon = retrofitListBeforeSelection[s]["LatLon"]
        populationListForOneSite = []
        for rs in rs_List:
            population = populationDensityCalculator(latlon, rs, geospatialQueryEndpointLabel)
            populationListForOneSite.append(population)
        population_list.append(populationListForOneSite)

    indexList = []
    rank_test = Ranking(numberOfSMRToBeIntroduced,
            geospatialQueryEndpointLabel,    
            retrofitListBeforeSelection, ## the list should contain the [0]PowerGenerator, [1]Bus, [2]Capacity, [3]LatLon, [4]fuelOrGenType, [5]annualOperatingHours, [6] CO2EmissionFactor
            discountRate, 
            projectLifeSpan,
            SMRCapitalCost,
            MonetaryValuePerHumanLife,
            NeighbourhoodRadiusForSMRUnitOf1MW,
            ProbabilityOfReactorFailure,
            SMRCapability,
            bankRate,
            CarbonTaxForOPF,
            maxmumSMRUnitAtOneSite,
            SMRIntergratedDiscount, 
            startTime_of_EnergyConsumption,
            population_list)
    for weight in weightList:
        re, index = rank_test.rankSite(weight)
        indexList.append(index)
        rankingJSON(re, 'ranking_' + str(maxmumSMRUnitAtOneSite) + '_SMRUnitPerSite_Weighter_' + str(weight))
    
    print(indexList)
