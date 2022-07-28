##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 26 July 2022         #
##########################################

"""
This module is used to pre-screen the protential SMR sites 
"""
from logging import raiseExceptions
from tkinter import S
from pyrsistent import b
from pyscipopt import Model
import os, sys, json
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
import SMRSitePreSelection.DiscommissioningCost as DC
from SMRSitePreSelection.populationDensityCalculator import populationDensityCalculator
from math import pi

class SitePreSelection(object):

    def __init__(self, 
            geospatialQueryEndpointLabel:str,    
            generatorToBeReplacedList:list, ## the list should contain the [0]PowerGenerator, [1]Bus, [2]Capacity, [3]LatLon, [4]fuelOrGenType, [5]annualOperatingHours, [6] CO2EmissionFactor
            discountRate:float, 
            projectLifeSpan:float,
            SMRCapitalCost:float,
            MonetaryValuePerHumanLife:float,
            NeighbourhoodRadiusForSMRUnitOf1MW:float,
            ProbabilityOfReactorFailure:float,
            SMRCapability:float,
            demandCapacityRatio:float,
            bankRate:float,
            carbonTax:float,
            shutNonRetrofittedGenerator:float,
            maxmumSMRUnitAtOneSite:int,
            DiscommissioningCostEstimatedLevel:int = 1 ## the number 0 indicates the using the minimum decommissioning cost, while 1 for middle and 2 for high
        ):
        ##-- Model Parameters --##  
        self.generatorToBeReplacedList = generatorToBeReplacedList
        self.D = discountRate
        self.L = projectLifeSpan
        self.Cost_SMR = SMRCapitalCost
        self.Cap_SMR = SMRCapability
        self.Hu = MonetaryValuePerHumanLife
        self.r0 = NeighbourhoodRadiusForSMRUnitOf1MW
        self.FP = ProbabilityOfReactorFailure
        self.capRatio = demandCapacityRatio    
        self.geospatialQueryEndpointLabel = geospatialQueryEndpointLabel
        self.i = bankRate
        self.carbonTax = carbonTax
        self.N = maxmumSMRUnitAtOneSite
        self.shutNonRetrofittedGenerator = shutNonRetrofittedGenerator

        if DiscommissioningCostEstimatedLevel in [0,1,2]:
            self.DcLevel = DiscommissioningCostEstimatedLevel
        else:
            raiseExceptions("Discommissioning Cost Estimated Level must be 0 for minimum cost, 1 for middle and 2 for high.")


    def SMRSitePreSelector(self):
        ##-- Setup model --##
        self.model = Model("SMRSitePreSelection")

        ##-- Binary variable --##
        self.varSets = locals()  
        self.binaryVarNameList = []
        for s in range(len(self.generatorToBeReplacedList)):
            sumOfBinaryVar = 0
            binaryVarNameShortList = []
            for i in range(self.N + 1):
                binaryVarName = "y_" + str(s) + "_" + str(i) 
                self.varSets[binaryVarName] = self.model.addVar(binaryVarName, vtype = "B")
                binaryVarNameShortList.append(binaryVarName)
                sumOfBinaryVar += self.varSets[binaryVarName]
            self.binaryVarNameList.append(binaryVarNameShortList)
            ## the binary variables constrians Sum(i, (0，4)) y_s_i = 1, where y_s_0 = 1 identifying there is no SMR in the site s ##  
            self.model.addCons(sumOfBinaryVar == 1 )

        ##-- Set up constraint --##
        ## 1. the replacedCapacity
        replacedCapacity = 0
        if self.shutNonRetrofittedGenerator is True:
            for gen in self.generatorToBeReplacedList:
                replacedCapacity += float(gen["Capacity"])
            replacedCapacity = float(self.capRatio) * float(replacedCapacity)
        else: 
            for s in range(len(self.generatorToBeReplacedList)):
                gen = self.generatorToBeReplacedList[s]
                bvList = self.binaryVarNameList[s]
                for bvname in bvList:
                    if bvList.index(bvname) > 0:
                        replacedCapacity += float(gen["Capacity"]) * self.varSets[bvname]
                
        ## 2. the total capacity of SMR
        totalSMRCapacity = 0
        for bvList in self.binaryVarNameList:
            for bv in bvList:
                i = bvList.index(bv)
                totalSMRCapacity += i * self.varSets[bv] * self.Cap_SMR
        ## 3. SMR capacity constraint
        self.model.addCons(totalSMRCapacity >= replacedCapacity, name = "SMR capacity constraint")

        ##-- Formulate the objective function --##
        totalSMRCapitalCost = 0
        totalDiscommissioningCost = 0
        carbonCost = 0
        totalProtentialCarbonCost = 0
        totalLifeMonetaryCost = 0
        
        for s in range(len(self.binaryVarNameList)):
            bvList = self.binaryVarNameList[s]
            existingGenCap = self.generatorToBeReplacedList[s]["Capacity"]
            existingGenFuelType = self.generatorToBeReplacedList[s]["fuelOrGenType"]
            annualOperatingHours = self.generatorToBeReplacedList[s]["annualOperatingHours"]
            CO2EmissionFactor = self.generatorToBeReplacedList[s]["CO2EmissionFactor"]

            if existingGenFuelType in DC.DiscommissioningCost.keys():
                dc = DC.DiscommissioningCost[existingGenFuelType][self.DcLevel]
            else:
                raise Exception("Cannot find the decommissioning cost for", existingGenFuelType)
            
            ## the protential carbon emission cost if the old generator is not being replaced by SMR
            for l in range(self.L):
                ## l starts frm 0, therefore it is no need to use -(l-1) bus just use -l
                carbonCost += float(existingGenCap) * float(CO2EmissionFactor) * float(annualOperatingHours) * float(self.carbonTax) * (1 + float(self.i)) **(-l)

            ## calculte the Neighbourhood radius for SMR unit and the population within the circle centred at the to be replaced generator with the radius rs
            for bvname in bvList:
                i = bvList.index(bvname)
                bv = self.varSets[bvname]
                totalSMRCapitalCost += int(i) * bv * self.Cost_SMR * self.D / (1 - ((1 + self.D)**(-1 * self.L))) 
                if i == 0:
                    totalDiscommissioningCost += (1-bv) * existingGenCap * dc * self.D / (1 - ((1 + self.D)**(-1 * self.L)))
                    totalProtentialCarbonCost += bv * carbonCost * self.D / (1 - ((1 + self.D)**(-1 * self.L)))
                else: 
                    rs =  (self.r0/1000) * (i * self.Cap_SMR)  #**(0.5)##TODO: check the population of each rs, change the y00
                    population = populationDensityCalculator(self.generatorToBeReplacedList[s]["LatLon"], rs, self.geospatialQueryEndpointLabel)
                    totalLifeMonetaryCost += bv * population * self.FP * self.Hu * self.D / (1 - ((1 + self.D)**(-1 * self.L)))
                
        ##-- Set up the objective function --##
        self.model.setObjective(totalSMRCapitalCost + totalDiscommissioningCost + totalProtentialCarbonCost + totalLifeMonetaryCost, "minimize")

        ##-- Set up optimisation method --##
        self.model.optimize()

        ##-- Results post processing --##
        print("Optimal value:", self.model.getObjVal())
        totalSMR = 0
        self.siteSelected = []
        for s in range(len(self.binaryVarNameList)):
            bvList = self.binaryVarNameList[s]
            numOfSMR = 0
            for bvname in bvList:
                print((self.varSets[bvname].name), " = ", (self.model.getVal(self.varSets[bvname])))
                totalSMR += self.model.getVal(self.varSets[bvname]) * bvList.index(bvname)
                numOfSMR += self.model.getVal(self.varSets[bvname]) * bvList.index(bvname)
            if numOfSMR > 1:
                self.generatorToBeReplacedList[s].update({"numberOfSMR": numOfSMR})
                self.siteSelected.append(self.generatorToBeReplacedList[s]) 
        totalSMRCapa = totalSMR * 470
        
        #print(self.siteSelected, len(self.siteSelected))
        capa = 0
        for gen in self.siteSelected:
            capa += float(gen["Capacity"])
        print('Replaced capacity', capa)
        print('The totalSMR is', totalSMR, 'totalSMRCapa', totalSMRCapa)

        return
      
        
if __name__ == '__main__': 
    ##NOTUSED [0]generator IRI, [1]capcacity, [2]primary fuel, [3]generaor technology, [4]lat-lon 
    GenCoal = [{'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_3b7acf93-cc8b-4ad7-9c85-2f737eace679', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_ebace1f4-7d3a-44f6-980e-a4b844de670b', 'Capacity': '1559.0', 'LatLon': [51.38731, -3.4049], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 482.06, 'CO2EmissionFactor': 0.319, 'place': 'https://dbpedia.org/page/Wales'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_7bda0d1f-8df0-496c-8f7a-b328c70ffe2d', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_2d76797b-c638-460e-b73c-769e29785466', 'Capacity': '2000.0', 'LatLon': [53.304, -0.7815], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 482.06, 'CO2EmissionFactor': 0.319, 'place': 'https://dbpedia.org/page/East_Midlands'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_598cdc45-52b8-4027-888f-3eb4758b329c', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_84f6905c-d4cb-409f-861f-ea66fe25ddd0', 'Capacity': '230.0', 'LatLon': [51.54907, -2.97053], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 482.06, 'CO2EmissionFactor': 0.319, 'place': 'https://dbpedia.org/page/Yorkshire_and_the_Humber'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_c2153e52-dee7-49d8-9ba2-90f83edfde5f', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_2d76797b-c638-460e-b73c-769e29785466', 'Capacity': '2000.0', 'LatLon': [53.36046, -0.81019], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 482.06, 'CO2EmissionFactor': 0.319, 'place': 'https://dbpedia.org/page/East_Midlands'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_a04e0e57-f847-426b-9cc3-c323d261aecd', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_2d76797b-c638-460e-b73c-769e29785466', 'Capacity': '2021.0', 'LatLon': [52.86463, -1.25829], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 482.06, 'CO2EmissionFactor': 0.319, 'place': 'https://dbpedia.org/page/East_Midlands'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_b32946fc-6abb-4df5-9437-7e01dbe1ca64', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_c4d7dcca-a7f5-4887-a460-31706ab7ec9c', 'Capacity': '1961.0', 'LatLon': [53.37234, -2.68912], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 482.06, 'CO2EmissionFactor': 0.319, 'place': 'https://dbpedia.org/page/North_West_England'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_74fbe81f-339b-41fe-b2da-940d05b774b2', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_84f6905c-d4cb-409f-861f-ea66fe25ddd0', 'Capacity': '1320.0', 'LatLon': [53.74043, -0.9981], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 482.06, 'CO2EmissionFactor': 0.319, 'place': 'https://dbpedia.org/page/Yorkshire_and_the_Humber'}]
    test = [{"PowerGenerator": 1, "Bus": 1, "Capacity": 10, "fuelOrGenType": "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas", "LatLon":"52.209556#0.120046", "CO2EmissionFactor": 0.181, "annualOperatingHours": 3593.48},
    {"PowerGenerator": 1, "Bus": 1, "Capacity":500, "fuelOrGenType": "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal", "LatLon":"52.209556#0.120046", "CO2EmissionFactor": 0.319, "annualOperatingHours": 482.06}]
    test = SitePreSelection('ukdigitaltwin_pd', GenCoal, 0.02, 40, 1800000000, 2400000, 200, 0.002985, 470, 0.5, 0.0125, 100, False, 4, 1)
    test.SMRSitePreSelector()
    print(test.siteSelected)
   



       
       


