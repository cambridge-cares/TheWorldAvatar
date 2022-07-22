##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 22 July 2022         #
##########################################

"""
This module is used to pre-screen the protential SMR sites 
"""
from logging import raiseExceptions
from tkinter import S
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
   
        ##-- Binary and Integer variable --##
        self.varSets = locals()  
        self.binaryVarNameList = []
        self.integerVarNameList = []
        for s in range(len(self.generatorToBeReplacedList)):
            binaryVarName = "y_" + str(s)
            integerVarName = "n_" + str(s)
            self.varSets[binaryVarName] = self.model.addVar(binaryVarName, vtype = "B")
            self.varSets[integerVarName] = self.model.addVar(integerVarName, vtype = "I",  lb = 0, ub = None)
            self.binaryVarNameList.append(binaryVarName)
            self.integerVarNameList.append(integerVarName)
            ## the binary and integer variables constrians M * y >= n > = y ## 
            self.model.addCons(self.varSets[binaryVarName] * self.N >= self.varSets[integerVarName])
            self.model.addCons(self.varSets[integerVarName] >= self.varSets[binaryVarName])

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
                bv = self.binaryVarNameList[s]
                replacedCapacity += float(gen["Capacity"]) * self.varSets[bv]
                
        ## 2. the total capacity of SMR
        totalSMRCapacity = 0
        for bv in self.binaryVarNameList:
            i = self.binaryVarNameList.index(bv)
            iv = self.integerVarNameList[i]
            totalSMRCapacity += self.varSets[iv] * self.Cap_SMR
        ## 3. SMR capacity constraint
        self.model.addCons(totalSMRCapacity >= replacedCapacity, name = "SMR capacity constraint")

        ##-- Formulate the objective function --##
        totalSMRCapitalCost = 0
        totalDiscommissioningCost = 0
        carbonCost = 0
        totalProtentialCarbonCost = 0
        totalLifeMonetaryCost = 0
        
        for s in range(len(self.binaryVarNameList)):
            bv = self.varSets[self.binaryVarNameList[s]]
            iv = self.varSets[self.integerVarNameList[s]]

            ## FIXME: the neighbourhood radius of SMR , this redius relates with the total capcacity of the SMR located in the same site
            ## therefore, rs is a variable instead of a constance
            rs = iv * self.r0 * self.Cap_SMR **(0.5)

            existingGenCap = self.generatorToBeReplacedList[s]["Capacity"]
            existingGenFuelType = self.generatorToBeReplacedList[s]["fuelOrGenType"]
            annualOperatingHours = self.generatorToBeReplacedList[s]["annualOperatingHours"]
            CO2EmissionFactor = self.generatorToBeReplacedList[s]["CO2EmissionFactor"]

            if existingGenFuelType in DC.DiscommissioningCost.keys():
                dc = DC.DiscommissioningCost[existingGenFuelType][self.DcLevel]
            else:
                raise Exception("Cannot find the decommissioning cost for", existingGenFuelType)
            
            ## the population within the circle centred at the to be replaced generator with the radius rs
            ## TODO: test population, test geospatical
            ## FIXME: as rs is a variable, it cannot become an input to a query, may need the pre-calculate values, for 1*redius, 2*redius...
            # population = populationDensityCalculator(self.generatorToBeReplacedList[s]["LatLon"], rs, self.geospatialQueryEndpointLabel)
            
            ## the protential carbon emssion cost if the old generator is not being replaced by SMR
            for l in range(self.L):
                carbonCost += float(existingGenCap) * float(CO2EmissionFactor) * float(annualOperatingHours) * float(self.carbonTax) * (1 + float(self.i)) **(-(l - 1))

            totalSMRCapitalCost += iv * self.Cost_SMR * self.D / (1 - ((1 + self.D)**(-1 * self.L)))
            totalDiscommissioningCost += bv * existingGenCap * dc * self.D / (1 - ((1 + self.D)**(-1 * self.L)))
            totalProtentialCarbonCost += (1-bv) * carbonCost * self.D / (1 - ((1 + self.D)**(-1 * self.L)))

            # totalLifeMonetaryCost += bv * population * self.FP * self.Hu * self.D / (1 - ((1 + self.D)**(-1 * self.L)))

        ##-- Set up the objective function --##
        self.model.setObjective(totalSMRCapitalCost + totalDiscommissioningCost + totalProtentialCarbonCost, "minimize")

        ##-- Set up optimisation method --##
        self.model.optimize()

        ##-- Results post processing --##
        print("Optimal value:", self.model.getObjVal())
        totalSMR = 0
        self.siteSelected = []
        for s in range(len(self.binaryVarNameList)):
            y = self.binaryVarNameList[s]
            n = self.integerVarNameList[s]
            print((self.varSets[y].name), " = ", (self.model.getVal(self.varSets[y])))
            print((self.varSets[n].name), " = ", (self.model.getVal(self.varSets[n])))

            # if self.model.getVal(self.varSets[n]) > 1:
            #     print(n, self.model.getVal(self.varSets[n]))

            totalSMR += self.model.getVal(self.varSets[n])

            if self.model.getVal(self.varSets[y]) > 0:
                self.generatorToBeReplacedList[s].update({"numberOfSMR": self.model.getVal(self.varSets[n])})
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
    test = SitePreSelection(None, [{"PowerGenerator": 1, "Bus": 1, "Capacity":10, "fuelOrGenType": "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas", "LatLon":"52.209556#0.120046", "CO2EmissionFactor": 0.181, "annualOperatingHours": 3593.48},
    {"PowerGenerator": 1, "Bus": 1, "Capacity":500, "fuelOrGenType": "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal", "LatLon":"52.209556#0.120046", "CO2EmissionFactor": 0.319, "annualOperatingHours": 482.06}],
     0.02, 40, 1800000000, 2400000, 200, 0.002985, 470, 0.5, 0.0125, 100, False, 3, 1)
    test.SMRSitePreSelector()
    print(test.siteSelected)
   



       
       


