##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 12 July 2022         #
##########################################

"""
This module is used to pre-screen the protential SMR sites 
"""
from logging import raiseExceptions
from pyscipopt import Model
from DiscommissioningCost import DiscommissioningCost
from populationDensityCalculator import populationDensityCalculator
from math import pi

class SMRSitePreSelection(object):

    def __init__(self, 
            geospatialQueryEndpointLabel:str,    
            generatorToBeReplacedList:list, ## the list should contain the [0]generator IRI, [1]capcacity, [2]primary fuel, [3]generaor technology, [4]lat-lon
            discountRate:float, 
            projectLifeSpan:float,
            SMRCapitalCost:float,
            MonetaryValuePerHumanLife:float,
            NeighbourhoodRadiusForSMRUnitOf1MW:float,
            ProbabilityOfReactorFailure:float,
            SMRCapability:float,
            demandCapacityRatio:float,
            DiscommissioningCostEstimatedLevel:int = 1 ## the number 0 indicates the using the minimum decommissioning cost, while 1 for middle and 2 for high
        ):

        ## Model Parameters  
        self.generatorToBeReplacedList = generatorToBeReplacedList
        self.D = discountRate
        self.L = projectLifeSpan
        self.Cost_SMR = SMRCapitalCost
        self.Cap_SMR = SMRCapability
        self.Hu = MonetaryValuePerHumanLife
        self.r0 = NeighbourhoodRadiusForSMRUnitOf1MW
        self.FP = ProbabilityOfReactorFailure
        self.ratio = demandCapacityRatio,    
        self.geospatialQueryEndpointLabel = geospatialQueryEndpointLabel

        if DiscommissioningCostEstimatedLevel in [0,1,2]:
            self.DcLevel = DiscommissioningCostEstimatedLevel
        else:
            raiseExceptions("Discommissioning Cost Estimated Level must be 0 for minimum cost, 1 for middle and 2 for high.")


    def SMRSitePreSelection(self):
        ##-- Setup model --##
        self.model = Model("SMRSitePreSelection")
   
        ##-- Binary variable --##
        self.varSets = locals()  
        self.binaryVarNameList = []
        for s in range(len(self.generatorToBeReplacedList)):
            binaryVarName = "y_" + str(s)
            self.varSets[binaryVarName] = self.model.addVar(binaryVarName, vtype = "B")
            self.binaryVarNameList.append(binaryVarName)
        
        ##-- Set up constraint for number of heads --##
        ## 1. the replacedCapacity
        replacedCapacity = 0
        for gen in self.generatorToBeReplacedList:
            replacedCapacity += gen[1] ##FIXME: the capacity of the existing generator located in site s
        replacedCapacity = self.ratio * replacedCapacity
        ## 2. the total capacity of SMR
        totalSMRCapacity = 0
        for bv in self.binaryVarNameList:
            totalSMRCapacity += self.varSets[bv] * self.Cap_SMR
        ## 3. SMR capacity constraint
        self.model.addCons(totalSMRCapacity >= replacedCapacity, name="SMR capacity constraint")

        ##--Formulate the objective function --##
        totalSMRCapitalCost = 0
        totalDiscommissioningCost = 0
        totalLifeMonetaryCost = 0
        generatorTypeKeys = DiscommissioningCost.keys()
        ## the neighbourhood radius of SMR 
        rs = self.r0 * self.Cap_SMR **(0.5)
        for s in range(len(self.binaryVarNameList)):
            bv = self.varSets[self.binaryVarNameList[s]]
            existingGenCap = self.generatorToBeReplacedList[s][1] ##FIXME: the capacity index
            existingGenFuelType = self.generatorToBeReplacedList[s][2] ##FIXME: the index
            existingGenTechType = self.generatorToBeReplacedList[s][3] ##FIXME: the index
            if existingGenFuelType in generatorTypeKeys:
                DC = DiscommissioningCost[existingGenFuelType][self.DcLevel]
            elif existingGenTechType in generatorTypeKeys:
                DC = DiscommissioningCost[existingGenTechType][self.DcLevel]

            
            ## the population within the circle centred at the to be replaced generator with the radius rs
            ## FIXME: check the latlon str, should has '#' in the middle
            population = populationDensityCalculator(self.generatorToBeReplacedList[s][4], rs, self.geospatialQueryEndpointLabel)
            
            totalSMRCapitalCost += bv * self.Cost_SMR * self.D / (1 - ((1 + self.D)**(-1 * self.L)))
            totalDiscommissioningCost += bv * existingGenCap * DC
            totalLifeMonetaryCost += bv * population * self.FP * self.Hu * self.D / (1 - ((1 + self.D)**(-1 * self.L)))
    ## TODO: upload ttl, test geospatical
        ##--Set up the objective function --##
        self.model.setObjective(totalSMRCapitalCost + totalDiscommissioningCost + totalLifeMonetaryCost, "minimize")

        ##--Set up optimisation method --##
        self.model.optimize()

        print("Optimal value:", self.model.getObjVal())

        for var in self.binaryVarNameList:
            print((var.name), " = ", (self.model.getVal(var)))
    
        return ## return the site selection results

      
        
if __name__ == '__main__':  
    test = SMRSitePreSelection([1,2,3], 1, 2, 3, 4, 5, 6, 7)
    for var in test.varSets:
        print(var)
    print(test.x)
    # print(test.model.getVal(test.x))



       
       


