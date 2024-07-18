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

class siteSelector(Problem):

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
            maxmumSMRUnitAtOneSite:int,
            SMRIntergratedDiscount:float, 
            startTime_of_EnergyConsumption:str,
            population_list:list,
            weightedDemandingDistance_list:list,
            contradictedPairs:list
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
        self.N = maxmumSMRUnitAtOneSite
        self.ir = SMRIntergratedDiscount
        self.population_list = population_list
        self.weightedDemandingDistance_list = weightedDemandingDistance_list
        self.varSets = locals()  
        self.startTime_of_EnergyConsumption = str(startTime_of_EnergyConsumption)
        siteSelectionBinaryVariableNumber = len(self.generatorToBeReplacedList) * self.N
        self.contradictedPairs = contradictedPairs
        
        super().__init__(
        n_var = siteSelectionBinaryVariableNumber, 
        n_obj = 2,  ## 'F'
        n_ieq_constr = len(self.generatorToBeReplacedList) + len(self.contradictedPairs), ## 'G' (n: for binary variables constraints; m: for safty distance between two SMR sites)
        n_eq_constr = 1, ## 'H'
        xl=0.0,
        xu=1.0, 
        vtype=int)

    def _evaluate(self, x, out, *args, **kwargs):
        ##-- 1. Define the iequality constraints --##
        g_list = []
        cp_list = []
        ## 1.1 This constraint limits that for each site there is only one status (either hosting 0 or 1 or 2  or 3 or 4 SMR units) ##
        for i in range(len(self.generatorToBeReplacedList)): 
            g = 0 
            ## g_Name = 'g_' + str(i) ## defining the name of the constraint function, the number of the ieq constraints is the number of the sites  
            for n in range(self.N):
                numOfBV = int(i * self.N + n) ## the index of the binary variable x 
                g += x[:, numOfBV]
            g = g - 1 ## the constaint looks like: x1 + x2 + x3 + x4 <= 1, while the sum up equal to 0, it means that there is no SMR being placed
            g_list.append(g)

            ## self.varSets[g_Name] = g - 1 ## the constaint looks like: x1 + x2 + x3 + x4 <= 1, while the sum up equal to 0, it means that there is no SMR being placed
            ## g_list.append(self.varSets[g_Name])

        ## 1.2 Safty distance constrain: distance_i,j >= safeDistance ## 
        for cp in self.contradictedPairs:
            g1 = 0
            g2 = 0
            for n in range(self.N):
                numOfBV1 = int(cp[0] * self.N + n) ## the index of the binary variable x 
                numOfBV2 = int(cp[1] * self.N + n) 
                g1 += x[:, numOfBV1]
                g2 += x[:, numOfBV2]
            g12 = g1 + g2 - 1
            cp_list.append(g12)
        out["G"] = np.column_stack(g_list + cp_list)  ## iequality constraints 

        ##--2. Define equality constraint --##
        ##This constraint limits the number of the SMR to be introduced to the system is equal to the value specified (self.numberOfSMRToBeIntroduced) ##     
        h1 = 0
        for i in range(len(self.generatorToBeReplacedList)):
            for n in range(self.N):
                numOfBV = int(i * self.N + n) ## the index of the binary variable x 
                h1 += x[:, numOfBV] * (n + 1)
        out["H"] = h1 - int(self.numberOfSMRToBeIntroduced) ## the constaint looks like: 1 *x1 + 2*x2 + 3*x3 + 4*x4 + 1*x5 + 2*x6 + .... == numberOfSMRToBeIntroduced
     
        ##--3. Define the objective function --##
        totalSMRCapitalCost = 0
        totalLifeMonetaryCost = 0
        f2 = 0
        ## pre-calculate the cost of the SMR with integration discount
        sumUpSMRIntegratedCost = 0
        SMRIntegratedCostForDifferentInterationNumberList = []
        for n in range(self.N):
            sumUpSMRIntegratedCost += (self.ir**n ) * self.Cost_SMR * self.D / (1 - ((1 + self.D)**(-1 * self.L)))
            SMRIntegratedCostForDifferentInterationNumberList.append(sumUpSMRIntegratedCost)

        ## Objective 1: the total cost of SMR retrofitting: SMR investment and risk cost ## 
        ## Objective 2: demanding weighter of each potential site ## 
        for i in range(len(self.generatorToBeReplacedList)):
            for n in range(self.N):
                numOfBV = int(i * self.N + n)
                population = self.population_list[i][n]
                totalLifeMonetaryCost += x[:, numOfBV] * population * self.FP * self.Hu * self.D / (1 - ((1 + self.D)**(-1 * self.L)))  
                totalSMRCapitalCost += SMRIntegratedCostForDifferentInterationNumberList[n] * x[:, numOfBV] 
                f2 += x[:, numOfBV] * self.weightedDemandingDistance_list[i]
        f1 = totalLifeMonetaryCost + totalSMRCapitalCost
        out["F"]  = np.column_stack([f1, f2])

