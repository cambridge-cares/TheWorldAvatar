##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 3 Oct 2022           #
##########################################

"""
This module is used to pre-screen the protential SMR sites 
"""
from logging import raiseExceptions
from pyscipopt import Model
import os, sys, json
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
import SMRSitePreSelection.DecommissioningCost as DCost
from SMRSitePreSelection.populationDensityCalculator import populationDensityCalculator
from demandingAndCentroid import demandingAndCentroid
from sklearn.cluster import DBSCAN
import numpy as np
from shapely.geometry import MultiPoint
from UK_Digital_Twin_Package import generatorCluster as genCluster
from UK_Digital_Twin_Package.DistanceCalculator import DistanceBasedOnGPSLocation
import matplotlib.pyplot as plt
import matplotlib

'''
These are the packages of pymoo
'''
import numpy as np

from pymoo.algorithms.soo.nonconvex.ga import GA
from pymoo.core.problem import Problem
from pymoo.operators.crossover.sbx import SBX
from pymoo.operators.mutation.pm import PM
from pymoo.operators.repair.rounding import RoundingRepair
from pymoo.operators.sampling.rnd import IntegerRandomSampling, BinaryRandomSampling, FloatRandomSampling

from pymoo.problems import get_problem
from pymoo.optimize import minimize

import matplotlib.pyplot as plt
from pymoo.util import plotting

from pymoo.algorithms.moo.nsga2 import NSGA2
from pymoo.factory import get_sampling, get_crossover, get_mutation
from pymoo.optimize import minimize

from pymoo.visualization.scatter import Scatter

from pymoo.operators.crossover.pntx import TwoPointCrossover
from pymoo.operators.mutation.bitflip import BitflipMutation

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
            carbonTax:float,
            clusterFlag:bool,
            maxmumSMRUnitAtOneSite:int,
            SMRIntergratedDiscount:float, 
            startTime_of_EnergyConsumption:str,
            genTypeSummary:list,
            ## GA setting
            pop_size:int = 40,
            n_offsprings:int = 10
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
        self.clusterFlag = clusterFlag
        self.genTypeSummary = genTypeSummary
        self.ir = SMRIntergratedDiscount

        self.varSets = locals()  
        self.ieq_constr_NameList = []

        self.startTime_of_EnergyConsumption = str(startTime_of_EnergyConsumption)
        
        siteSelectionBinaryVariableNumber = len(self.generatorToBeReplacedList) * self.N

        super().__init__(
        n_var = siteSelectionBinaryVariableNumber, 
        n_obj = 2, 
        n_ieq_constr = len(self.generatorToBeReplacedList), 
        n_eq_constr = 1,
        xl=0.0,
        xu=1.0, 
        vtype=int)
        
        self.algorithm = NSGA2(
            pop_size = pop_size, ## the initial population size 
            n_offsprings = n_offsprings, ## the number of the offspring of each generation 
            sampling=IntegerRandomSampling(),
            ##  sampling=BinaryRandomSampling(),
            ##sampling=FloatRandomSampling(),
            crossover=SBX(prob=1.0, eta=3.0, vtype=float, repair=RoundingRepair()),
            mutation=PM(prob=1.0, eta=3.0, vtype=float, repair=RoundingRepair()),
            ## crossover=TwoPointCrossover(),
            ##mutation=BitflipMutation(),
            eliminate_duplicates=True)


    def _evaluate(self, x, out, *args, **kwargs):
        ##-- 1. Define the iequality constraints --##
        ## This constraint limits that for each site there is only one status (either hosting 0 or 1 or 2  or 3 or 4 SMR units) ##
        g_list = []
        for i in range(len(self.generatorToBeReplacedList)):
            g = 0 ## this is the initialisation of the constaint function for each site 
            g_Name = 'g_' + str(i) ## defining the name of the constraint function, the number of the ieq constraints is the number of the sites  
            self.ieq_constr_NameList.append(g_Name) ## recording the name of each constraint function 
            for n in self.N:
                numOfBV = int(i * self.N + n) ## the index of the binary variable x 
                g += x[:, numOfBV]
            self.varSets[g_Name] = g - 1 ## the constaint looks like: x1 + x2 + x3 + x4 <= 1, while the sum up equal to 0, it means that there is no SMR being placed
            g_list.append(self.varSets[g_Name])
        out["G"] = np.column_stack(g_list)  ## iequality constraints 


        ##--2. Define equality constraint --##
        ## This constraint limits the number of the SMR to be introduced to the system is equal to the value specified (self.numberOfSMRToBeIntroduced) ##
        h1 = 0
        for i in range(len(self.generatorToBeReplacedList)):
            for n in self.N:
                numOfBV = int(i * self.N + n) ## the index of the binary variable x 
                h1 += x[:, numOfBV] * (n + 1)
        out["H"] = h1 - int(self.numberOfSMRToBeIntroduced) ## the constaint looks like: 1 *x1 + 2*x2 + 3*x3 + 4*x4 + 1*x5 + 2*x6 + .... == numberOfSMRToBeIntroduced
     
        ##--3. Define the objective function --##
        totalSMRCapitalCost = 0
        totalLifeMonetaryCost = 0
        ## pre-calculate the cost of the SMR with integration discount
        sumUpSMRIntegratedCost = 0
        SMRIntegratedCostForDifferentInterationNumberList = []
        f2 = 0
        for n in range(self.N):
            sumUpSMRIntegratedCost += (self.ir**n ) * self.Cost_SMR * self.D / (1 - ((1 + self.D)**(-1 * self.L)))
            SMRIntegratedCostForDifferentInterationNumberList.append(sumUpSMRIntegratedCost)

        ## Objective 1: the total cost of SMR retrofitting: SMR investment and risk cost ## 
        ## Objective 2: sum up of the weighted distance between the site location and the centroid of the demanding area (demanding * distance) ## 
        for i in range(len(self.generatorToBeReplacedList)):
            sumUpOfWeightedDemanding = 0
            genIRI = self.generatorToBeReplacedList[i]["PowerGenerator"]
            latlon = self.generatorToBeReplacedList[i]["LatLon"]
            
            for demand in demandingAndCentroid[self.startTime_of_EnergyConsumption]:
                distance = DistanceBasedOnGPSLocation(latlon + demand['Geo_InfoList'])
                weightedDemanding = distance * float(demand['v_TotalELecConsumption'])  
                sumUpOfWeightedDemanding += weightedDemanding

            for n in self.N:
                rs =  (self.r0/1000) * ((n + 1) * (self.Cap_SMR**(0.5))) ## where n+1 is the number of SMR units
                print(i, genIRI)
                numOfBV = int(i * self.N + n)
                population = populationDensityCalculator(latlon, rs, self.geospatialQueryEndpointLabel)
                totalLifeMonetaryCost += x[:, numOfBV] * population * self.FP * self.Hu * self.D / (1 - ((1 + self.D)**(-1 * self.L)))  
                totalSMRCapitalCost += SMRIntegratedCostForDifferentInterationNumberList[n] * x[:, numOfBV] 

                f2 += x[:, numOfBV] * sumUpOfWeightedDemanding

        f1 = totalLifeMonetaryCost + totalSMRCapitalCost
        out["F"]  = np.column_stack([f1, f2])
    
    