##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 28 Sept 2022         #
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
from sklearn.cluster import DBSCAN
import numpy as np
from shapely.geometry import MultiPoint
from UK_Digital_Twin_Package import generatorCluster as genCluster
import matplotlib.pyplot as plt
import matplotlib

class SitePreSelectionAndCapacityReplacement(object):

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
            backUpCapacityRatio:float,
            bankRate:float,
            carbonTax:float,
            pureBackUpAllGenerator:bool,
            replaceRenewableGenerator:bool,
            clusterFlag:bool,
            maxmumSMRUnitAtOneSite:int,
            SMRIntergratedDiscount:float, 
            genTypeSummary:list,
            DecommissioningCostEstimatedLevel:int = 1, ## the number 0 indicates the using the minimum decommissioning cost, while 1 for middle and 2 for high
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
        self.backUpCapRatio = backUpCapacityRatio    
        self.geospatialQueryEndpointLabel = geospatialQueryEndpointLabel
        self.i = bankRate
        self.carbonTax = carbonTax
        self.N = maxmumSMRUnitAtOneSite
        self.pureBackUpAllGenerator = pureBackUpAllGenerator
        self.replaceRenewableGenerator = replaceRenewableGenerator
        self.clusterFlag = clusterFlag
        self.genTypeSummary = genTypeSummary
        self.ir = SMRIntergratedDiscount

        if DecommissioningCostEstimatedLevel in [0, 1 , 2, 3]:
            self.DcLevel = DecommissioningCostEstimatedLevel
        else:
            raiseExceptions("DecommissioningCost Estimated Level must be 0 for no cost, and 1 for minimum cost, 2 for middle and 3 for high cost level.")
        
        self.totalRenewableCapacity = 0
        for gen in self.generatorToBeReplacedList: 
            if 'Solar' in gen["fuelOrGenType"] or 'Wind' in gen["fuelOrGenType"]:  
                self.totalRenewableCapacity += float(gen["Capacity"])

    def SitePreSelectorAndCapacityReplacementFinder(self):
        ##-- Setup model --##
        self.model = Model("SitePreSelectorAndCapacityReplacementFinder")

        ##--Site Pre-cluster --##
        if self.clusterFlag:
            print('...Renewable generator clustering starts...')
            self.siteCluster()

        ##-- Binary variable --##
        self.varSets = locals()  
        self.CapacityReplacementBinaryVarNameList = [] ## identify if the capacity is replaced by SMR, does not nacessarily means that the plant has to be decommissioned
        self.siteSelectionBinaryVarNameList = [] ## identify if the site is selected to host the SMR. The site whose plant's capacity being replaced by SMR does not nacessarily becomes the site hosting the SMR
        # self.solarOrWindBackedUpBinaryVarNameList = [] ## identify if the solar or wind site to be backed up by SMR
        # self.newClusteredSiteSelectedVarNameList = [] ## identify if a new site selected to be the potential site hosting SMRs
       
        replacedCapacity = 0
        totalSMRCapacity = 0
        ## NOTE: do not consider the solar and wind at this iteration
        for s in range(len(self.generatorToBeReplacedList)):
            gen = self.generatorToBeReplacedList[s]
            ## 1. identify power plant located at site s being replaced or not
            capacityReplaceBinaryVarName = "y_" + str(s) 
            self.varSets[capacityReplaceBinaryVarName] = self.model.addVar(capacityReplaceBinaryVarName, vtype = "B")
            self.CapacityReplacementBinaryVarNameList.append(capacityReplaceBinaryVarName)
            replacedCapacity += float(gen["Capacity"]) * self.varSets[capacityReplaceBinaryVarName]

            ## 2. identify if site s hosting SMRs or not
            sumOfSiteSelectionBinaryVar = 0
            siteSelectionBinaryVarNameShortList = []
            for i in range(self.N + 1):
                siteSelectionBinaryVarName = "smr_" + str(s) + "_" + str(i) 
                self.varSets[siteSelectionBinaryVarName] = self.model.addVar(siteSelectionBinaryVarName, vtype = "B")
                siteSelectionBinaryVarNameShortList.append(siteSelectionBinaryVarName)
                sumOfSiteSelectionBinaryVar += self.varSets[siteSelectionBinaryVarName]
                totalSMRCapacity += i * self.varSets[siteSelectionBinaryVarName] * self.Cap_SMR
            self.siteSelectionBinaryVarNameList.append(siteSelectionBinaryVarNameShortList)

            ##--Set up constrains --##
            ## 1. limit the number of SMR to be hosted at each site
            ## the binary variables constrians Sum(i, (0，4)) smr_s_i = 1, where smr_s_0 = 1 identifying there is no SMR in the site s ##  
            self.model.addCons(sumOfSiteSelectionBinaryVar == 1)

            ## 2. Capacity constraint
            self.model.addCons(totalSMRCapacity >= replacedCapacity, name = "CapacityConstraint")

        ##-- Formulate the objective function --##
        ## Assign the variables
        totalSMRCapitalCost = self.model.addVar("totalSMRCapitalCost")
        totalProtentialCarbonCost = self.model.addVar("totalProtentialCarbonCost")
        totalLifeMonetaryCost = self.model.addVar("totalLifeMonetaryCost")

        totalProtentialCarbonCost = 0 ## promote the capacity replacement 
        totalSMRCapitalCost = 0 ## prevent placing the SMRs
        totalLifeMonetaryCost = 0 ## prevent placing the SMRs

        ## Calculate the cost of the SMR investment considering the intergration discount
        sumUpSMRIntergratedCost = 0
        SMRIntergratedCostForDifferentInterationNumberList = []
        for i in range(self.N):
            sumUpSMRIntergratedCost += (self.ir**i ) * self.Cost_SMR * self.D / (1 - ((1 + self.D)**(-1 * self.L)))
            SMRIntergratedCostForDifferentInterationNumberList.append(sumUpSMRIntergratedCost)

        for s in range(len(self.CapacityReplacementBinaryVarNameList)):
            y = self.varSets[self.CapacityReplacementBinaryVarNameList[s]]
            genIRI = self.generatorToBeReplacedList[s]["PowerGenerator"]
            existingGenCap = self.generatorToBeReplacedList[s]["Capacity"]
            existingGenFuelType = self.generatorToBeReplacedList[s]["fuelOrGenType"]
            annualOperatingHours = self.generatorToBeReplacedList[s]["annualOperatingHours"]
            CO2EmissionFactor = self.generatorToBeReplacedList[s]["CO2EmissionFactor"]

            ## 1. Formulate the Carbon emmision penalty
            carbonCost = 0
            ## the protential carbon emission cost if the old generator is not being replaced by SMR
            for l in range(self.L):
                ## l starts frm 0, therefore it is no need to use -(l-1) bus just use -l
                carbonCost += float(existingGenCap) * float(CO2EmissionFactor) * float(annualOperatingHours) * float(self.carbonTax) * ((1 + float(self.i)) **(-l))
            totalProtentialCarbonCost += (1-y) * carbonCost * self.D / (1 - ((1 + self.D)**(-1 * self.L)))

            ## 2. Formulate the SMR investment and the risk cost for SMR failure
            siteSelectionBinaryVarShortList = self.siteSelectionBinaryVarNameList[s]
            for i in range(len(siteSelectionBinaryVarShortList)):
                smr_bv = self.varSets[siteSelectionBinaryVarShortList[i]] 
                if i > 0:
                    rs =  (self.r0/1000) * (i * (self.Cap_SMR**(0.5)))
                    print(i, genIRI)
                    population = populationDensityCalculator(self.generatorToBeReplacedList[s]["LatLon"], rs, self.geospatialQueryEndpointLabel)
                    totalLifeMonetaryCost += smr_bv * population * self.FP * self.Hu * self.D / (1 - ((1 + self.D)**(-1 * self.L)))  
                    totalSMRCapitalCost += SMRIntergratedCostForDifferentInterationNumberList[i-1] * smr_bv
                    ## totalSMRCapitalCost += int(i) * smr_bv * self.Cost_SMR * self.D / (1 - ((1 + self.D)**(-1 * self.L))) 

        ##-- Set up the objective function --##
        self.model.setObjective(totalSMRCapitalCost + totalProtentialCarbonCost + totalLifeMonetaryCost, "minimize")

        ##-- Set up optimisation method --##
        self.model.optimize()
        
        for y in self.CapacityReplacementBinaryVarNameList:
            print((self.varSets[y].name), self.model.getVal(self.varSets[y]), type(self.model.getVal(self.varSets[y])))

        for nameList in self.siteSelectionBinaryVarNameList:
            for smr in nameList:
                 print((self.varSets[smr].name), self.model.getVal(self.varSets[smr]))

        ##-- Results post processing --##
        print("The Site Selection Optimal Value:", self.model.getObjVal())

        numOfSMR = 0
        totalReplacedCapacity = 0
        capacityReplacedbvNames = []
        siteSelectedbvNames = [] 
        self.capacityReplacedGeneratorList = []
        self.SMRList = []
              
        for s in range(len(self.generatorToBeReplacedList)):
            capacityReplaceBinaryVarName = self.CapacityReplacementBinaryVarNameList[s]
            siteSelectionBinaryVarShortList = self.siteSelectionBinaryVarNameList[s]
            if round(self.model.getVal(self.varSets[capacityReplaceBinaryVarName])) > 0:
                genType = self.generatorToBeReplacedList[s]["fuelOrGenType"]
                capacity = float(self.generatorToBeReplacedList[s]["Capacity"])
                print((self.varSets[capacityReplaceBinaryVarName].name), "TechType is: ", genType, "The capacity is: ", capacity)
                totalReplacedCapacity += capacity
                capacityReplacedbvNames.append(capacityReplaceBinaryVarName)

                self.generatorToBeReplacedList[s].update({"Status": "CapacityReplaced"})
                self.capacityReplacedGeneratorList.append(self.generatorToBeReplacedList[s])

                for sumary in self.genTypeSummary:
                    if sumary['fuelOrGenType'] == genType:
                        sumary['Replaced_Capacity'] += capacity
                        sumary['Replaced_number_of_Generators'] += 1
                        break

            for smr_bv in siteSelectionBinaryVarShortList:
                if round(self.model.getVal(self.varSets[smr_bv])) > 0 and siteSelectionBinaryVarShortList.index(smr_bv) > 0:
                    numOfSMR += round(self.model.getVal(self.varSets[smr_bv])) * siteSelectionBinaryVarShortList.index(smr_bv)
                    siteSelectedbvNames.append(smr_bv)
                    ## vibrate the site location from the original site a bit
                    lalon = [float(self.generatorToBeReplacedList[s]["LatLon"][0]) + 0.004, float(self.generatorToBeReplacedList[s]["LatLon"][1]) + 0.004]
                    ## initialise the SMR generator with the atttributes
                    SMRSite = {'PowerGenerator': None, 
                    'Bus': self.generatorToBeReplacedList[s]["Bus"], 
                    'Capacity': round(self.model.getVal(self.varSets[smr_bv])) * siteSelectionBinaryVarShortList.index(smr_bv) * self.Cap_SMR, 
                    'LatLon': lalon,
                    'fuelOrGenType': 'SMR', 
                    'annualOperatingHours': 0, 
                    'CO2EmissionFactor': 0.0, 
                    'place': None,
                    'NumberOfSMRUnits': round(self.model.getVal(self.varSets[smr_bv])) * siteSelectionBinaryVarShortList.index(smr_bv) }
                    self.SMRList.append(SMRSite)
        
        self.TotalRetrofittingCost = round(self.model.getVal(totalSMRCapitalCost), 2) + round(self.model.getVal(totalLifeMonetaryCost), 2) 

        print("The number of generators being replaced capacity: ", len(capacityReplacedbvNames))
        print("The list of generators being replaced capacity: ", capacityReplacedbvNames)
        print("Total replaced capacity: ", totalReplacedCapacity)

        print("The total number of SMR is: ", numOfSMR)
        print("The selected sites of hosting SMR are: ", siteSelectedbvNames)
        print("Total SMR Capacity: ", numOfSMR * self.Cap_SMR)
        print("Total Retrofitting Cost is: ", self.TotalRetrofittingCost)
        return

    def barChartCreator_Capacity(self):
        matplotlib.rcParams['axes.unicode_minus'] = False
        label_list = []
        totcalCapacityDiff = []
        replacedCapacity = []
        totalNumberOfGeneratorsDiff = []
        replacedNumberOfGenerators = [] 

        totcalCapacity = []
        totalNumberOfGenerators = []

        for sumary in self.genTypeSummary:
            genType = sumary['fuelOrGenType'].split('#')[1]
            label_list.append(genType)
            totcalCapacityDiff.append(float(sumary['Total_Capacity']) - float(sumary['Replaced_Capacity']))
            totcalCapacity.append(float(sumary['Total_Capacity']))
            replacedCapacity.append(float(sumary['Replaced_Capacity']))
            totalNumberOfGeneratorsDiff.append(float(sumary['Total_Number_of_Generators']) - float(sumary['Replaced_number_of_Generators']))
            replacedNumberOfGenerators.append(float(sumary['Replaced_number_of_Generators']))
            totalNumberOfGenerators.append(float(sumary['Total_Number_of_Generators']))

        x = range(len(totcalCapacity))
        rects1 = plt.bar(x, height=replacedCapacity, width=0.35, alpha = 0.5, color='blue', label="Replaced Capacity")
        rects2 = plt.bar(x, height=totcalCapacityDiff, width=0.35, alpha = 0.3, color='blue', label="Total Capacity", bottom=replacedCapacity)
        plt.ylim(0, max(totcalCapacity))
        plt.ylabel("Capacity")
        plt.xticks(x, label_list)
        plt.xlabel("Generator Technology Types")
        plt.title("Generator Replacement -- Capacity")
        plt.legend()
        plt.show()
        plt.clf()

    def barChartCreator_NumOfGenerator(self):
        matplotlib.rcParams['axes.unicode_minus'] = False
        label_list = []
        totcalCapacityDiff = []
        replacedCapacity = []
        totalNumberOfGeneratorsDiff = []
        replacedNumberOfGenerators = [] 

        totcalCapacity = []
        totalNumberOfGenerators = []

        for sumary in self.genTypeSummary:
            genType = sumary['fuelOrGenType'].split('#')[1]
            label_list.append(genType)
            totcalCapacityDiff.append(float(sumary['Total_Capacity']) - float(sumary['Replaced_Capacity']))
            totcalCapacity.append(float(sumary['Total_Capacity']))
            replacedCapacity.append(float(sumary['Replaced_Capacity']))
            totalNumberOfGeneratorsDiff.append(float(sumary['Total_Number_of_Generators']) - float(sumary['Replaced_number_of_Generators']))
            replacedNumberOfGenerators.append(float(sumary['Replaced_number_of_Generators']))
            totalNumberOfGenerators.append(float(sumary['Total_Number_of_Generators']))

        x = range(len(totalNumberOfGenerators))
        rects1 = plt.bar(x, height=replacedNumberOfGenerators, width=0.35, alpha = 0.5, color='grey', label="Number of Replaced Generators")
        rects2 = plt.bar(x, height=totalNumberOfGeneratorsDiff, width=0.35, alpha = 0.3, color='grey', label="Total Number of Generators", bottom=replacedNumberOfGenerators)
        plt.ylim(0, max(totalNumberOfGenerators))
        plt.ylabel("Number of Generators")
        plt.xticks(x, label_list)
        plt.xlabel("Generator Technology Types")
        plt.title("Generator Replacement -- Number of Generators")
        plt.legend()
        plt.show()
        plt.clf()



        # ## Capacity
        # 
        # rects1 = plt.bar(x, height=totcalCapacity, width=0.4, alpha=0.8, color='blue', label="Total Capacity")
        # rects2 = plt.bar([i + 0.4 for i in x], height=replacedCapacity, width=0.4, color='grey', label="Replaced Capacity")

        # plt.ylim(0, max(totcalCapacity))
        # plt.ylabel("Capacity")

        # plt.xticks([index + 0.2 for index in x], label_list)
        # plt.xlabel("Generator Technology Types")
        # plt.title("Generator Replacement -- Capacity")
        # plt.legend()     
        
        # for rect in rects1:
        #     height = rect.get_height()
        #     plt.text(rect.get_x() + rect.get_width() / 2, height+1, str(height), ha="center", va="bottom")
        # for rect in rects2:
        #     height = rect.get_height()
        #     plt.text(rect.get_x() + rect.get_width() / 2, height+1, str(height), ha="center", va="bottom")
        # plt.show()

        # ## Number of Generator
        # x = range(len(totalNumberOfGenerators))
        # rects1 = plt.bar(x, height=totalNumberOfGenerators, width=0.4, alpha=0.8, color='blue', label="Total Number of Generators")
        # rects2 = plt.bar([i + 0.4 for i in x], height=replacedNumberOfGenerators, width=0.4, color='grey', label="Number of Replaced Generators")

        # plt.ylim(0, max(totalNumberOfGenerators))
        # plt.ylabel("Number of Generators")

        # plt.xticks([index + 0.2 for index in x], label_list)
        # plt.xlabel("Generator Technology Types")
        # plt.title("Generator Replacement -- Number of Generators")
        # plt.legend()     
        
        # for rect in rects1:
        #     height = rect.get_height()
        #     plt.text(rect.get_x() + rect.get_width() / 2, height+1, str(height), ha="center", va="bottom")
        # for rect in rects2:
        #     height = rect.get_height()
        #     plt.text(rect.get_x() + rect.get_width() / 2, height+1, str(height), ha="center", va="bottom")
        # plt.show()


##NOTE: This function is not used 
    """The generator cluster function"""
    def siteCluster(self):
        genNotCluster = []
        toBeCluster = [] 
        location = []

        ## split the generator into not being clustered and  to be clustered
        for gen in self.generatorToBeReplacedList: 
            if not ('Solar' in gen["fuelOrGenType"] or 'Wind' in gen["fuelOrGenType"]):  
                genNotCluster.append(gen)
            else:
                toBeCluster.append(gen)

        if len(toBeCluster) == 0:
            print("!!!!There is no generator to be clustered!!!!")
            self.NonClusteredGeneratorNumber = len(genNotCluster)
            self.OutliersNumber = 0
            self.clusteredGeneratorNumber = 0
            return

        ## Form the loaction point list for clustering
        for gen in toBeCluster:
            if "#" in gen["LatLon"]:
                gen['LatLon'] = [float(gen['LatLon'].split('#')[0]), float(gen['LatLon'].split('#')[1])]    
                location.append(gen['LatLon'])  
            else:
                location.append(gen['LatLon'])
        print('The number of the point to be clustered is:', len(location))
        
        ## perform the clustering algorithm: Density-Based Spatial Clustering of Applications with Noise (DBSCAN)
        clustering = DBSCAN(eps = 0.16, min_samples = 2).fit(location)
        label = clustering.labels_
        print('The number of clusters is:',  max(label) + 1)
        print('The number of outliers is:', np.count_nonzero(label==-1))
        print('The number of points (sites) after the clustering is:', max(label) + 1 + np.count_nonzero(label==-1))

        outliers = []
        beClastered = [ [] for i in range(max(label) + 1) ]
        beClasteredGenerator = [ [] for i in range(max(label) + 1) ]
        capacityForEachCluster = [ 0 for i in range(max(label) + 1) ]
        centriodList = []

        ## classify the outliers and the clustered 
        for l in location:
            i =  location.index(l)
            clusteringlabel = label[i]
            if clusteringlabel == -1:
                outliers.append(toBeCluster[i])
            else: 
                beClastered[int(clusteringlabel)].append((l[1], l[0]))
                originalGenerator = toBeCluster[i]
                beClasteredGenerator[int(clusteringlabel)].append(originalGenerator)
                capacityForEachCluster[int(clusteringlabel)] += float(toBeCluster[i]['Capacity'])

        ## Record the number of non-clustered and outliers
        self.NonClusteredGeneratorNumber = len(genNotCluster)
        self.OutliersNumber = len(outliers)
        self.clusteredGeneratorNumber = len(beClastered)
        
        ## genNotCluster includes the non-solar/wind generators and the outliers of solar and wind generators which are not being clustered.
        genNotCluster += outliers

        ## Find the centroid of each cluster
        for mp in beClastered:
            centriod = MultiPoint(mp).centroid
            centriodList.append([round(float(centriod.y), 5), round(float(centriod.x), 5)]) 
        
        ## initialize the new sites with the location of the centroids
        clusteredSite = []
        for centroid in centriodList:
            n = centriodList.index(centroid)
            beclusteredgens = beClasteredGenerator[n]
            gen = {'PowerGenerator': None, 
                    'Bus': None, 
                    'Capacity': capacityForEachCluster[n], 
                    'LatLon': centroid, 
                    'fuelOrGenType': 'SMR', 
                    'annualOperatingHours': 0, 
                    'CO2EmissionFactor': 0.0, 
                    'place': None,
                    'beClusteredGenerators': beclusteredgens}
            clusteredSite.append(gen)

        self.generatorToBeReplacedList = genNotCluster + clusteredSite

        ## print(self.generatorToBeReplacedList, len(self.generatorToBeReplacedList))
        return
        
if __name__ == '__main__':
    carbonTax = 820 
    GenCoal = [{'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_3b7acf93-cc8b-4ad7-9c85-2f737eace679', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_ebace1f4-7d3a-44f6-980e-a4b844de670b', 'Capacity': '1559.0', 'LatLon': [51.38731, -3.4049], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 482.06, 'CO2EmissionFactor': 0.319, 'place': 'https://dbpedia.org/page/Wales'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_7bda0d1f-8df0-496c-8f7a-b328c70ffe2d', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_2d76797b-c638-460e-b73c-769e29785466', 'Capacity': '2000.0', 'LatLon': [53.304, -0.7815], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 482.06, 'CO2EmissionFactor': 0.319, 'place': 'https://dbpedia.org/page/East_Midlands'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_598cdc45-52b8-4027-888f-3eb4758b329c', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_84f6905c-d4cb-409f-861f-ea66fe25ddd0', 'Capacity': '230.0', 'LatLon': [51.54907, -2.97053], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 482.06, 'CO2EmissionFactor': 0.319, 'place': 'https://dbpedia.org/page/Yorkshire_and_the_Humber'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_c2153e52-dee7-49d8-9ba2-90f83edfde5f', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_2d76797b-c638-460e-b73c-769e29785466', 'Capacity': '2000.0', 'LatLon': [53.36046, -0.81019], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 482.06, 'CO2EmissionFactor': 0.319, 'place': 'https://dbpedia.org/page/East_Midlands'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_a04e0e57-f847-426b-9cc3-c323d261aecd', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_2d76797b-c638-460e-b73c-769e29785466', 'Capacity': '2021.0', 'LatLon': [52.86463, -1.25829], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 482.06, 'CO2EmissionFactor': 0.319, 'place': 'https://dbpedia.org/page/East_Midlands'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_b32946fc-6abb-4df5-9437-7e01dbe1ca64', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_c4d7dcca-a7f5-4887-a460-31706ab7ec9c', 'Capacity': '1961.0', 'LatLon': [53.37234, -2.68912], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 482.06, 'CO2EmissionFactor': 0.319, 'place': 'https://dbpedia.org/page/North_West_England'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_74fbe81f-339b-41fe-b2da-940d05b774b2', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_84f6905c-d4cb-409f-861f-ea66fe25ddd0', 'Capacity': '1320.0', 'LatLon': [53.74043, -0.9981], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 482.06, 'CO2EmissionFactor': 0.319, 'place': 'https://dbpedia.org/page/Yorkshire_and_the_Humber'}]
    testPP = [{"PowerGenerator": 1, "Bus": 1, "Capacity": 500, "fuelOrGenType": "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas", "LatLon":"52.209556#0.120046", "CO2EmissionFactor": 0.181, "annualOperatingHours": 3593.48},
    {"PowerGenerator": 1, "Bus": 1, "Capacity":500, "fuelOrGenType": "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar", "LatLon":"52.209556#0.120046", "CO2EmissionFactor": 0, "annualOperatingHours": 482.06}]
    
    gen = [{'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_20026d02-c3e8-42c0-90fc-278292409f9c', 
    'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_ebace1f4-7d3a-44f6-980e-a4b844de670b', 
    'Capacity': '3.7', 'LatLon': [51.76599, -3.6216], 
    'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 
    'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/Wales'}]

    solarGen = [{'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_178815a0-ff5a-46d2-b458-ed2a769dd754', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_f17335d2-53f6-4044-9d09-c3d9438c0950', 'Capacity': '4.2', 'LatLon': [56.63556, -2.70823], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 
    3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/Scotland'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_d00fc7db-69a6-498e-bd75-577ff832c6c6', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_024c0566-d9f0-497d-955e-f7f4e55d4296', 'Capacity': '4.2', 'LatLon': [52.87362, -1.77829], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/West_Midlands_(county)'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_1d5943ed-9e7e-422b-908d-ed55afc34da5', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_55285d5a-1d0e-4b1f-8713-246d601671e5', 'Capacity': '4.2', 'LatLon': [50.77248, -3.46779], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/South_West_England'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_4bb4b1d6-1799-49f3-973c-dc80fe6deaa4', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_2d76797b-c638-460e-b73c-769e29785466', 'Capacity': '4.2', 'LatLon': [53.15549, -1.01682], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/East_Midlands'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_38abdf6a-6df2-4222-83ab-f8c2876d2f2a', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_2d76797b-c638-460e-b73c-769e29785466', 'Capacity': '4.2', 'LatLon': [53.26208, -1.30669], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/East_Midlands'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_ce1a8be0-4b54-4358-b487-fc9d7cdd34f8', 
    'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_2d76797b-c638-460e-b73c-769e29785466', 'Capacity': '4.2', 'LatLon': [52.58526, -0.70932], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/East_Midlands'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_83c8259a-d661-4c04-a8c3-a4ab08a7bcf5', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_2d76797b-c638-460e-b73c-769e29785466', 'Capacity': '4.2', 'LatLon': [52.93586, -1.06178], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/East_Midlands'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_cfa9e957-1ff4-4cc2-9b1e-a274fc440a47', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_1f3c4462-3472-4949-bffb-eae7d3135591', 'Capacity': '4.1', 'LatLon': [51.92478, -0.61447], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/South_East_England'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_f291e93e-2df9-424f-9bee-1313812b5575', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_55285d5a-1d0e-4b1f-8713-246d601671e5', 'Capacity': '4.1', 
    'LatLon': [50.2933, -3.7671], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/South_West_England'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_237658a9-f415-4ad8-9d84-3c67ced5c9fc', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_55285d5a-1d0e-4b1f-8713-246d601671e5', 'Capacity': '4.1', 'LatLon': [50.6473, -3.38468], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/South_West_England'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_176f6c4f-d601-42c2-a527-d17b683bf13a', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_55285d5a-1d0e-4b1f-8713-246d601671e5', 'Capacity': '4.0', 'LatLon': [50.4303, -4.01809], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/South_West_England'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_c4481c3a-909f-42b6-b079-cc65649c1afb', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_1f3c4462-3472-4949-bffb-eae7d3135591', 'Capacity': '4.0', 'LatLon': [51.14376, -0.95849], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/South_East_England'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_50065262-f28f-4cf7-8a73-83abbcff0dcb', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_1f3c4462-3472-4949-bffb-eae7d3135591', 'Capacity': '4.0', 'LatLon': [52.10287, -1.0062], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/South_East_England'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_785b64b1-27c4-4942-b061-cd7b1d961612', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_ebace1f4-7d3a-44f6-980e-a4b844de670b', 'Capacity': '3.9', 'LatLon': [51.11884, -2.9748], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/Wales'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_baa4ddc4-b0f3-45d7-86a7-b25ef12ad3e9', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_1f3c4462-3472-4949-bffb-eae7d3135591', 'Capacity': '3.9', 'LatLon': [51.32697, -1.11208], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/South_East_England'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_9485528c-c0b2-4f1f-94b6-36751b047b29', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_55285d5a-1d0e-4b1f-8713-246d601671e5', 'Capacity': '3.9', 'LatLon': [51.55863, -2.65744], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/South_West_England'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_62705655-fbff-4d3b-90d8-c027b48adc33', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_55285d5a-1d0e-4b1f-8713-246d601671e5', 'Capacity': '3.9', 'LatLon': [50.32172, -4.84497], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/South_West_England'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_166b2d12-c577-48f1-a4a1-bda6cecd83dd', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_55285d5a-1d0e-4b1f-8713-246d601671e5', 'Capacity': '3.8', 'LatLon': [52.08901, -1.92453], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/South_West_England'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_363286a2-ada8-447f-8e16-9d89aafbc8da', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_2d76797b-c638-460e-b73c-769e29785466', 'Capacity': '3.8', 'LatLon': [53.18206, -1.20073], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/East_Midlands'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_867b9858-8fc3-4b6f-8b12-1eee91cf519c', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_55285d5a-1d0e-4b1f-8713-246d601671e5', 'Capacity': '3.8', 'LatLon': [56.16735, -3.17232], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/South_West_England'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_c5f4d4ad-c394-4da4-98e2-1411a282de57', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_1f3c4462-3472-4949-bffb-eae7d3135591', 'Capacity': '3.8', 'LatLon': [51.24982, -1.2763], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/South_East_England'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_e413b826-5055-48ed-a073-a8c43c0eeeef', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_024c0566-d9f0-497d-955e-f7f4e55d4296', 'Capacity': '3.7', 'LatLon': [52.33552, -2.49608], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/West_Midlands_(county)'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_e0045eeb-dec5-408a-b011-bdf2d6d1194c', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_ebace1f4-7d3a-44f6-980e-a4b844de670b', 'Capacity': '3.7', 'LatLon': [51.4314, -3.4099], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/Wales'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_198b9609-9ee5-4712-9087-6bd73912de01', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_55285d5a-1d0e-4b1f-8713-246d601671e5', 'Capacity': '3.7', 'LatLon': [50.83671, -3.78238], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/South_West_England'}]


    genDiffType =  [{'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_3b7acf93-cc8b-4ad7-9c85-2f737eace679', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_ebace1f4-7d3a-44f6-980e-a4b844de670b', 'Capacity': '500', 'LatLon': [51.38731, -3.4049], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'annualOperatingHours': 482.06, 'CO2EmissionFactor': 0.319, 'place': 'https://dbpedia.org/page/Wales'}, 
                    {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_6785c6c3-e01c-4deb-9e99-4bcf11885617', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_1f3c4462-3472-4949-bffb-eae7d3135591', 'Capacity': '1000', 'LatLon': [51.38731, -3.4049], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Oil', 'annualOperatingHours': 7880.6, 'CO2EmissionFactor': 0.181, 'place': 'https://dbpedia.org/page/South_East_England'},
                    {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_4252b921-f307-4476-a9e8-df650d85bc56', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_2d76797b-c638-460e-b73c-769e29785466', 'Capacity': '500', 'LatLon': [51.38731, -3.4049], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'annualOperatingHours': 3430.73, 'CO2EmissionFactor': 0.0, 'place': 'https://dbpedia.org/page/East_Midlands'},
                    {'PowerGenerator': 'http://www.theworldavatar.com/kb/ontoeip/PowerGenerator_08f97f64-251c-4b5a-89ee-cb826ea440d6', 'Bus': 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_d6046ef2-6909-4f20-808f-cd9aa01c8ae5', 'Capacity': '500', 'LatLon': [51.38731, -3.4049], 'fuelOrGenType': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas', 'annualOperatingHours': 3593.48, 'CO2EmissionFactor': 0.181, 'place': 'https://dbpedia.org/page/East_of_England'}
                    ]

    test = SitePreSelectionAndCapacityReplacement('ukdigitaltwin_pd', GenCoal, 0.02, 40, 1800000000, 2400000, 200, 0.002985, 470, 0.7, 0.0125, carbonTax, False, False, True, 4, 0)
    test.SitePreSelectorAndCapacityReplacementFinder()
   



       
       


