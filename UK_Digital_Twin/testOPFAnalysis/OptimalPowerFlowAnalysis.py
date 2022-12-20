##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 19 Dec 2022          #
##########################################

"""
Optimal Power Flow Analysis
"""
from ast import Str
from cgi import test
from logging import raiseExceptions
from pickle import TRUE
import sys, os, numpy, uuid, time
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE) 
import queryOPFInput 
from datetime import datetime
import pytz
from rfc3987 import parse
from UK_Digital_Twin_Package import UKPowerGridModel as UK_PG
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from UK_Digital_Twin_Package.jpsSingletons import jpsBaseLibGW
from UK_Digital_Twin_Package import demandLoadAllocator as DLA
from UK_Digital_Twin_Package import BranchPropertyInitialisation as BPI
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel as endpointList
from UK_Digital_Twin_Package import generatorCluster as genCluster
import SPARQLQueryUsedInModelInitialiser as queryModelInitialiser
import UK_Power_Grid_Model_Generator.SPARQLQueryUsedInModel as query_model
from UK_Power_Grid_Model_Generator.costFunctionParameterInitialiser import costFuncPara
from UK_Power_Grid_Model_Generator import model_EBusABoxGeneration, model_EGenABoxGeneration, model_ELineABoxGeneration
from BusModelKGInstanceCreator import BusModelKGInstanceCreator
from BranchModelKGInstanceCreator import BranchModelKGInstanceCreator
from GeneratorModelKGInstanceCreator import GeneratorModelKGInstanceCreator
import UK_Power_Grid_Model_Generator.initialiseEBusModelVariable as InitialiseEbus
from UK_Digital_Twin_Package import UKDigitalTwinTBox as T_BOX
from UK_Digital_Twin_Package import UKPowerPlant as UKpp
from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs, selectStoragePath, readFile, specifyValidFilePath
from UK_Digital_Twin_Package import CO2FactorAndGenCostFactor as ModelFactor
from SMRSitePreSelection import SitePreSelection as sp
from SMRSitePreSelection import SitePreSelection_pymoo as sp_pymoo
from SMRSitePreSelection import rankingTheSite as ranker
from pypower.api import ppoption, runopf, isload, runuopf
import UK_Digital_Twin_Package.PYPOWER.pypower.runpf as pf
from numpy import False_, r_, c_, ix_, zeros, pi, ones, exp, union1d, array, linalg, where, logical_or, arange, \
                    ones, sort, exp, pi, diff, min, \
                    argmin, argmax, real, imag, any, delete
from numpy import flatnonzero as find
from scipy.sparse import hstack, vstack
from pypower.loadcase import loadcase
from pypower.ext2int import ext2int
from pypower.bustypes import bustypes
from pypower.makeYbus import makeYbus
from pypower.makeSbus import makeSbus
from pypower.dSbus_dV import dSbus_dV
from pypower.idx_bus import BUS_I, PD, QD, VM, VA, GS, BUS_TYPE, PV, PQ, REF
from pypower.idx_brch import PF, PT, QF, QT, F_BUS, TAP, SHIFT, T_BUS, BR_R, BR_X, BR_STATUS
from pypower.idx_gen import PG, QG, VG, QMAX, QMIN, GEN_BUS, GEN_STATUS
import matplotlib.pyplot as plt
from UK_Digital_Twin_Package import generatorCluster
from shapely.geometry import MultiPoint

from pymoo.algorithms.moo.nsga2 import NSGA2
from pymoo.optimize import minimize
from pymoo.operators.crossover.sbx import SBX
from pymoo.operators.mutation.pm import PM
from pymoo.operators.repair.rounding import RoundingRepair
from pymoo.operators.sampling.rnd import IntegerRandomSampling, BinaryRandomSampling, FloatRandomSampling
from SMRSitePreSelection.populationDensityCalculator import populationDensityCalculator

from visualisationColourCreator import gen_fuel_col
## import SMRSitePreSelection.demandingAndCentroidList as dclist
from UK_Digital_Twin_Package.DistanceCalculator import DistanceBasedOnGPSLocation
from SMRSitePreSelection.demandingAndCentroidList import demandingAndCentroid
from UK_Power_Grid_Topology_Generator.SPARQLQueriesUsedInTopologyABox import queryWithinRegion
import shapely.geometry
import csv
from pymoo.util import plotting
from pymoo.visualization.scatter import Scatter
from sklearn.cluster import DBSCAN
from pyscipopt import Model
from SMRSitePreSelection.DecommissioningCost import DecommissioningCost as DCost
import matplotlib.pyplot as plt
from pymoo.decomposition.asf import ASF ##Augmented Scalarization Function (ASF)
import pandas as pd
import seaborn
from adjustText import adjust_text
import matplotlib as mpl
import matplotlib.ticker as ticker 

## create configuration objects
SLASH = '/'

markersList = ['o', 'v', 's', '*', 'X', 'P', 'D']
lineStyleList = ['solid', 'dotted', 'dashed', 'dashdot', (0, (3,1,1,1)), (0, (1,10)), (5, (10, 3)), (0, (5,10)), (0, (3, 10, 1, 10)), (0, (3,5,1,5)), (0, (3,5,1,5,1,5)), (0, (3,10, 1,10,1,10)), (0, (3,1,1,1,1,1))]

labelFontSize = 15
legendFontSize = 13
dotLabel = 11

dt = UKDT.UKDigitalTwin()
ukmf = ModelFactor.ModelFactor()

t_box = T_BOX.UKDigitalTwinTBox()
ukpp = UKpp.UKPowerPlant()
## set up the derivationInstanceBaseURL
derivationInstanceBaseURL = dt.baseURL + SLASH + dt.topNode + SLASH
## read cost factors file
modelFactorArrays = readFile(ukmf.CO2EmissionFactorAndCostFactor)

class OptimalPowerFlowAnalysis:
    
    def __init__(
        self, 
        topologyNodeIRI:str,
        agentIRI:str,
        startTime_of_EnergyConsumption:str,
        slackBusNodeIRI:str, loadAllocatorName:str, EBusModelVariableInitialisationMethodName:str,
        ELineInitialisationMethodName:str, 
        piecewiseOrPolynomial:int, pointsOfPiecewiseOrcostFuncOrder:int, baseMVA: float,  
        withRetrofit:bool, retrofitGenerator: list, retrofitGenerationTechTypeOrGenerationTechnology: list, newGeneratorType:str,
        discountRate:float,
        bankRate:float, 
        projectLifeSpan:float,
        SMRCapitalCost:float,
        MonetaryValuePerHumanLife:float,
        NeighbourhoodRadiusForSMRUnitOf1MW:float,
        ProbabilityOfReactorFailure:float,
        SMRCapability:float,
        maxmumSMRUnitAtOneSite: int,
        SMRIntergratedDiscount:float, 
        DiscommissioningCostEstimatedLevel:int,
        safeDistance:float,
        pop_size:int,
        n_offsprings:int,
        numberOfGAGenerations:int,
        OWLUpdateEndPointURL:str, endPointUser:str = None, endPointPassWord:str = None,
        OWLFileStoragePath = None, updateLocalPowerPlantOWLFileFlag:bool = True
        ):
        ## -- Local objectives container --##
        self.ObjectSet = locals()     
        ##--1. specify the query/update endpoint information--##
        self.queryUKDigitalTwinEndpointLabel = endpointList.ukdigitaltwin['label'] ## ukdigitaltwin
        self.queryUKDigitalTwinEndpointIRI = endpointList.ukdigitaltwin['endpoint_iri']
        self.geospatialQueryEndpointLabel = endpointList.ukdigitaltwin_pd['label'] ## population: ukdigitaltwin_pd
        self.geospatialQueryEndpointIRI = endpointList.ukdigitaltwin_pd['endpoint_iri']
        self.OWLUpdateEndPointURL = OWLUpdateEndPointURL ## derivation
        self.endPointUser = endPointUser
        self.endPointPassWord = endPointPassWord
        self.ons_endpointIRI = endpointList.ONS['endpoint_iri']
        self.ons_endpointLabel = endpointList.ONS['label']
        ## create the power system model node IRI
        self.powerSystemModelIRI = UK_PG.ontopowsys_namespace + UK_PG.powerSystemModelKey + str(uuid.uuid4())
        ## create the timeStamp, e.x. 2022-06-15T16:24:29.371941+00:00
        self.timeStamp = datetime.now(pytz.utc).isoformat()
        ## query the number of the bus under the topology node IRI, and the bus node IRI, branch node IRI and generator node IRI
        self.numOfBus, self.busNodeList = query_model.queryBusTopologicalInformation(topologyNodeIRI, self.queryUKDigitalTwinEndpointLabel) ## ?BusNodeIRI ?BusLatLon ?GenerationLinkedToBusNode
        self.branchNodeList, self.branchVoltageLevel = query_model.queryELineTopologicalInformation(topologyNodeIRI, self.queryUKDigitalTwinEndpointLabel) ## ?ELineNode ?From_Bus ?To_Bus ?Value_Length_ELine ?Num_OHL_400 or 275 
        self.generatorNodeList = query_model.queryEGenInfo(topologyNodeIRI, self.queryUKDigitalTwinEndpointLabel) ## 0?PowerGeneratorIRI 1?FixedMO 2?VarMO 3?FuelCost 4?CO2EmissionFactor 5?Bus 6?Capacity 7?PrimaryFuel 8?Latlon 9?PowerPlant_LACode 10:Extant 11: samllerLAcode   
        if withRetrofit is True:
            for egen in self.generatorNodeList:
                egen.append("Extant")
        self.capa_demand_ratio = model_EGenABoxGeneration.demandAndCapacityRatioCalculator(self.generatorNodeList, topologyNodeIRI, startTime_of_EnergyConsumption)
        ##--2. passing arguments--##
        ## specify the topology node
        self.topologyNodeIRI = topologyNodeIRI
        ## specify the startTime_of_EnergyConsumption for querying the demand load 
        self.startTime_of_EnergyConsumption = startTime_of_EnergyConsumption
        ## specify the agent IRI        
        self.agentIRI = agentIRI
        ## specify the slackBusNodeIRI (there is only one slack bus is allowed in the modelling)
        self.slackBusNodeIRI = slackBusNodeIRI
        ## specify the loadAllocatorName
        self.loadAllocatorName = loadAllocatorName
        ## specify the EBusModel, ELine and EGen Variable Initialisation Method Name
        self.EBbusInitialisationMethodName = str(EBusModelVariableInitialisationMethodName)
        self.ELineInitialisationMethodName = str(ELineInitialisationMethodName)
        self.OrderedBusNodeIRIList = []
        ##--3. specify the OPF model factors--##
        ## specify the baseMVA and OPFOrPF 
        self.baseMVA = float(baseMVA)
        self.OPFOrPF:bool = True ## true for OPF simulation 
        ## specify the OPF objective function type, 1 for piecewise, 2 for polynomial
        if int(piecewiseOrPolynomial) not in [1, 2]:
            raiseExceptions("piecewiseOrPolynomial has to be 1 or 2")
        else:
            self.piecewiseOrPolynomial = int(piecewiseOrPolynomial)
        ## specify the pointsOfPiecewiseOrcostFuncOrder
        if int(pointsOfPiecewiseOrcostFuncOrder) < 0:
            raiseExceptions("pointsOfPiecewiseOrcostFuncOrder has to be a positive number")
        else:
            self.pointsOfPiecewiseOrcostFuncOrder = int(pointsOfPiecewiseOrcostFuncOrder)     
        ## specify the local storage path (to be deleted)
        self.OWLFileStoragePath = OWLFileStoragePath
        self.updateLocalPowerPlantOWLFileFlag = updateLocalPowerPlantOWLFileFlag       
        ##--4. JVM module view and use it to import the required java classes--##
        self.jpsBaseLib_view = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view,"uk.ac.cam.cares.jps.base.derivation.*")
        ## initialise the storeClient with SPARQL Query and Update endpoint
        if self.endPointUser is None:
            self.storeClient = self.jpsBaseLib_view.RemoteStoreClient(self.OWLUpdateEndPointURL, self.OWLUpdateEndPointURL)
        else:
            self.storeClient = self.jpsBaseLib_view.RemoteStoreClient(self.OWLUpdateEndPointURL, self.OWLUpdateEndPointURL, self.endPointUser, self.endPointPassWord)
        ## initialise the derivationClient
        self.derivationClient = self.jpsBaseLib_view.DerivationClient(self.storeClient, derivationInstanceBaseURL)  
        ##--5. Identify the retrofitting generators--##
        if type(withRetrofit) is not bool:
            raiseExceptions("withRetrofit has to be a bool number")
        else:
            self.withRetrofit = withRetrofit 
        self.retrofitGenerator = retrofitGenerator # GeneratorIRI, location, capacity
        self.retrofitGenerationFuelOrGenType = retrofitGenerationTechTypeOrGenerationTechnology    
        ##--6. Identify the generators type used to replace the exisiting generators--##
        self.newGeneratorType = newGeneratorType
        ##--7. initialise the bus/branch/generator objects name list--##
        self.BusObjectList: list = []
        self.BranchObjectList: list = []
        self.GeneratorObjectList: list = []
        self.SMRSiteObjectList:list = []
        ##--8. site selection factors--##
        self.discountRate = discountRate
        self.bankRate = bankRate
        self.projectLifeSpan = projectLifeSpan
        self.SMRCapitalCost = SMRCapitalCost
        self.maxmumSMRUnitAtOneSite = maxmumSMRUnitAtOneSite
        self.SMRIntergratedDiscount = SMRIntergratedDiscount
        self.MonetaryValuePerHumanLife = MonetaryValuePerHumanLife 
        self.NeighbourhoodRadiusForSMRUnitOf1MW = NeighbourhoodRadiusForSMRUnitOf1MW
        self.ProbabilityOfReactorFailure = ProbabilityOfReactorFailure
        self.SMRCapability = float(SMRCapability)
        self.DiscommissioningCostEstimatedLevel = DiscommissioningCostEstimatedLevel
        self.safeDistance = float(safeDistance)
        self.pop_size = pop_size
        self.n_offsprings = n_offsprings
        self.numberOfGenerations = numberOfGAGenerations
        self.retrofittingCost = 0
        ##--9. Demanding area query --##
        self.demandingAreaList = demandingAndCentroid[self.startTime_of_EnergyConsumption]
        ##FIXME: demanding should be queried
        # self.demandingAreaList = list(query_model.queryElectricityConsumption_LocalArea(startTime_of_EnergyConsumption, self.queryUKDigitalTwinEndpointIRI, self.ons_endpointIRI))
        # # find the centroid of the polygon, the value of the 
        # for ec in self.demandingAreaList:
        #     if ec['Geo_InfoList'].geom_type == 'MultiPolygon':
        #         ec['Geo_InfoList'] = DLA.centroidOfMultipolygon(ec['Geo_InfoList']) 
        #     elif ec['Geo_InfoList'].geom_type == 'Polygon':
        #         lon = ec['Geo_InfoList'].centroid.x
        #         lat = ec['Geo_InfoList'].centroid.y
        #         ec['Geo_InfoList'] = [lat, lon]

        self.CarbonTaxForOPF = -1 ## the initial carbon tax not for OPF calculation
        self.weatherConditionName = None
        self.time_now = time.strftime("%Y%m%d-%H%M", time.localtime())
        self.diagramPath = '.\\figFiles\\' + self.time_now + '\\'

    """Find the power plants located in each demanding areas"""
    ##FIXME: this method is for the pre-opf method
    def powerPlantAndDemandingAreasMapper(self):
        for demanding in self.demandingAreaList:
            Area_LACode = str(demanding['Area_LACode'])
            boundary = queryOPFInput.queryAreaBoundaries(Area_LACode)
            if Area_LACode in ["E22000303", "E22000306", "E22000311", "E14001056"]:
                official_region = "E12000008"
            elif Area_LACode in ["E41000222", "E14000981", "E14001022", "E41000225"]:
                official_region = "E12000006"
            elif Area_LACode in ["E41000092", "E14000839", "E41000088", "E14001031", "E41000090", "E41000212", "E14000881", "E14000988"]:
                official_region = "E12000009" 
            elif Area_LACode in ["K03000001", "K02000001", "W92000004","S92000003", "E12000001", "E12000002", "E12000003", "E12000004", "E12000005", 
                                "E12000006", "E12000007", "E12000008", "E12000009", "E13000001", "E13000002"]:
                continue
            else:
                official_region = queryWithinRegion(Area_LACode, self.ons_endpointLabel) ## return a list of the region LA code
            for gen in self.generatorNodeList:
                if gen[9] in official_region:
                    genLocation = shapely.geometry.Point(gen[8][1], gen[8][0])
                    interiorFlag = boundary.intersects(genLocation)
                    if interiorFlag == True:
                        gen.append(Area_LACode)
        return 
        
    """This method is called to select the site to be replaced by SMR"""
    def retrofitGeneratorInstanceFinder(self):
        if self.withRetrofit is True: 
            if len(self.retrofitGenerator) == 0 and len(self.retrofitGenerationFuelOrGenType) == 0:  
                print("***As there is not specific generator assigned to be retrofitted by SMR, all generators located in GB will be treated as the potential sites.***")
                ## retrofitList：[0]PowerGenerator, [1]Bus, [2]Capacity, [3]LatLon, [4]fuelOrGenType, [5]annualOperatingHours, [6] CO2EmissionFactor ([7]numberOfSMR added after SMR site selection)
                ##FIXME: fix the query string
                self.retrofitListBeforeSelection = queryOPFInput.queryGeneratorToBeRetrofitted_AllPowerPlant(self.topologyNodeIRI, self.queryUKDigitalTwinEndpointLabel)
            elif not len(self.retrofitGenerator) == 0:
                for iri in self.retrofitGenerator:
                    parse(iri, rule='IRI')
                ##FIXME: fix the query string
                self.retrofitListBeforeSelection = queryOPFInput.queryGeneratorToBeRetrofitted_SelectedGenerator(self.retrofitGenerator, self.queryUKDigitalTwinEndpointLabel) 
            elif not len(self.retrofitGenerationFuelOrGenType) == 0:
                for iri in self.retrofitGenerationFuelOrGenType:
                    parse(iri, rule='IRI')
                self.retrofitListBeforeSelection, self.genTypeSummary = queryOPFInput.queryGeneratorToBeRetrofitted_SelectedFuelOrGenerationTechnologyType(self.retrofitGenerationFuelOrGenType, self.topologyNodeIRI, self.queryUKDigitalTwinEndpointLabel)
            print("===The number of the generator that goes to the site selection method is (before cluster):===", len(self.retrofitListBeforeSelection))           
            ## cluster the close generators, self.retrofitListBeforeSelection will be changed
            ##TODO:close the sire cluster using the new site selection method instead
            ##self.siteCluster()
            print("===The number of the generator that goes to the site selection method is (after cluster):===", len(self.retrofitListBeforeSelection))
            print("The total number of generator is", len(self.generatorNodeList))

            ## Pre-calculate the the population and the weightedDemandingDistance
            rs_List = []
            for n in range(self.maxmumSMRUnitAtOneSite):
                rs =  (self.NeighbourhoodRadiusForSMRUnitOf1MW/1000) * ((n + 1) * (self.SMRCapability**(0.5)))
                rs_List.append(rs)
            self.population_list = []
            self.weightedDemandingDistance_list = []
            for s in range(len(self.retrofitListBeforeSelection)):
                latlon = self.retrofitListBeforeSelection[s]["LatLon"]
                populationListForOneSite = []
                sumUpOfWeightedDemanding = 0
                for rs in rs_List:
                    population = populationDensityCalculator(latlon, rs, self.geospatialQueryEndpointLabel)
                    populationListForOneSite.append(population)
                self.population_list.append(populationListForOneSite)

                ## pre-calculation of demanding distance
                for demand in self.demandingAreaList:
                    LA_code  = demand["Area_LACode"]
                    ## Avoid 
                    if LA_code in ["K03000001", "K02000001", "W92000004","S92000003", "E12000001", "E12000002", 
                                    "E12000003", "E12000004", "E12000005", "E12000006", "E12000007", "E12000008", 
                                    "E12000009", "E13000001", "E13000002"]:
                        print(LA_code)
                        print(self.demandingAreaList.index(demand))
                        print("This LA code should not be taked into the considerations.")
                        continue
                    distance = DistanceBasedOnGPSLocation(latlon + demand['Geo_InfoList'])
                    weightedDemanding = distance * float(demand['v_TotalELecConsumption'])  
                    sumUpOfWeightedDemanding += weightedDemanding
                self.weightedDemandingDistance_list.append(sumUpOfWeightedDemanding)   
            return

    """The generator cluster function: cluster the generator who gets too close to each other (the safty distance is 30 km)"""
    def siteCluster(self):
        location = []
        ## Form the loaction point list for clustering
        for gen in self.retrofitListBeforeSelection:
            if "#" in gen["LatLon"]:
                gen['LatLon'] = [float(gen['LatLon'].split('#')[0]), float(gen['LatLon'].split('#')[1])]    
                location.append(gen['LatLon'])  
            else:
                location.append(gen['LatLon'])
        print('The number of the point to be clustered is:', len(location))
        
        ## perform the clustering algorithm: Density-Based Spatial Clustering of Applications with Noise (DBSCAN)
        clustering = DBSCAN(eps = 0.27, min_samples = 2).fit(location)
        label = clustering.labels_
        print('The number of clusters is:',  max(label) + 1)
        print('The number of outliers is:', numpy.count_nonzero(label==-1))
        print('The number of points (sites) after the clustering is:', max(label) + 1 + numpy.count_nonzero(label==-1))

        outliers = []
        beClastered = [ [] for i in range(max(label) + 1) ]
        beClasteredGenerator = [ [] for i in range(max(label) + 1) ]
        ##  capacityForEachCluster = [ 0 for i in range(max(label) + 1) ]
        centriodList = []

        ## classify the outliers and the clustered 
        for l in location:
            i = location.index(l)
            clusteringlabel = label[i]
            if clusteringlabel == -1:
                outliers.append(self.retrofitListBeforeSelection[i])
            else: 
                beClastered[int(clusteringlabel)].append((l[1], l[0])) ## append the lat lon list
                originalGenerator = self.retrofitListBeforeSelection[i]
                beClasteredGenerator[int(clusteringlabel)].append(originalGenerator)

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
                    'Capacity': 0, 
                    'LatLon': centroid, 
                    'fuelOrGenType': 'SMR', 
                    'annualOperatingHours': 0, 
                    'CO2EmissionFactor': 0.0, 
                    'place': None,
                    'beClusteredGenerators': beclusteredgens}
            clusteredSite.append(gen)

        ## assign the bus to the clustered generators
        gc = generatorCluster.generatorCluster()
        clusteredGeneratorBusPairList = gc.closestBus(self.busNodeList, clusteredSite, [], "BusLatLon", "BusNodeIRI", "LatLon")

        for i in range(len(clusteredSite)):
            busIRI = clusteredGeneratorBusPairList[i]["BusNodeIRI"]
            clusteredSite[i]['Bus'] = busIRI
        self.retrofitListBeforeSelection = outliers + clusteredSite
        return
        
            ###########################NEW SITE SELECTION Pre-OPF#########################
            ## pre-calculation of each site surrounding population density
#             rs_List = []
#             for n in range(self.maxmumSMRUnitAtOneSite):
#                 rs =  (self.NeighbourhoodRadiusForSMRUnitOf1MW/1000) * ((n + 1) * (self.SMRCapability**(0.5)))
#                 rs_List.append(rs)
#             self.population_list = []
#             for s in range(len(self.retrofitListBeforeSelection)):
#                 latlon = self.retrofitListBeforeSelection[s]["LatLon"]
#                 populationListForOneSite = []
#                 for rs in rs_List:
#                     population = populationDensityCalculator(latlon, rs, self.geospatialQueryEndpointLabel)
#                     populationListForOneSite.append(population)
#                 self.population_list.append(populationListForOneSite)
# ## FIXME:
            # ## Pre-determing the demanding ranking of each ara based on the pre-OPF analysis
            # for gen in self.generatorNodeList:
            #     if len(gen) < 10:
            #         raiseExceptions('The powerPlantAndDemandingAreasMapper should be ran before this function being called.')
            #     for potentialSiteGen in self.retrofitListBeforeSelection:
            #         if gen[0] == potentialSiteGen['PowerGenerator']:
            #             potentialSiteGen['LACode_SmallerArea'] = gen[11]
            
            # netDemandingIndicator_busArea = []
            # for gen in self.retrofitListBeforeSelection:
            #     for BusNode in self.netDemandingList_busRadiatingArea:
            #         if gen['Bus'] == BusNode[0]:
            #             netDemandingIndicator_busArea.append(BusNode[1])
            # if len(netDemandingIndicator_busArea) != len(self.retrofitListBeforeSelection):
            #     raiseExceptions('The netDemandingIndicator_busArea must have the same length as the retrofit list.')
            
            # netDemandingIndicator_smallDemandingArea = []
            # for gen in self.retrofitListBeforeSelection:
            #     for demanding in self.netDemandingList_smallArea:
            #         if gen['LACode_SmallerArea'] == demanding[0]:
            #             netDemandingIndicator_smallDemandingArea.append(demanding[1])
            # if len(netDemandingIndicator_smallDemandingArea) != len(self.retrofitListBeforeSelection):
            #     raiseExceptions('The netDemandingIndicator_smallDemandingArea must have the same length as the retrofit list.')
            
            # for i in range(len(self.retrofitListBeforeSelection)):
            #     demandingWeighter = -1 * (netDemandingIndicator_busArea[i] * 10000000000 + netDemandingIndicator_smallDemandingArea[i])
            #     self.demandingWeighterList.append(demandingWeighter)        
         
    """The SMR site selection algorithm"""   
    def siteSelector(self, numberOfSMRToBeIntroduced):
        if self.withRetrofit is True: 
            self.numberOfSMRToBeIntroduced = numberOfSMRToBeIntroduced
            contradictedPairs = []
            i = 0 
            while i < len(self.retrofitListBeforeSelection):
                LatLon_i = self.retrofitListBeforeSelection[i]['LatLon']
                j = i + 1
                while j < len(self.retrofitListBeforeSelection): 
                    LatLon_j = self.retrofitListBeforeSelection[j]['LatLon']
                    distanceBetweenij = DistanceBasedOnGPSLocation(LatLon_i + LatLon_j)
                    if distanceBetweenij < self.safeDistance:
                        contradictedPairs.append([i, j])
                    j += 1
                i += 1
 
            ## Initialise the selector
            siteSelector = sp_pymoo.siteSelector(numberOfSMRToBeIntroduced, self.geospatialQueryEndpointLabel, self.retrofitListBeforeSelection, self.discountRate, self.projectLifeSpan, self.SMRCapitalCost,
            self.MonetaryValuePerHumanLife, self.NeighbourhoodRadiusForSMRUnitOf1MW, self.ProbabilityOfReactorFailure, self.SMRCapability, self.bankRate,
            self.maxmumSMRUnitAtOneSite, self.SMRIntergratedDiscount, self.startTime_of_EnergyConsumption, self.population_list, self.weightedDemandingDistance_list, contradictedPairs)
            ## Selecte the Genetic Algorithm NSGA2
            algorithm = NSGA2(
                pop_size = self.pop_size, ## the initial population size 
                n_offsprings = self.n_offsprings, ## the number of the offspring of each generation 
                sampling=IntegerRandomSampling(),
                crossover=SBX(prob=1.0, eta=3.0, vtype=float, repair=RoundingRepair()),
                mutation=PM(prob=1.0, eta=3.0, vtype=float, repair=RoundingRepair()),
                eliminate_duplicates=True)

            res = minimize(siteSelector,
                        algorithm,
                        ('n_gen', int(self.numberOfGenerations)),
                        seed=1,
                        verbose=True,
                        save_history=True
                        )        
                        
            print("Best solution found: %s" % res.X)
            print("Function value: %s" % res.F)

            self.res = res
            self.X = res.X
            self.F = res.F
            return

    """This method is used to pick up the optima from the pareto front according to the given weighter"""
    def optimaPicker(self, weighterList):
        self.indexListOfSiteSelectionResults = []
        self.retrofittingCostList = []
        self.weighterList = weighterList

        ## Form the weight list
        weightNumpyMatrix = numpy.zeros((len(weighterList),2))
        for i in range(len(weighterList)):
            if float(weighterList[i]) > 1 or float(weighterList[i]) < 0:
                raise ValueError("Invalid weight: %s" % weighterList[i])
            if float(weighterList[i]) < 0.000001:
                weighterList[i] = float(weighterList[i]) + 0.000001
            elif float(weighterList[i]) > 9.999999 or abs(float(weighterList[i]) -1) < 0.000001:
                weighterList[i] = float(weighterList[i]) - 0.000001

            weightNumpyMatrix[i, 0] = weighterList[i]
            weightNumpyMatrix[i, 1] = float(1 - weighterList[i])

        ## Normalisation the objective space: apply the ideal and nadir point method 
        self.approx_ideal = self.F.min(axis = 0)
        self.approx_nadir = self.F.max(axis = 0)
        self.nF = (self.F - self.approx_ideal) / (self.approx_nadir - self.approx_ideal)
        ## Decomposition method called Augmented Scalarization Function (ASF),
        decomp = ASF()
        indexOfOptima = []
        for weights in weightNumpyMatrix:
            print(weights)
            i = decomp.do(self.nF, 1/weights).argmin()
            indexOfOptima.append(i)
            self.indexListOfSiteSelectionResults.append(numpy.where(self.X[i] == 1)[0])
            self.retrofittingCostList.append(round(float(self.F[i, 0]), 2))

        ## Results diagram: Pareto Front, feasible points and optima 
        #_X = numpy.row_stack([a.pop.get("X") for a in self.res.history])
        _F = numpy.row_stack([a.pop.get("F") for a in self.res.history])
        feasible = numpy.row_stack([a.pop.get("feasible") for a in self.res.history])[:, 0] 
        ## Real feasible points
        feasibleSolustions_F = _F[feasible]
        ## Normalised feasible points
        self.approx_ideal_feasibleNormalised = feasibleSolustions_F.min(axis = 0)
        self.approx_nadir_feasibleNormalised = feasibleSolustions_F.max(axis = 0)
        self.nF_feasibleNormalised = (self.F - self.approx_ideal_feasibleNormalised) / (self.approx_nadir_feasibleNormalised - self.approx_ideal_feasibleNormalised)
        feasibleSolustions_F_feasibleNormalised = (feasibleSolustions_F - self.approx_ideal_feasibleNormalised) / (self.approx_nadir_feasibleNormalised - self.approx_ideal_feasibleNormalised)

        plt.scatter(feasibleSolustions_F_feasibleNormalised[:, 0], feasibleSolustions_F_feasibleNormalised[:, 1], label='Normalised Feasible Solutions', alpha=0.3, s=20, facecolors='#728FCE', edgecolors='none')
        plt.scatter(self.nF_feasibleNormalised[:, 0], self.nF_feasibleNormalised[:, 1], label= 'Normalised Pareto Front', alpha=0.7, s=30, facecolors='#FF8C00', edgecolors='none')
        
        # optima_label_list = ['weight:' + str(round(weightNumpyMatrix[i_, 0], 2)) + ',' + str(round(weightNumpyMatrix[i_, 1], 2)) for i_ in range(len(indexOfOptima)) ]
        optima_label_list = ['weight:' + str(round(weightNumpyMatrix[i_, 0], 2))  for i_ in range(len(indexOfOptima)) ]
        
        x_data = []
        y_data = []
        for i in indexOfOptima:
            x_data.append(self.nF_feasibleNormalised[i, 0])
            y_data.append(self.nF_feasibleNormalised[i, 1])
            plt.scatter(self.nF_feasibleNormalised[i, 0], self.nF_feasibleNormalised[i, 1], marker="x", alpha=0.8, s=40, color = '#00A36C')
        pointAndText = [plt.text(x_, y_, label, fontsize = 13) for x_, y_, label in zip(x_data, y_data, optima_label_list)]
        adjust_text(pointAndText, only_move={'text': 'y'}, arrowprops=dict(arrowstyle='-', color='grey')) 
        # for i in indexOfOptima:
        #     i_ = indexOfOptima.index(i)
        #     plt.scatter(self.nF_feasibleNormalised[i, 0], self.nF_feasibleNormalised[i, 1], marker="x", alpha=0.8, s=40, color = '#00A36C')# facecolors='#00A36C', edgecolors='none')
        #     weightLabel = 'weight:' + str(round(weightNumpyMatrix[i_, 0], 2)) + ',' + str(round(weightNumpyMatrix[i_, 1], 2))
        #     plt.annotate(weightLabel, (self.nF_feasibleNormalised[i, 0], self.nF_feasibleNormalised[i, 1]), fontsize = 8, xycoords='data')    
        ## plt.title("Normalised Objective Space")
        plt.xlabel("Normalised SMR Investment and Risk Cost (-)", fontsize = labelFontSize)
        plt.ylabel("Normalised Load-Demand Distance (-)", fontsize = labelFontSize) 
        plt.legend(fontsize = legendFontSize, loc='upper left')
        plt.tight_layout()
        self.mkdirFig()
        plt.savefig(self.diagramPath + 'SMR_%s.pdf' % str(self.numberOfSMRToBeIntroduced), dpi = 1200, bbox_inches='tight')
        ## plt.show() ## show must come after the savefig
        ##plt.close()
        plt.clf()
        plt.cla()
        ##OLD METHOD FOR PLOTTING: plotting.plot(feasibleSolustions_F, self.F, optima, show=True, labels=["Feasible", "Pareto front", "Optima"])
            
        ##-- Create the SMR instances according to the site selection and the optima pick processing --##
        ## self.SMRList is a list of lists containing the SMR instances that are selected from different weighters
        self.SMRList =[] ## the length of the list of the self.SMRList should equal to the number of the weighters at the same weather condition and same carbon tax
        for indexListOfResults_EachWeight in self.indexListOfSiteSelectionResults: 
            SMRArrangement = []
            for index in indexListOfResults_EachWeight:
                s = index // self.maxmumSMRUnitAtOneSite
                numOfSMRUnit = (index % self.maxmumSMRUnitAtOneSite) + 1
                ## vibrate the site location from the original site a bit
                lalon = [float(self.retrofitListBeforeSelection[s]["LatLon"][0]) + 0.004, float(self.retrofitListBeforeSelection[s]["LatLon"][1]) + 0.004]
                ## initialise the SMR generator with the atttributes
                SMRSite = {'PowerGenerator': None, 
                'Bus': self.retrofitListBeforeSelection[s]["Bus"], 
                'Capacity': numOfSMRUnit * self.SMRCapability, 
                'LatLon': lalon,
                'fuelOrGenType': 'SMR', 
                'annualOperatingHours': 0, 
                'CO2EmissionFactor': 0.0, 
                'place': None,
                'NumberOfSMRUnits': numOfSMRUnit}
                SMRArrangement.append(SMRSite)
            self.SMRList.append(SMRArrangement)
            ## i_ = self.indexListOfSiteSelectionResults.index(indexListOfResults_EachWeight)
            ## weightLabel = 'weight:' + str(round(weightNumpyMatrix[i_, 0], 2)) + ',' + str(round(weightNumpyMatrix[i_, 1], 2))
            ## print("At %s" % weightLabel)
            ## print("The total number of SMR sites is", len(indexListOfResults_EachWeight))
        return 
        
    """This method is called to initialize the model entities objects: bus and branch (this initialisation will not be affected by SMR introduction or Carbon tax change"""
    def ModelPythonObjectInputInitialiser_BusAndBranch(self): 
        self.BusObjectList = []
        self.BranchObjectList = []
        self.OrderedBusNodeIRIList = []
        ###=== Initialisation of the Bus Model Entities ===###
        ## create an instance of class demandLoadAllocator
        dla = DLA.demandLoadAllocator()
        ## get the load allocation method via getattr function 
        allocator = getattr(dla, self.loadAllocatorName)
        ## pass the arrguments to the cluster method
        EBus_Load_List, self.demandingAreaList, aggregatedBusFlag = allocator(self.busNodeList, self.startTime_of_EnergyConsumption, self.numOfBus, self.demandingAreaList, "BusLatLon") # busNodeList[0]: EquipmentConnection_EBus, busNodeList[1]: v_TotalELecConsumption 
        ## check if the allocator method is applicable
        while EBus_Load_List == None:
            loadAllocatorName = str(input('The current allocator is not applicable. Please choose another allocator: '))
            # get the load allocation method via getattr function 
            allocator = getattr(dla, loadAllocatorName)
            # pass the arrguments to the cluster method
            EBus_Load_List, self.demandingAreaList, aggregatedBusFlag = allocator(self.busNodeList, self.startTime_of_EnergyConsumption, self.numOfBus, self.demandingAreaList, "BusLatLon") # busNodeList[0]: EquipmentConnection_EBus, busNodeList[1]: v_TotalELecConsumption 
        ##The sum up of the load of the aggegated bus is done in the loadAllocatorName
        if aggregatedBusFlag == True:
            EBus_Load_List = model_EBusABoxGeneration.addUpConsumptionForAggregatedBus(EBus_Load_List) # sum up the demand of an AggregatedBus
        
        self.busNodeList = EBus_Load_List
        
        for ebus in self.busNodeList:
            objectName = UK_PG.UKEbusModel.EBusKey + str(self.busNodeList.index(ebus)) ## bus model python object name
            uk_ebus_model = UK_PG.UKEbusModel(int(self.numOfBus), str(ebus["BusNodeIRI"]))
            # create an instance of class initialiseEBusModelVariable
            initialiseEbus = InitialiseEbus.initialiseEBusModelVariable()
            # get the initialiser via getattr function 
            initialiser = getattr(initialiseEbus, self.EBbusInitialisationMethodName)
            # pass the arrguments to the initialiser method
            self.ObjectSet[objectName] = initialiser(uk_ebus_model, ebus, self.busNodeList.index(ebus), self.slackBusNodeIRI) 
            self.BusObjectList.append(objectName)
            self.OrderedBusNodeIRIList.append(ebus["BusNodeIRI"])
            print('the bus type is ',uk_ebus_model.TYPE)

        ###=== Initialisation of the Branch Model Entities ===###
        for eline in self.branchNodeList:
            objectName = UK_PG.UKElineModel.ELineKey + str(self.branchNodeList.index(eline)) ## eline model python object name
            uk_eline_model = UK_PG.UKElineModel(int(self.numOfBus), str(eline["ELineNode"]), self.ELineInitialisationMethodName)
            ###1. create an instance of the BranchPropertyInitialisation class and get the initialiser method by applying the 'getattr' function 
            initialisation = BPI.BranchPropertyInitialisation()
            initialiser = getattr(initialisation, self.ELineInitialisationMethodName)
            ###2. execute the initialiser with the branch model instance as the function argument 
            self.ObjectSet[objectName] = initialiser(eline['ELineNode'], uk_eline_model, eline, self.branchVoltageLevel, self.OrderedBusNodeIRIList, self.queryUKDigitalTwinEndpointLabel) 
            self.BranchObjectList.append(objectName)
        return

    """This method is called to initialize the model entities objects: Generator"""
    def ModelPythonObjectInputInitialiser_Generator(self, CarbonTaxForOPF, ifWithSMR, windOutputRatio, solarOutputRatio, weatherConditionName, decommissionFlag):  
        ## FIXME: think over the way of introducing the decommission post-process
        # if not decommissionFlag:
        #     self.generatorNodeList = query_model.queryEGenInfo(self.topologyNodeIRI, self.queryUKDigitalTwinEndpointLabel) ## 0?PowerGeneratorIRI 1?FixedMO 2?VarMO 3?FuelCost 4?CO2EmissionFactor 5?Bus 6?Capacity 7?PrimaryFuel 8?Latlon 9?PowerPlant_LACode 10: samllerLAcode   
    
    ## if self.CarbonTaxForOPF != CarbonTaxForOPF or self.weatherConditionName != weatherConditionName: ## The carbon tax changed or the weather condition changed, the generator should be reinitialised
        ## for each OPF with different carbon tax, clean up the list 
            
        self.CarbonTaxForOPF = CarbonTaxForOPF
        self.weatherConditionName = weatherConditionName
        self.GeneratorObjectList = []
        self.SMRSiteObjectList = []

        for SMRList_EachWeight in self.SMRList:
            GeneratorObjectList_EachWeight = []
            SMRSiteObjectList_EachWeight = []
            i = self.SMRList.index(SMRList_EachWeight)
            weighter = str(self.weighterList[i])
            if decommissionFlag:
                self.genTag = "SMRDesign-" + str(self.numberOfSMRToBeIntroduced) + "-weighter" + weighter + "-CarbonTaxForOPF" + str(CarbonTaxForOPF) + "-weatherCondition" + str(weatherConditionName) + "-afterDecommissioned-" ## FIXME: this is the label used in the loop of the old demanding method
            else:
                self.genTag = "SMRDesign-" + str(self.numberOfSMRToBeIntroduced) + "-weighter" + weighter + "-CarbonTaxForOPF" + str(CarbonTaxForOPF) + "-weatherCondition" + str(weatherConditionName) + "-beforeDecommissioned-" ## FIXME: this is the label used in the loop of the old demanding method
            
            for egen in self.generatorNodeList:
                objectName = UK_PG.UKEGenModel.EGenKey + self.genTag + str(self.generatorNodeList.index(egen)) ## egen model python object name
                uk_egen_OPF_model = UK_PG.UKEGenModel_CostFunc(int(self.numOfBus), str(egen[0]), float(egen[4]), str(egen[7]), egen[8], float(egen[6]), None, str(egen[9]), CarbonTaxForOPF, self.piecewiseOrPolynomial, self.pointsOfPiecewiseOrcostFuncOrder)
                uk_egen_OPF_model = costFuncPara(uk_egen_OPF_model, egen)
                ###add EGen model parametor###
                self.ObjectSet[objectName] = model_EGenABoxGeneration.initialiseEGenModelVar(uk_egen_OPF_model, egen, self.OrderedBusNodeIRIList, self.capa_demand_ratio, windOutputRatio, solarOutputRatio)
                GeneratorObjectList_EachWeight.append(objectName)
            self.GeneratorObjectList.append(GeneratorObjectList_EachWeight)
        
            ### Initialisation of the SMR Generator Model Entities ###
            if self.withRetrofit is True and ifWithSMR is True:
                ## extract the emssion factor
                for i in range(len(modelFactorArrays)):
                    if str(self.newGeneratorType) in modelFactorArrays[i]:
                        factorArray = modelFactorArrays[i]
                        break
                if not 'factorArray' in locals():
                    raiseExceptions("The given generator type which used to retrofit the existing genenrators cannot be found in the factor list, please check the generator type.")

                for egen_re in SMRList_EachWeight:
                    objectName = UK_PG.UKEGenModel.EGenRetrofitKey + self.genTag + str(SMRList_EachWeight.index(egen_re)) 
                    newGeneratorNodeIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + ukpp.RealizationAspectKey + str(uuid.uuid4()) 
                    uk_egen_re_OPF_model = UK_PG.UKEGenModel_CostFunc(int(self.numOfBus), newGeneratorNodeIRI, 0, str(self.newGeneratorType), egen_re["LatLon"], egen_re["Capacity"], str(egen_re["PowerGenerator"]), 'Added', CarbonTaxForOPF, self.piecewiseOrPolynomial, self.pointsOfPiecewiseOrcostFuncOrder)
                    egen_re = [newGeneratorNodeIRI, float(factorArray[1]), float(factorArray[2]), float(factorArray[3]), float(factorArray[4]), egen_re["Bus"], egen_re["Capacity"], self.newGeneratorType] ## ?PowerGenerator ?FixedMO ?VarMO ?FuelCost ?CO2EmissionFactor ?Bus ?Capacity ?fuel type
                    uk_egen_re_OPF_model = costFuncPara(uk_egen_re_OPF_model, egen_re)
                    ###add EGen model parametor###
                    self.ObjectSet[objectName] = model_EGenABoxGeneration.initialiseEGenModelVar(uk_egen_re_OPF_model, egen_re, self.OrderedBusNodeIRIList, self.capa_demand_ratio, windOutputRatio, solarOutputRatio)
                    SMRSiteObjectList_EachWeight.append(objectName)
                self.SMRSiteObjectList.append(SMRSiteObjectList_EachWeight)
        return    

    ## generate a list of the ppc at different weighters and the same weather condition and same carbon tax   
    def OPFModelInputFormatter(self):
        """
        The OPFModelInputFormatter is used to created the pypower OPF model input from the model objects which are created from the F{ModelObjectInputInitialiser}.
        This function will be called after F{ModelObjectInputInitialiser}.
        
        Raises
        ------
        Exception
            If the attribute ObjectSet does not exist.

        Returns
        -------
        None.
            
        """

        if not hasattr(self, 'ObjectSet'):  
            raise Exception("The model object has not been properly created, please run the function ModelObjectInputInitialiser at first.")

        ##-- Format the PF analysis input, ppc --##
        ppc: dict = {"version": '2'}
        
        ## system MVA base
        ppc["baseMVA"] = float(self.baseMVA)
        
        ## bus data
        # bus_i type Pd Qd Gs Bs area Vm Va baseKV zone Vmax Vmin  
        ppc["bus"] = numpy.zeros((self.numOfBus, len(UK_PG.UKEbusModel.INPUT_VARIABLE_KEYS)), dtype = float)
        index_bus  = 0
        while index_bus < self.numOfBus:
            objectiveName = self.BusObjectList[index_bus]
            for key in UK_PG.UKEbusModel.INPUT_VARIABLE_KEYS:
                index = int(UK_PG.UKEbusModel.INPUT_VARIABLE[key])
                ppc["bus"][index_bus][index] = getattr(self.ObjectSet.get(objectiveName), key)
            index_bus += 1
            
        ## branch data
        # fbus, tbus, r, x, b, rateA, rateB, rateC, ratio, angle, status, angmin, angmax     
        ppc["branch"] = numpy.zeros((len(self.BranchObjectList), len(UK_PG.UKElineModel.INPUT_VARIABLE_KEYS)), dtype = float)
        index_br  = 0
        while index_br < len(self.BranchObjectList):
            objectiveName = self.BranchObjectList[index_br]
            for key in UK_PG.UKElineModel.INPUT_VARIABLE_KEYS:
                index = int(UK_PG.UKElineModel.INPUT_VARIABLE[key])
                ppc["branch"][index_br][index] = getattr(self.ObjectSet.get(objectiveName), key)
            index_br += 1

        self.ppc_List = []

        for SMRSiteObjectList_EachWeight in self.SMRSiteObjectList:
            i_ = self.SMRSiteObjectList.index(SMRSiteObjectList_EachWeight)
            GeneratorObjectList_EachWeight = self.GeneratorObjectList[i_]
            ## generator data
            # bus, Pg, Qg, Qmax, Qmin, Vg, mBase, status, Pmax, Pmin, Pc1, Pc2,
            # Qc1min, Qc1max, Qc2min, Qc2max, ramp_agc, ramp_10, ramp_30, ramp_q, apf
            numOfExistAndRetrofittedGenerators = len(GeneratorObjectList_EachWeight) + len(SMRSiteObjectList_EachWeight) # len(self.GeneratorToBeRetrofittedObjectList) + len(self.GeneratorToBeBackUpObjectList) + len(self.GeneratorToBeAllRetrofittedOrAllBackUpObjectList)
            
            ppc["gen"] = numpy.zeros((numOfExistAndRetrofittedGenerators, len(UK_PG.UKEGenModel.INPUT_VARIABLE_KEYS)), dtype = float)
            index_gen  = 0 
            while index_gen < len(GeneratorObjectList_EachWeight):
                genObjectiveName = GeneratorObjectList_EachWeight[index_gen]
                for key in UK_PG.UKEGenModel.INPUT_VARIABLE_KEYS:
                    index = int(UK_PG.UKEGenModel.INPUT_VARIABLE[key])
                    ## ppc["gen"][index_gen][index] = getattr(self.ObjectSet.get(UK_PG.UKEGenModel.EGenKey + self.extantGenTag + str(index_gen)), key)
                    ppc["gen"][index_gen][index] = getattr(self.ObjectSet.get(genObjectiveName), key)
                index_gen += 1

            if self.withRetrofit is True: 
                index_regen = 0
                while index_gen < numOfExistAndRetrofittedGenerators:
                    SMRObjectiveName = SMRSiteObjectList_EachWeight[index_regen]
                    for key in UK_PG.UKEGenModel.INPUT_VARIABLE_KEYS:
                        index = int(UK_PG.UKEGenModel.INPUT_VARIABLE[key])                 
                        ppc["gen"][index_gen][index] = getattr(self.ObjectSet.get(SMRObjectiveName), key)
                    index_gen += 1
                    index_regen += 1

            ## generator COST data
            # MODEL, STARTUP, SHUTDOWN, NCOST, COST[a, b]
            columnNum = len(UK_PG.UKEGenModel_CostFunc.INPUT_VARIABLE_KEYS) + self.pointsOfPiecewiseOrcostFuncOrder -1
            ppc["gencost"] = numpy.zeros((numOfExistAndRetrofittedGenerators, columnNum), dtype = float)
            index_gen  = 0
            while index_gen < len(GeneratorObjectList_EachWeight):
                genObjectiveName = GeneratorObjectList_EachWeight[index_gen]
                for key in UK_PG.UKEGenModel_CostFunc.INPUT_VARIABLE_KEYS:
                    index = int(UK_PG.UKEGenModel_CostFunc.INPUT_VARIABLE[key])
                    if key == "COST":
                        ## for para in getattr(self.ObjectSet.get(UK_PG.UKEGenModel.EGenKey + self.extantGenTag + str(index_gen)), key):
                        for para in getattr(self.ObjectSet.get(genObjectiveName), key):
                            ppc["gencost"][index_gen][index] = para
                            index += 1
                    else:
                        ## ppc["gencost"][index_gen][index] = getattr(self.ObjectSet.get(UK_PG.UKEGenModel.EGenKey + self.SMRTag + str(index_gen)), key)
                        ppc["gencost"][index_gen][index] = getattr(self.ObjectSet.get(genObjectiveName), key)
                index_gen += 1

            if self.withRetrofit is True: 
                index_regen = 0
                while index_gen < numOfExistAndRetrofittedGenerators:
                    SMRObjectiveName = SMRSiteObjectList_EachWeight[index_regen]
                    for key in UK_PG.UKEGenModel_CostFunc.INPUT_VARIABLE_KEYS:
                        index = int(UK_PG.UKEGenModel_CostFunc.INPUT_VARIABLE[key])
                        if key == "COST":
                            for para in getattr(self.ObjectSet.get(SMRObjectiveName), key):
                                ppc["gencost"][index_gen][index] = para
                                index += 1
                        else:
                            ppc["gencost"][index_gen][index] = getattr(self.ObjectSet.get(SMRObjectiveName), key)
                    index_gen += 1
                    index_regen += 1
            ppc_copy = ppc.copy()
            self.ppc_List.append(ppc_copy)
        return 

    
    ## Generate the list of the results, total cost and OPEX for each weighter at the same weather and same carbon tax
    def OptimalPowerFlowAnalysisSimulation(self, ppc: list = None):
        """
        Perform the optimal power flow analysis.

        Parameters
        ----------
        ppc : List
            ppc is the list of the input for optimal power flow model. The default is None.

        Returns
        -------
        None.

        """
         
        if not hasattr(self, 'ppc') and not hasattr(self, 'ppc_List'): 
            if ppc is None or not isinstance(ppc, list):
                raise Exception("The model input has not been reformatted, please run the function ModelInputFormatter at first.")
            else:
                self.ppc_List = ppc

        ## Clean the list of the results for each carbontax-weather condition 
        self.annualisedOPEXList = []
        self.totalCostList = []
        self.resultsList = []
        self.OPEXRatioList = []
        
        # set up numerical method: Newton's Method
        ppopt = ppoption(OUT_ALL = 0, VERBOSE = 2) 
        
        for ppc in self.ppc_List: ## the length of the ppc_List should equal to the number of the weights
            i = self.ppc_List.index(ppc)
            ##-- starts opf analysis --##
            results = runopf(ppc, ppopt)
            self.resultsList.append(results)

            ## there are several different ways to do OPF analysis
            # results = runuopf(ppc, ppopt)

            annualOperationCost_OPF = round(results["f"], 2) * 8760
            totalOperationCost_OPF = 0
            for l in range(self.projectLifeSpan):
                ## l starts frm 0, therefore it is no need to use -(l-1) bus just use -l
                totalOperationCost_OPF += annualOperationCost_OPF * (1 + float(self.bankRate)) **(-l)
            
            annualisedOPEX = round((totalOperationCost_OPF * self.discountRate / (1 - ((1 + self.discountRate)**(-1 * self.projectLifeSpan)))), 2)
            self.annualisedOPEXList.append(annualisedOPEX)
            OPEXRatio = round(annualisedOPEX/(annualisedOPEX + self.retrofittingCostList[i]), 2)
            self.totalCostList.append(round((annualisedOPEX + self.retrofittingCostList[i]), 2))
            self.OPEXRatioList.append(OPEXRatio)
            # percentageOfOPEX = round(self.annualisedOPEX/self.totalCost, 2)
            # percentageOfCAPEX = round((1- percentageOfOPEX),2)

            print("***Total cost (£): ", round((annualisedOPEX + self.retrofittingCostList[i]), 2))
            print("***Annualised OPEX cost (£): ", annualisedOPEX) ## calculated from OPF, OPEX 
            print("***RetrofittingCost (CAPEX) cost (£): ", self.retrofittingCostList[i]) ## calculated from site selection, CAPEX
            ## print("***Percentage of OPEX: ", percentageOfOPEX, "and the percentage of CAPEX is: ", percentageOfCAPEX)

            ConvergeFlag = results["success"]
            if ConvergeFlag is True:
                print('-----The OPF model is converged.-----')
            else:
                print('!!!!!!The OPF model is diverged.!!!!!')
        return

    def ModelOutputFormatter(self, generateVisualisationJSON:bool):
        """
        Reformat the result and add attributes into the objects.

        Returns
        -------
        None.

        """
        self.SMRTotalOutputList = []
        self.SMRTotalOperationalRationList = []
        
        for index_ in range(len(self.resultsList)): ## the length of the results equals to the length of the weights
            results = self.resultsList[index_]
            GeneratorObjectList_EachWeight = self.GeneratorObjectList[index_]
            SMRSiteObjectList_EachWeight = self.SMRSiteObjectList[index_]
            weightForObjective1 = round(self.weighterList[index_], 2)

            ## the bus, gen, branch and loss result
            bus = results["bus"]
            branch = results["branch"]
            gen = results["gen"]
            
            ##--Bus--##  
            ##  VM_OUTPUT, VM_OUTPUT, P_GEN, G_GEN, PD_OUTPUT, GD_OUTPUT        
            ## post processsing of the bus results   
            busPostResult = numpy.zeros((self.numOfBus, len(UK_PG.UKEbusModel.OUTPUT_VARIABLE_KEYS)), dtype = float) 
            
            for i in range(self.numOfBus):
                busPostResult[i][UK_PG.UKEbusModel.OUTPUT_VARIABLE["VM_OUTPUT"]] = bus[i][VM]
                busPostResult[i][UK_PG.UKEbusModel.OUTPUT_VARIABLE["VA_OUTPUT"]] = bus[i][VA]
                
                g  = find((gen[:, GEN_STATUS] > 0) & (gen[:, GEN_BUS] == bus[i, BUS_I]) & ~isload(gen))
                ld = find((gen[:, GEN_STATUS] > 0) & (gen[:, GEN_BUS] == bus[i, BUS_I]) & isload(gen))
                
                if any(g + 1):
                    busPostResult[i][UK_PG.UKEbusModel.OUTPUT_VARIABLE["P_GEN"]] = sum(gen[g, PG])
                    busPostResult[i][UK_PG.UKEbusModel.OUTPUT_VARIABLE["G_GEN"]] = sum(gen[g, QG])
                        
                if logical_or(bus[i, PD], bus[i, QD]) | any(ld + 1):
                    if any(ld + 1):
                        busPostResult[i][UK_PG.UKEbusModel.OUTPUT_VARIABLE["PD_OUTPUT"]] = bus[i, PD] - sum(gen[ld, PG])
                        busPostResult[i][UK_PG.UKEbusModel.OUTPUT_VARIABLE["GD_OUTPUT"]] = bus[i, QD] - sum(gen[ld, QG])    
                    else:
                        busPostResult[i][UK_PG.UKEbusModel.OUTPUT_VARIABLE["PD_OUTPUT"]] = bus[i][PD]
                        busPostResult[i][UK_PG.UKEbusModel.OUTPUT_VARIABLE["GD_OUTPUT"]] = bus[i][QD]
                
            ## update the object attributes with the model results
            index_bus  = 0
            while index_bus < self.numOfBus:
                objectiveName = self.BusObjectList[index_bus]
                for key in UK_PG.UKEbusModel.OUTPUT_VARIABLE_KEYS:
                    index = int(UK_PG.UKEbusModel.OUTPUT_VARIABLE[key])
                    setattr(self.ObjectSet.get(objectiveName), key, busPostResult[index_bus][index])        
                index_bus += 1   
            
            ##--Branch--##    
            ## FROMBUSINJECTION_P, FROMBUSINJECTION_Q, TOBUSINJECTION_P, TOBUSINJECTION_Q, LOSS_P, LOSS_Q
            ## post processsing of the branch results   
            branchPostResult = numpy.zeros((len(self.BranchObjectList), len(UK_PG.UKElineModel.OUTPUT_VARIABLE_KEYS)), dtype = float) 
            
            for i in range(len(self.BranchObjectList)):
                branchPostResult[i][UK_PG.UKElineModel.OUTPUT_VARIABLE["FROMBUSINJECTION_P"]] = branch[i][PF]
                branchPostResult[i][UK_PG.UKElineModel.OUTPUT_VARIABLE["FROMBUSINJECTION_Q"]] = branch[i][QF]
                branchPostResult[i][UK_PG.UKElineModel.OUTPUT_VARIABLE["TOBUSINJECTION_P"]] = branch[i][PT]
                branchPostResult[i][UK_PG.UKElineModel.OUTPUT_VARIABLE["TOBUSINJECTION_Q"]] = branch[i][QT]
                branchPostResult[i][UK_PG.UKElineModel.OUTPUT_VARIABLE["LOSS_P"]] = abs(abs(branch[i][PF]) - abs(branch[i][PT]))  
                branchPostResult[i][UK_PG.UKElineModel.OUTPUT_VARIABLE["LOSS_Q"]] = abs(abs(branch[i][QF]) - abs(branch[i][QT]))  
                
            ## update the object attributes with the model results
            index_br  = 0
            while index_br < len(self.BranchObjectList):
                objectiveName = self.BranchObjectList[index_br]
                for key in UK_PG.UKElineModel.OUTPUT_VARIABLE_KEYS:
                    index = int(UK_PG.UKElineModel.OUTPUT_VARIABLE[key])
                    setattr(self.ObjectSet.get(objectiveName), key, branchPostResult[index_br][index])        
                index_br += 1   
            
            ##--Generator--##    
            ## PG_OUTPUT, QG_OUTPUT
            ## post processsing of the generator results 
            numOfExistAndRetrofittedGenerators = len(GeneratorObjectList_EachWeight) + len(SMRSiteObjectList_EachWeight)  
            generatorPostResult = numpy.zeros((numOfExistAndRetrofittedGenerators, len(UK_PG.UKEGenModel.OUTPUT_VARIABLE_KEYS)), dtype = float) 
            for i in range(numOfExistAndRetrofittedGenerators):
                if (gen[i, GEN_STATUS] > 0) & logical_or(gen[i, PG], gen[i, QG]):
                    generatorPostResult[i][UK_PG.UKEGenModel.OUTPUT_VARIABLE["PG_OUTPUT"]] = gen[i][PG]
                    generatorPostResult[i][UK_PG.UKEGenModel.OUTPUT_VARIABLE["QG_OUTPUT"]] = gen[i][QG]
            ## update the object attributes with the model results
            index_gen  = 0
            while index_gen < len(GeneratorObjectList_EachWeight):
                objectiveName = GeneratorObjectList_EachWeight[index_gen]
                for key in UK_PG.UKEGenModel.OUTPUT_VARIABLE_KEYS:
                    index = int(UK_PG.UKEGenModel.OUTPUT_VARIABLE[key])
                    setattr(self.ObjectSet.get(objectiveName), key, generatorPostResult[index_gen][index])        
                index_gen += 1

            if self.withRetrofit is True:
                index_regen = 0
                while index_gen < numOfExistAndRetrofittedGenerators:
                    objectiveName = SMRSiteObjectList_EachWeight[index_regen]
                    for key in UK_PG.UKEGenModel.OUTPUT_VARIABLE_KEYS:
                        index = int(UK_PG.UKEGenModel.OUTPUT_VARIABLE[key])                  
                        setattr(self.ObjectSet.get(objectiveName), key, generatorPostResult[index_gen][index])        
                    index_regen += 1
                    index_gen += 1

                totalSMROutput = 0
                for SMRName in SMRSiteObjectList_EachWeight:
                    totalSMROutput += round(float(self.ObjectSet[SMRName].PG_OUTPUT),2)
                self.SMRTotalOutputList.append(totalSMROutput)
                if self.numberOfSMRToBeIntroduced == 0:
                    ratio = 0
                else:
                    ratio = round((totalSMROutput / (self.numberOfSMRToBeIntroduced * self.SMRCapability)), 2)
                self.SMRTotalOperationalRationList.append(ratio)

            #FIXME: modify the ModelPythonObjectOntologiser method 
            ## self.ModelPythonObjectOntologiser() 
            
            if generateVisualisationJSON:
                ExtantGeneratorLabel = str(self.numOfBus) + 'BusModel_' + str(self.numberOfSMRToBeIntroduced) + '_SMRs_Introduced_CarbonTax' + str(self.CarbonTaxForOPF) + "_WeatherCondition_" + str(self.weatherConditionName) + "_weighter_" + str(weightForObjective1) + '_ExtantGenerator'
                SMRIntroducedLabel = str(self.numOfBus) + 'BusModel_' + str(self.numberOfSMRToBeIntroduced) + '_SMRs_Introduced_CarbonTax' + str(self.CarbonTaxForOPF) + "_WeatherCondition_" + str(self.weatherConditionName) + "_weighter_" + str(weightForObjective1) + '_SMR'
                ClosedGeneratorLabel = str(self.numOfBus) + 'BusModel_' + str(self.numberOfSMRToBeIntroduced) + '_SMRs_Introduced_CarbonTax' + str(self.CarbonTaxForOPF) + "_WeatherCondition_" + str(self.weatherConditionName) + "_weighter_" + str(weightForObjective1) + '_ClosedGenerator'
                DecommissionedLabel = str(self.numOfBus) + 'BusModel_' + str(self.numberOfSMRToBeIntroduced) + '_SMRs_Introduced_CarbonTax' + str(self.CarbonTaxForOPF) + "_WeatherCondition_" + str(self.weatherConditionName) + "_weighter_" + str(weightForObjective1) + '_DecommissionedGenerator'

                self.filePathForJSON = '.\\JSONFiles\\' +  str(self.numberOfSMRToBeIntroduced) + '_SMRs_' + str(self.CarbonTaxForOPF) +'_CarbonTax' 
                
                self.visualisationFileCreator_ExtantGenerator(GeneratorObjectList_EachWeight, ExtantGeneratorLabel) 
                self.visualisationFileCreator_AddedSMRGenerator(SMRSiteObjectList_EachWeight, SMRIntroducedLabel)
                self.visualisationFileCreator_ClosedGenerator(GeneratorObjectList_EachWeight, ClosedGeneratorLabel)
                ## FIXME: add the decommssion visualisationFileCreator
                ## self.visualisationFileCreator_decommissionedGenerator(DecommissionLabel)          
        return 

#FIXME: still need to check why there is no generator not been used for each weather condition
    def decommissionPowerPlantDecider(self, numberOfSMRToBeIntroduced, slackFactor:float, generatorNameList:list):
        index_NonOutputList = []    
        apperanceTime_NonOutputList = []
        alwaysNonOutputList = []
        alwaysNonOutputIndexList = []
        ## Find the shut down power plant in each weather condition scenario and count the number of times
        
        # if type(generatorNameList[0]) is list: 
        #     raiseExceptions("The generator name list should be a 2D list.")
        for genListOfEachWeatherCondition in generatorNameList:
            for genName in genListOfEachWeatherCondition: ## has the same order with the generatorNodeList
                i = genListOfEachWeatherCondition.index(genName)
                if round(float(self.ObjectSet[genName].PG_OUTPUT), 2) < 0.01:
                    if not i in index_NonOutputList:
                        apperanceTime_NonOutputList.append(1)
                        index_NonOutputList.append(i)
                    else:
                        index_ = index_NonOutputList.index(i)
                        apperanceTime_NonOutputList[index_] += 1

        ## Find the always shut down generators and ready to be selected to be decommissioned
        for c in range(len(apperanceTime_NonOutputList)):
            apperanceTime = apperanceTime_NonOutputList[c]
            if apperanceTime == len(generatorNameList):
                alwaysShutDownGenIndex = index_NonOutputList[c]
                alwaysNonOutputIndexList.append(alwaysShutDownGenIndex)
                alwaysNonOutputList.append(self.generatorNodeList[alwaysShutDownGenIndex])

        ##-- Setup a pure economical optimisation MILP model for selecting the power plant to be decommissioned --##
        model = Model("DecommisssionDecider")
        ##-- Binary variable --## 
        binaryVarNameList = []
        sumOfDecommissionCapacity = 0
        totalDecommissioningCost = 0
        for s in range(len(alwaysNonOutputList)):
            binaryVarName = "y_" + str(s)
            self.ObjectSet[binaryVarName] = model.addVar(binaryVarName, vtype = "B")
            binaryVarNameList.append(binaryVarName)

        dcKeys = DCost.keys()
        for bv in binaryVarNameList:
            i = binaryVarNameList.index(bv)
            print(self.ObjectSet[bv])
            sumOfDecommissionCapacity += self.ObjectSet[bv] * float(alwaysNonOutputList[i][6])
            fuelType = alwaysNonOutputList[i][7]
            for key in dcKeys:
                if fuelType in key:
                    totalDecommissioningCost += self.ObjectSet[bv] * DCost[key][2] * float(alwaysNonOutputList[i][6]) * self.discountRate / (1 - ((1 + self.discountRate)**(-1 * self.projectLifeSpan)))
                    break

        ##-- Add Constrains --##  
        model.addCons(sumOfDecommissionCapacity <= numberOfSMRToBeIntroduced * self.SMRCapability * float(slackFactor)) ## slack factor is 1.1

        ##-- Set up the objective function --##
        model.setObjective(totalDecommissioningCost, "minimize")

        ##-- Set up optimisation method --##
        model.optimize()

        ##-- Results post processing --##
        print("The decommission cost is:", model.getObjVal())

        decommissionedPowerPlant_Index = []
        self.decommissionedPowerPlant_generator = []
        for bvname in binaryVarNameList:
            print( bvname, " = ", (model.getVal(self.ObjectSet[bvname])))
            if int(model.getVal(self.ObjectSet[bvname])) > 0:
                decommissionedPowerPlant_Index.append(binaryVarNameList.index(bvname))
        
        TotalDecommissionedCapacity = 0
        for i in decommissionedPowerPlant_Index: 
            TotalDecommissionedCapacity += float(alwaysNonOutputList[i][6])
            self.decommissionedPowerPlant_generator.append(alwaysNonOutputList[i])
            self.generatorNodeList.remove(alwaysNonOutputList[i])
        
        capacityDifference = numberOfSMRToBeIntroduced * self.SMRCapability - TotalDecommissionedCapacity
        
        print("capacityDifference is:", capacityDifference)
        return 

#FIXME: This function is used in the pre-opf method, check the LA code of the demanding, also fix the problem
    def netDemandingCalculator(self):
        ## net demanding of each demanding area
        self.demandingAreaList = demandingAndCentroid[self.startTime_of_EnergyConsumption]
        self.netDemandingList_smallArea = [] ## small areas refer to the demanding areas, ['LA_code', netDemanding]
        self.netDemandingList_busRadiatingArea = [] ## [busIRI, totalNetDemanding of the bus, LACodeList of which demanding area are allocated to this bus]
        LACode_indexingList = []
        busNodeList = []
        netDemanding_busArea = []
        LACodeList_busArea = []
        for demanding in self.demandingAreaList:
            Area_LACode = demanding['Area_LACode']
            if Area_LACode in ["K03000001", "K02000001", "W92000004","S92000003", "E12000001", "E12000002", "E12000003", "E12000004", "E12000005", 
                                "E12000006", "E12000007", "E12000008", "E12000009", "E13000001", "E13000002"]:
                continue
            netDemandingOfThisArea = demanding['v_TotalELecConsumption']
            for i in range(len(self.generatorNodeList)):
                gen = self.generatorNodeList[i]
                if len(gen) < 10:
                    raiseExceptions("The generator has not be attached with a smaller area LA code, please run powerPlantAndDemandingAreasMapper at first or check if", 
                    gen[0], " has not find the smaller area LA code.")
                if str(gen[11]) == Area_LACode:
                    output = float(self.ObjectSet[self.GeneratorObjectList[i]].PG_OUTPUT)
                    netDemandingOfThisArea -= output
            self.netDemandingList_smallArea.append([Area_LACode, netDemandingOfThisArea])
            LACode_indexingList.append(Area_LACode)

        ## net demanding of the bus connected areas
        for ec in self.res_queryElectricityConsumption_LocalArea:
            busIRI = ec['BusNodeIRI']
            LACode = ec['Area_LACode']
            if busIRI in busNodeList:
                index_ = busNodeList.index(busIRI)
                index_smallArea = LACode_indexingList.index(LACode)
                netDemanding_busArea[index_] += self.netDemandingList_smallArea[index_smallArea][1]
                LACodeList_busArea[index_].append(LACode)
            else:
                LACode_indexingList.append(busIRI)
                index_smallArea = LACode_indexingList.index(LACode)
                netDemanding_busArea.append(self.netDemandingList_smallArea[index_smallArea][1])
                LACodeList_busArea.append([LACode])
        for i in range(len(busNodeList)):
            self.netDemandingList_busRadiatingArea.append(busNodeList[i], netDemanding_busArea[i], LACodeList_busArea[i])
        return 

##FIXME: this function should be updated according to the updated version of the code   
    def ModelPythonObjectOntologiser(self):
        """
        create KG representation for all model objects
        
        """
        ## create the power system model node IRI
        self.powerSystemModelIRI = UK_PG.ontopowsys_namespace + UK_PG.powerSystemModelKey + str(uuid.uuid4())
        ## create the timeStamp, e.x. 2022-06-15T16:24:29.371941+00:00
        self.timeStamp = datetime.now(pytz.utc).isoformat()

        BusModelKGInstanceCreator(self.ObjectSet, self.BusObjectList, self.numOfBus, self.topologyNodeIRI, self.powerSystemModelIRI, \
            self.timeStamp, self.agentIRI, self.derivationClient, self.OWLUpdateEndPointURL, self.OWLFileStoragePath)
        BranchModelKGInstanceCreator(self.ObjectSet, self.BranchObjectList, self.numOfBus, self.topologyNodeIRI, self.powerSystemModelIRI, \
            self.timeStamp, self.agentIRI, self.derivationClient, self.OWLUpdateEndPointURL, self.OWLFileStoragePath)
        ##FIXME: self.GeneratorToBeRetrofittedObjectList, self.GeneratorToBeBackUpObjectList, self.GeneratorToBeAllRetrofittedOrAllBackUpObjectList 
        ##FIXME: modify the clustered generator instances
        GeneratorModelKGInstanceCreator(self.ObjectSet, self.GeneratorObjectList, self.GeneratorToBeRetrofittedObjectList, self.OPFOrPF, self.newGeneratorType, self.numOfBus, self.topologyNodeIRI, self.powerSystemModelIRI, \
        self.timeStamp, self.agentIRI, self.derivationClient, self.OWLUpdateEndPointURL, self.OWLFileStoragePath)  
        return 
    
    def CarbonEmissionCalculator(self):
        self.totalCO2EmissionList = []
        self.annualisedTotalEmissionCostList = []
        self.emissionCostContributionList_OPEX = []
        self.emissionCostContributionList_TotalCost = []
        for GeneratorObjectList_EachWeight in self.GeneratorObjectList:
            totalCO2Emission = 0
            emissionCost = 0
            totalEmissionCostOverLifespan = 0
            for gen in GeneratorObjectList_EachWeight:
                totalCO2Emission += float(self.ObjectSet[gen].PG_OUTPUT) * float(self.ObjectSet[gen].CO2EmissionFactor) 
                emissionCost += float(self.ObjectSet[gen].PG_OUTPUT) * float(self.ObjectSet[gen].CO2EmissionFactor) * self.CarbonTaxForOPF 
            self.totalCO2EmissionList.append(totalCO2Emission)
            annualEmissionCost = emissionCost * 8760
            for l in range(self.projectLifeSpan):
                ## l starts frm 0, therefore it is no need to use -(l-1) bus just use -l
                totalEmissionCostOverLifespan += annualEmissionCost * (1 + float(self.bankRate)) **(-l)
            annualisedTotalEmissionCostOverLifespan = round((totalEmissionCostOverLifespan * self.discountRate / (1 - ((1 + self.discountRate)**(-1 * self.projectLifeSpan)))), 2)
            self.annualisedTotalEmissionCostList.append(annualisedTotalEmissionCostOverLifespan)
        
        for annualisedOPEX in self.annualisedOPEXList:
            emissionCostContribution_OPEX = round((annualisedTotalEmissionCostOverLifespan / annualisedOPEX), 2)
            self.emissionCostContributionList_OPEX.append(emissionCostContribution_OPEX)
        for totalAnnualisedCost in self.totalCostList:
            emissionCostContribution_TotalCost = round((annualisedTotalEmissionCostOverLifespan / totalAnnualisedCost), 2)
            self.emissionCostContributionList_TotalCost.append(emissionCostContribution_TotalCost)          
        return 

    def EnergySupplyBreakDownPieChartCreator(self, weighterList, CarbonTaxForOPF, weatherCondition, numberOfSMRToBeIntroduced):
        energyBreakdownList = []
        for w in range(len(self.GeneratorObjectList)): ## the length of GeneratorObjectList should equal to the number of the weights
            genListForEachWeight = self.GeneratorObjectList[w]
            genTypeLabel = []
            outPutData = []
            for gen in genListForEachWeight:
                if not self.ObjectSet[gen].fueltype in genTypeLabel:
                    genTypeLabel.append(self.ObjectSet[gen].fueltype)
                    outPutData.append(float(self.ObjectSet[gen].PG_OUTPUT))
                else:
                    i = genTypeLabel.index(self.ObjectSet[gen].fueltype) 
                    outPutData[i] += float(self.ObjectSet[gen].PG_OUTPUT)

            totalOutputOfSMR = 0
            if len(self.SMRSiteObjectList) > 0:
                SMRListForEachWeight = self.SMRSiteObjectList[w]
                for regen in SMRListForEachWeight:
                    totalOutputOfSMR += self.ObjectSet[regen].PG_OUTPUT
                genTypeLabel.append(self.newGeneratorType)
                outPutData.append(totalOutputOfSMR) 

            percentage = []
            sum_up = sum(outPutData)
            for output in outPutData:
                p = round(output/sum_up, 2)
                percentage.append(p * 100)

            print(genTypeLabel)
            print(percentage)
            print(outPutData)

            ## Old code for generating the pie chart
            otherCapa = 0
            labelToBeDeleted = []
            dataToBeDeleted = []
            for label in genTypeLabel:
                if label in ['SourGas']:
                    op = outPutData[genTypeLabel.index(label)]
                    outPutData[genTypeLabel.index('NaturalGas')] += op
                    labelToBeDeleted.append('SourGas')
                    dataToBeDeleted.append(op)
                elif not label in ['Solar', 'Oil', 'NaturalGas', 'Coal', 'Wind', 'Nuclear', 'SMR']:
                    i = genTypeLabel.index(label)
                    labelToBeDeleted.append(label)
                    dataToBeDeleted.append(outPutData[i])
                    otherCapa += outPutData[i] 

            for label in labelToBeDeleted:
                genTypeLabel.remove(label)
            
            for data in dataToBeDeleted:
                outPutData.remove(data)

            outPutData.append(otherCapa)
            genTypeLabel.append('Others')

            print(genTypeLabel)
            print(outPutData)

            plt.pie(outPutData, labels=genTypeLabel, autopct='%1.1f%%', startangle=90)
            plt.title('Energy Supply BreakDown')
            plt.axis('equal')
            plt.tight_layout()
            path = 'EnergyBreakdown_PieChart_' + str(numberOfSMRToBeIntroduced) + 'SMR_weight_' + str(round(weighterList[w],2)) + '_weather_' + str(weatherCondition[2]) + '_carbonTax_' + str(CarbonTaxForOPF) + '.pdf' 
            self.mkdirFig()
            plt.savefig(self.diagramPath + path, dpi = 1200, bbox_inches='tight')
            # plt.show()
            # plt.close()
            plt.clf()
            plt.cla()
            energyBreakdownList.append(outPutData)    
        return energyBreakdownList, genTypeLabel

    def visualisationFileCreator_ExtantGenerator(self, GeneratorObjectList, file_label):
        geojson_file = """
        {
            "type": "FeatureCollection",
            "features": ["""
        for extant_gen in GeneratorObjectList:
            if round(float(self.ObjectSet[extant_gen].PG_OUTPUT), 2) < 0.01:
                continue
            else: 
                feature = """{
                    "type": "Feature",
                    "properties": {
                    "Fuel Type": "%s",
                    "Capacity": "%s",
                    "Output": "%s",
                    "Carbon tax rate": "%s",
                    "Status": "%s",
                    "marker-color": "%s",
                    "marker-size": "medium",
                    "marker-symbol": "",
                    "IRI": "%s"
                    },
                    "geometry": {
                    "type": "Point",
                    "coordinates": [
                        %s,
                        %s
                    ]
                    }
                    },"""%(self.ObjectSet[extant_gen].fueltype, self.ObjectSet[extant_gen].capacity, round(float(self.ObjectSet[extant_gen].PG_OUTPUT), 2), self.ObjectSet[extant_gen].CarbonTax, 
                self.ObjectSet[extant_gen].status, gen_fuel_col(str(self.ObjectSet[extant_gen].fueltype)), self.ObjectSet[extant_gen].generatorNodeIRI, self.ObjectSet[extant_gen].latlon[1], self.ObjectSet[extant_gen].latlon[0])
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
        self.mkdirJSON()
        geojson_written = open(self.filePathForJSON + '\\' + file_label +'.geojson','w')
        geojson_written.write(geojson_file)
        geojson_written.close() 
        print('---GeoJSON written successfully: visualisationFileCreator_ExtantGenerator---', file_label)
        return

##FIXME: if add the decommission method, it might be a concreate list of the closed generators, for now it will used the same list of the GeneratorObjectList
    def visualisationFileCreator_ClosedGenerator(self, GeneratorObjectList, file_label):
        geojson_file = """
        {
            "type": "FeatureCollection",
            "features": ["""
        for extant_gen in GeneratorObjectList:
            if round(float(self.ObjectSet[extant_gen].PG_OUTPUT), 2) < 0.01:
                feature = """{
                    "type": "Feature",
                    "properties": {
                    "Fuel Type": "%s",
                    "Capacity": "%s",
                    "Output": "%s",
                    "Carbon tax rate": "%s",
                    "Status": "Closed",
                    "marker-color": "%s",
                    "marker-size": "medium",
                    "marker-symbol": "",
                    "IRI": "%s"
                    },
                    "geometry": {
                    "type": "Point",
                    "coordinates": [
                        %s,
                        %s
                    ]
                    }
                    },"""%(self.ObjectSet[extant_gen].fueltype, self.ObjectSet[extant_gen].capacity, round(float(self.ObjectSet[extant_gen].PG_OUTPUT), 2), self.ObjectSet[extant_gen].CarbonTax, 
                    gen_fuel_col(str(self.ObjectSet[extant_gen].fueltype)), self.ObjectSet[extant_gen].generatorNodeIRI, self.ObjectSet[extant_gen].latlon[1], self.ObjectSet[extant_gen].latlon[0])
                # adding new line 
                geojson_file += '\n'+feature
            else: 
                continue

        # removing last comma as is last line
        geojson_file = geojson_file[:-1]
        # finishing file end 
        end_geojson = """
            ]
        }
        """
        geojson_file += end_geojson
        # saving as geoJSON
        self.mkdirJSON()
        geojson_written = open(self.filePathForJSON + '\\' + file_label +'.geojson','w')
        geojson_written.write(geojson_file)
        geojson_written.close() 
        print('---GeoJSON written successfully: visualisationFileCreator_ClosedGenerator---', file_label)
        return

    def visualisationFileCreator_AddedSMRGenerator(self, SMRSiteObjectList, file_label):
        if len(SMRSiteObjectList) == 0:
            print("***There is no SMR to be retrofitted.***")
            return
        geojson_file = """
        {
            "type": "FeatureCollection",
            "features": ["""
        for smr in SMRSiteObjectList:
            feature = """{
                "type": "Feature",
                "properties": {
                "Fuel Type": "%s",
                "Capacity": "%s",
                "Output": "%s",
                "Operation ratio": "%s",
                "Number of SMR units": "%s",
                "Carbon tax rate": "%s",
                "Status": "%s",
                "IRI": "%s"
                },
                "geometry": {
                "type": "Point",
                "coordinates": [
                    %s,
                    %s
                ]
                }
            },"""%(self.ObjectSet[smr].fueltype, self.ObjectSet[smr].capacity, round(float(self.ObjectSet[smr].PG_OUTPUT),2), round(float(self.ObjectSet[smr].PG_OUTPUT)/float(self.ObjectSet[smr].capacity), 2), int(float(self.ObjectSet[smr].capacity)/self.SMRCapability), 
            self.ObjectSet[smr].CarbonTax, self.ObjectSet[smr].status, self.ObjectSet[smr].generatorNodeIRI,  self.ObjectSet[smr].latlon[1], self.ObjectSet[smr].latlon[0])
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
        self.mkdirJSON()
        geojson_written = open(self.filePathForJSON + '\\' + file_label +'.geojson','w')
        geojson_written.write(geojson_file)
        geojson_written.close() 
        print('---GeoJSON written successfully: visualisationFileCreator_AddedSMRGenerator---', file_label)
        return

## FIXME: modify this function according to the new decommission method 
    def visualisationFileCreator_decommissionedGenerator(self, file_label):
        geojson_file = """
        {
            "type": "FeatureCollection",
            "features": ["""
        for dcgen in self.decommissionedPowerPlant_generator:
            feature = """{
                "type": "Feature",
                "properties": {
                "Fuel Type": "%s",
                "Capacity": "%s",
                "Status": "Decommissioned",
                "marker-color": "#000000",
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
                },"""%(dcgen[7], dcgen[6], dcgen[8][1], dcgen[8][0])
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
        self.mkdirJSON()
        geojson_written = open(self.filePathForJSON + '\\' + file_label +'.geojson','w')
        geojson_written.write(geojson_file)
        geojson_written.close() 
        print('---GeoJSON written successfully: visualisationFileCreator_decommissionedGenerator---', file_label)
        return 

    def mkdirJSON(self):
        folder = os.path.exists(self.filePathForJSON)
        if not folder:                
            os.makedirs(self.filePathForJSON)           
            print("---  new folder %s...  ---" % self.filePathForJSON)
        else:
            print("---  There has this folder!  ---")

    def mkdirFig(self):
        folder = os.path.exists(self.diagramPath)
        if not folder:                
            os.makedirs(self.diagramPath)           
            print("---  new folder %s...  ---" % self.diagramPath)
        else:
            print("---  There has this folder!  ---")

    """Create the heatmap for total cost and CO2 emission"""
    ## TODO: remove the title
    def dataHeatmapCreator_totalCostAndEmission(self, dataMatrix, CarbonTaxForOPFList, NumberOfSMRUnitList, weatherConditionList, weighterList):
        rowNum = len(NumberOfSMRUnitList)
        colNum = len(CarbonTaxForOPFList)
        self.weightRecorder = []
        self.minTotalCostIndexRecoder = []

        ## colour pattern, "crest" was used at the first time
        cmap = seaborn.diverging_palette(200,20,sep=20,as_cmap=True)
        
        for k in range(len(weatherConditionList)):
            matrix_minTotalCost = numpy.zeros((rowNum, colNum), dtype = float)
            matrix_minCO2Emission = numpy.zeros((rowNum, colNum), dtype = float)
            matrix_weight = numpy.zeros((rowNum, colNum), dtype = float)
            matrix_minTotalCostIndex = numpy.zeros((rowNum, colNum), dtype = float)
            matrix_minTotalCostForAnnotation = numpy.zeros((rowNum, colNum), dtype = float)
            #row = 0
            for i in range(rowNum): ## SMR design index
                SMRdesign = dataMatrix[i]
                #col = 0
                for j in range(colNum): ## carbon tax index
                    results_sameCarbonTaxAndSameWeather = SMRdesign[j][k]
                    totalCost = results_sameCarbonTaxAndSameWeather[0]
                    minTotalCost = min(totalCost)
                    indexList = []
                    for i_tc in range(len(totalCost)):
                        tc = totalCost[i_tc]
                        if tc == minTotalCost:
                            indexList.append(i_tc)
                    index_minTotalCost = max(indexList) #totalCost.index(minTotalCost)
                    CO2EmissionOftheMinimumCost = results_sameCarbonTaxAndSameWeather[1][totalCost.index(minTotalCost)]
                    matrix_minTotalCost[i, j] = minTotalCost
                    matrix_minTotalCostForAnnotation[i, j] = float(minTotalCost)/1E10
                    matrix_minCO2Emission[i, j] = CO2EmissionOftheMinimumCost
                    matrix_weight[i, j] = round(weighterList[totalCost.index(minTotalCost)], 2)   
                    matrix_minTotalCostIndex[i, j] = int(index_minTotalCost)                 
                    
            self.weightRecorder.append(matrix_weight)
            self.minTotalCostIndexRecoder.append(matrix_minTotalCostIndex)
            
            ## Draw the heatmap of total cost
            seaborn.heatmap(matrix_minTotalCost, linewidth=0.004, cmap=cmap, annot=matrix_minTotalCostForAnnotation, fmt=".3f", square = False, xticklabels = CarbonTaxForOPFList, yticklabels = NumberOfSMRUnitList, center = 1.8E10, annot_kws={'size':7.5}, vmin=1E10, vmax=3.0E10)
            plt.title("Total cost at weather condition %s" % weatherConditionList[k][2])
            plt.xlabel("Carbon tax (£)", fontsize = labelFontSize)
            plt.ylabel("SMR Number", fontsize = labelFontSize) 
            plt.tight_layout()
            self.mkdirFig()
            plt.savefig(self.diagramPath + 'TotalCost_Heatmap_%s.pdf' % str(weatherConditionList[k][2]), dpi = 1200)
            # plt.show()
            # plt.close()
            plt.clf()
            plt.cla()

            ## Draw the heatmap of carbon emission
            seaborn.heatmap(matrix_minCO2Emission, linewidth=0.004, cmap=cmap, annot=True, fmt=".1f", square = False, xticklabels = CarbonTaxForOPFList, yticklabels = NumberOfSMRUnitList, center = 3000, annot_kws={'size':7}, vmin=0, vmax=6000)
            plt.title("Carbon emission at weather condition %s" % weatherConditionList[k][2])
            plt.xlabel("Carbon tax (£)", fontsize = labelFontSize)
            plt.ylabel("SMR Number", fontsize = labelFontSize) 
            plt.tight_layout()
            self.mkdirFig()
            plt.savefig(self.diagramPath + 'CarbonEmission_Heatmap_%s.pdf' % str(weatherConditionList[k][2]), dpi = 1200, bbox_inches='tight')
            ## plt.savefig('CarbonEmission_Heatmap_%s.svg' % str(weatherConditionList[k][2]))
            # plt.show()
            # plt.close()
            plt.clf()
            plt.cla()

            ## Draw the heatmap of weight
            seaborn.heatmap(matrix_weight, linewidth=0.004, cmap=cmap, annot=True, fmt=".2f", square = False, xticklabels = CarbonTaxForOPFList, yticklabels = NumberOfSMRUnitList, center = 0.5, annot_kws={'size':7.5}, vmin=0, vmax=1)
            plt.title("Picked weight at weather condition %s" % weatherConditionList[k][2])
            plt.xlabel("Carbon tax (£)", fontsize = labelFontSize)
            plt.ylabel("SMR Number", fontsize = labelFontSize) 
            plt.tight_layout()
            self.mkdirFig()
            plt.savefig(self.diagramPath + 'weight_Heatmap_%s.pdf' % str(weatherConditionList[k][2]), dpi = 1200, bbox_inches='tight')
            ## plt.savefig('weight_Heatmap_%s.svg' % str(weatherConditionList[k][2]))
            # plt.show()
            # plt.close()
            plt.clf()
            plt.cla()

        ## Draw the heatmap at each weight
        for k in range(len(weatherConditionList)):
            for m in range(len(weighterList)):
                matrix_totalCostAtEachWeight = numpy.zeros((rowNum, colNum), dtype = float)
                matrix_CO2EmissionAtEachWeight = numpy.zeros((rowNum, colNum), dtype = float)
                matrix_totalCostAtEachWeightForAnotation = numpy.zeros((rowNum, colNum), dtype = float)
                for i in range(rowNum): ## SMR design index
                    SMRdesign = dataMatrix[i]
                    for j in range(colNum): ## carbon tax index
                        results_sameCarbonTaxAndSameWeather = SMRdesign[j][k]
                        totalCost = results_sameCarbonTaxAndSameWeather[0]
                        co2Emission = results_sameCarbonTaxAndSameWeather[1]
                        matrix_totalCostAtEachWeight[i,j] = totalCost[m]
                        matrix_totalCostAtEachWeightForAnotation [i,j] = totalCost[m]/1E10
                        matrix_CO2EmissionAtEachWeight[i,j] = co2Emission[m]
 
                ## Draw the heatmap of total cost
                seaborn.heatmap(matrix_totalCostAtEachWeight, linewidth=0.004, cmap=cmap, annot=matrix_totalCostAtEachWeightForAnotation, fmt=".3f", square = False, xticklabels = CarbonTaxForOPFList, yticklabels = NumberOfSMRUnitList, center = 1.8E10, annot_kws={'size':7.5}, vmin=1E10, vmax=2.6E10)
                title = "Total cost at weather condition " + weatherConditionList[k][2] + " (weight = " + str(round(weighterList[m], 2)) + ")"
                plt.title(title)
                plt.xlabel("Carbon tax (£)", fontsize = labelFontSize)
                plt.ylabel("SMR Number", fontsize = labelFontSize) 
                plt.tight_layout()
                label_png = 'TotalCost_Heatmap_' + str(weatherConditionList[k][2]) + '_weight_' + str(round(weighterList[m], 2)) + '.pdf'
                ## label_svg = 'TotalCost_Heatmap_' + str(weatherConditionList[k][2]) + '_weight_' + str(round(weighterList[m], 2)) + '.svg'
                self.mkdirFig()
                plt.savefig(self.diagramPath + label_png, dpi = 1200, bbox_inches='tight')
                ## plt.savefig(label_svg)
                # plt.show()
                # plt.close()
                plt.clf()
                plt.cla()

                ## Draw the heatmap of carbon emission
                seaborn.heatmap(matrix_CO2EmissionAtEachWeight, linewidth=0.004, cmap=cmap, annot=True, fmt=".1f", square = False, xticklabels = CarbonTaxForOPFList, yticklabels = NumberOfSMRUnitList, center = 3000, annot_kws={'size':7}, vmin=0, vmax=6000)
                title = "Carbon emission at weather condition " + weatherConditionList[k][2] + " (weight = " + str(round(weighterList[m], 2)) + ")"
                plt.title(title)
                plt.xlabel("Carbon tax (£)", fontsize = labelFontSize)
                plt.ylabel("SMR Number", fontsize = labelFontSize) 
                plt.tight_layout()
                label_png = 'CarbonEmission_Heatmap_' + str(weatherConditionList[k][2]) + '_weight_' + str(round(weighterList[m], 2)) + '.pdf'
                ## label_svg = 'CarbonEmission_Heatmap_' + str(weatherConditionList[k][2]) + '_weight_' + str(round(weighterList[m], 2)) + '.svg'
                self.mkdirFig()
                plt.savefig(self.diagramPath + label_png, dpi = 1200, bbox_inches='tight')
                # plt.show()
                # plt.close()
                plt.clf()
                plt.cla()        
        return

    """Create the OPEXRatio"""
    ## TODO: remove the title
    def dataHeatmapCreator_OPEXRatio(self, dataMatrix, CarbonTaxForOPFList, NumberOfSMRUnitList, weatherConditionList, weighterList):
        rowNum = len(NumberOfSMRUnitList)
        colNum = len(CarbonTaxForOPFList)

        ## colour pattern, "crest" was used at the first time
        ## cmap = seaborn.cubehelix_palette(as_cmap=True, start=1.5, rot=-0.75, dark= 0.3)
        cmap = "crest"
        
        for k in range(len(weatherConditionList)):
            indexMatrix = self.minTotalCostIndexRecoder[k]
            matrix_OPEXRatio = numpy.zeros((rowNum, colNum), dtype = float)
           
            for i in range(rowNum): ## SMR design index
                SMRdesign = dataMatrix[i]
                indexRow = indexMatrix[i]
                for j in range(colNum): ## carbon tax index
                    OPEXRatio = round(SMRdesign[j][k][0][int(indexRow[j])], 2)
                    matrix_OPEXRatio[i, j] = OPEXRatio
                             
            ## Draw the heatmap of total cost
            seaborn.heatmap(matrix_OPEXRatio, linewidth=0.004, cmap=cmap, annot=matrix_OPEXRatio, fmt=".2f", square = False, xticklabels = CarbonTaxForOPFList, yticklabels = NumberOfSMRUnitList, center = 0.9, annot_kws={'size':7.5}, vmin=0.5, vmax=1)
            plt.title("OPEX contribution at weather condition %s" % weatherConditionList[k][2])
            plt.xlabel("Carbon tax (£)", fontsize = labelFontSize)
            plt.ylabel("SMR Number", fontsize = labelFontSize) 
            plt.tight_layout()
            self.mkdirFig()
            plt.savefig(self.diagramPath + 'OPEXRatio_Heatmap_%s.pdf' % str(weatherConditionList[k][2]), dpi = 1200, bbox_inches='tight')
            # plt.show()
            # plt.close()
            plt.clf()
            plt.cla()
        return
    
    """Create the SMR output and operational retio"""
    ## TODO: remove the title
    def dataHeatmapCreator_SMROutputAndOperationalRatio(self, dataMatrix, CarbonTaxForOPFList, NumberOfSMRUnitList, weatherConditionList, weighterList):
        rowNum = len(NumberOfSMRUnitList)
        colNum = len(CarbonTaxForOPFList)

        ## colour pattern, "crest" was used at the first time
        ## cmap = seaborn.cubehelix_palette(as_cmap=True, start=1.5, rot=-0.75, dark= 0.3)
        
        ## heat map for different weather conditions
        for k in range(len(weatherConditionList)):
            indexMatrix = self.minTotalCostIndexRecoder[k]
            matrix_SMROutput = numpy.zeros((rowNum, colNum), dtype = float)
            matrix_SMROperationalRatio = numpy.zeros((rowNum, colNum), dtype = float)
           
            for i in range(rowNum): ## SMR design index
                SMRdesign = dataMatrix[i]
                indexRow = indexMatrix[i]
                for j in range(colNum): ## carbon tax index
                    SMROutput = round(SMRdesign[j][k][0][int(indexRow[j])], 2)
                    SMROperationalRatio = round(SMRdesign[j][k][1][int(indexRow[j])], 2)
                    matrix_SMROutput[i, j] = SMROutput
                    matrix_SMROperationalRatio[i, j] = SMROperationalRatio
                             
            ## Draw the heatmap of SMR output
            cmap = seaborn.diverging_palette(200,20,sep=20,as_cmap=True)
            seaborn.heatmap(matrix_SMROutput, linewidth=0.004, cmap=cmap, annot=matrix_SMROutput, fmt=".2f", square = False, xticklabels = CarbonTaxForOPFList, yticklabels = NumberOfSMRUnitList, center = 0.9, annot_kws={'size':7}, vmin=0, vmax=2.6E4)
            plt.title("SMR total output at weather condition %s" % weatherConditionList[k][2])
            plt.xlabel("Carbon tax (£)", fontsize = labelFontSize)
            plt.ylabel("SMR Number", fontsize = labelFontSize) 
            plt.tight_layout()
            self.mkdirFig()
            plt.savefig(self.diagramPath + 'SMROutput_Heatmap_%s.pdf' % str(weatherConditionList[k][2]), dpi = 1200, bbox_inches='tight')
            # plt.show()
            # plt.close()
            plt.clf()
            plt.cla()

            ## Draw the heatmap of SMR operational ratio
            seaborn.heatmap(matrix_SMROperationalRatio, linewidth=0.004, cmap="crest", annot=matrix_SMROperationalRatio, fmt=".2f", square = False, xticklabels = CarbonTaxForOPFList, yticklabels = NumberOfSMRUnitList, center = 0.9, annot_kws={'size':7.5}, vmin=0, vmax=1)
            plt.title("SMR operational ratio at weather condition %s" % weatherConditionList[k][2])
            plt.xlabel("Carbon tax (£)", fontsize = labelFontSize)
            plt.ylabel("SMR Number", fontsize = labelFontSize) 
            plt.tight_layout()
            self.mkdirFig()
            plt.savefig(self.diagramPath + 'SMROperationalRatio_Heatmap_%s.pdf' % str(weatherConditionList[k][2]), dpi = 1200, bbox_inches='tight')
            # plt.show()
            # plt.close()
            plt.clf()
            plt.cla()

        ## Draw the heatmap at each weight
        for k in range(len(weatherConditionList)):
            for m in range(len(weighterList)):
                matrix_SMROutputAtEachWeight = numpy.zeros((rowNum, colNum), dtype = float)
                matrix_SMROperationalRatioAtEachWeight = numpy.zeros((rowNum, colNum), dtype = float)
                for i in range(rowNum): ## SMR design index
                    SMRdesign = dataMatrix[i]
                    for j in range(colNum): ## carbon tax index
                        results_sameCarbonTaxAndSameWeather = SMRdesign[j][k]
                        SMROutput = results_sameCarbonTaxAndSameWeather[0]
                        SMROperationalRatio = results_sameCarbonTaxAndSameWeather[1]
                        matrix_SMROutputAtEachWeight[i,j] = SMROutput[m]
                        matrix_SMROperationalRatioAtEachWeight [i,j] = SMROperationalRatio[m]
                        

            ## Draw the heatmap of SMR output
            cmap = seaborn.diverging_palette(200,20,sep=20,as_cmap=True)
            seaborn.heatmap(matrix_SMROutput, linewidth=0.004, cmap=cmap, annot=matrix_SMROutput, fmt=".2f", square = False, xticklabels = CarbonTaxForOPFList, yticklabels = NumberOfSMRUnitList, center = 0.9, annot_kws={'size':7}, vmin=0, vmax=2.6E4)
            title = "SMR total output at weather condition " + weatherConditionList[k][2] + " (weight = " + str(round(weighterList[m], 2)) + ")"
            plt.title(title)
            plt.xlabel("Carbon tax (£)", fontsize = labelFontSize)
            plt.ylabel("SMR Number", fontsize = labelFontSize) 
            plt.tight_layout()
            label_png = 'SMROutput_Heatmap_' + str(weatherConditionList[k][2]) + '_weight_' + str(round(weighterList[m], 2)) + '.pdf'
            ## label_svg = 'SMROutput_Heatmap_' + str(weatherConditionList[k][2]) + '_weight_' + str(round(weighterList[m], 2)) + '.svg'
            self.mkdirFig()
            plt.savefig(self.diagramPath + label_png, dpi = 1200, bbox_inches='tight')
            # plt.show()
            # plt.close()
            plt.clf()
            plt.cla()

            ## Draw the heatmap of SMR operational ratio
            seaborn.heatmap(matrix_SMROperationalRatio, linewidth=0.004, cmap="crest", annot=matrix_SMROperationalRatio, fmt=".2f", square = False, xticklabels = CarbonTaxForOPFList, yticklabels = NumberOfSMRUnitList, center = 0.9, annot_kws={'size':7.5}, vmin=0, vmax=1)
            title = "SMR operational ratio at weather condition " + weatherConditionList[k][2] + " (weight = " + str(round(weighterList[m], 2)) + ")"
            plt.title(title)
            plt.xlabel("Carbon tax (£)", fontsize = labelFontSize)
            plt.ylabel("SMR Number", fontsize = labelFontSize) 
            plt.tight_layout()
            label_png = 'SMROperationalRatio_Heatmap_' + str(weatherConditionList[k][2]) + '_weight_' + str(round(weighterList[m], 2)) + '.pdf'
            ## label_svg = 'SMROperationalRatio_Heatmap_' + str(weatherConditionList[k][2]) + '_weight_' + str(round(weighterList[m], 2)) + '.svg'
            self.mkdirFig()
            plt.savefig(self.diagramPath + label_png, dpi = 1200, bbox_inches='tight')
            # plt.show()
            # plt.close()
            plt.clf()
            plt.cla()
        return
    
    """This method is used to create emission heatmap"""
    ## TODO: remove the title
    def dataHeatmapCreator_CO2Emission(self, dataMatrix, CarbonTaxForOPFList, NumberOfSMRUnitList, weatherConditionList, weighterList):
        rowNum = len(NumberOfSMRUnitList)
        colNum = len(CarbonTaxForOPFList)

        ## colour pattern, "crest" was used at the first time
        ## cmap = seaborn.cubehelix_palette(as_cmap=True, start=1.5, rot=-0.75, dark= 0.3)
        
        ## heat map for different weather conditions: pick the beat solution regardless of the weight
        for k in range(len(weatherConditionList)):
            indexMatrix = self.minTotalCostIndexRecoder[k]
            matrix_EmissionCost = numpy.zeros((rowNum, colNum), dtype = float)
            matrix_EmissionOPEXRatio = numpy.zeros((rowNum, colNum), dtype = float)
            matrix_EmissionTotalCostRatio = numpy.zeros((rowNum, colNum), dtype = float)
            matrix_EmissionCost_Annotation = numpy.zeros((rowNum, colNum), dtype = float)
            for i in range(rowNum): ## SMR design index
                SMRdesign = dataMatrix[i]
                indexRow = indexMatrix[i]
                for j in range(colNum): ## carbon tax index
                    emissionCost = round(SMRdesign[j][k][0][int(indexRow[j])], 2)
                    emissionOPEXRatio = round(SMRdesign[j][k][1][int(indexRow[j])], 2)
                    emissionTotalCostRatio = round(SMRdesign[j][k][2][int(indexRow[j])], 2)
                    matrix_EmissionCost[i, j] = emissionCost
                    matrix_EmissionOPEXRatio[i, j] = emissionOPEXRatio
                    matrix_EmissionCost_Annotation[i, j] = emissionOPEXRatio / 1E8
                    matrix_EmissionTotalCostRatio[i, j] = emissionTotalCostRatio
                             
            ## Draw the heatmap of emission cost
            cmap = seaborn.diverging_palette(200,20,sep=20,as_cmap=True)
            seaborn.heatmap(matrix_EmissionCost, linewidth=0.004, cmap=cmap, annot = matrix_EmissionCost_Annotation, fmt=".2f", square = False, xticklabels = CarbonTaxForOPFList, yticklabels = NumberOfSMRUnitList, center = 0.9, annot_kws={'size':7}, vmin=0, vmax=5E8)
            plt.title("Emission cost at weather condition %s" % weatherConditionList[k][2])
            plt.xlabel("Carbon tax (£)",  fontsize = labelFontSize)
            plt.ylabel("SMR Number", fontsize = labelFontSize) 
            plt.tight_layout()
            self.mkdirFig()
            plt.savefig(self.diagramPath + 'emissionCost_Heatmap_%s.pdf' % str(weatherConditionList[k][2]), dpi = 1200, bbox_inches='tight')
            # plt.show()
            # plt.close()
            plt.clf()
            plt.cla()

            ## Draw the heatmap of emission OPEX ratio
            seaborn.heatmap(matrix_EmissionOPEXRatio, linewidth=0.004, cmap="crest", annot=matrix_EmissionOPEXRatio, fmt=".2f", square = False, xticklabels = CarbonTaxForOPFList, yticklabels = NumberOfSMRUnitList, center = 0.9, annot_kws={'size':7.5}, vmin=0, vmax=1)
            plt.title("Emission cost and OPEX ratio at weather condition %s" % weatherConditionList[k][2], fontsize = 11)
            plt.xlabel("Carbon tax (£)", fontsize = labelFontSize)
            plt.ylabel("SMR Number", fontsize = labelFontSize) 
            plt.tight_layout()
            self.mkdirFig()
            plt.savefig(self.diagramPath + 'emissionCostOPEXRatio_Heatmap_%s.pdf' % str(weatherConditionList[k][2]), dpi = 1200, bbox_inches='tight')
            # plt.show()
            # plt.close()
            plt.clf()
            plt.cla()

            ## Draw the heatmap of emission/TotalCost ratio
            seaborn.heatmap(matrix_EmissionTotalCostRatio, linewidth=0.004, cmap="crest", annot=matrix_EmissionTotalCostRatio, fmt=".2f", square = False, xticklabels = CarbonTaxForOPFList, yticklabels = NumberOfSMRUnitList, center = 0.9, annot_kws={'size':7.5}, vmin=0, vmax=1)
            plt.title("Emission cost and Total Cost ratio at weather condition %s" % weatherConditionList[k][2], fontsize = 10)
            plt.xlabel("Carbon tax (£)", fontsize = labelFontSize)
            plt.ylabel("SMR Number", fontsize = labelFontSize) 
            plt.tight_layout()
            self.mkdirFig()
            plt.savefig(self.diagramPath + 'emissionCostTotalCostRatio_Heatmap_%s.pdf' % str(weatherConditionList[k][2]), dpi = 1200, bbox_inches='tight')
            # plt.show()
            # plt.close()
            plt.clf()
            plt.cla()

        ## Draw the heatmap at each weight
        for k in range(len(weatherConditionList)):
            for m in range(len(weighterList)):
                matrix_emissionCostAtEachWeight = numpy.zeros((rowNum, colNum), dtype = float)
                matrix_emissionCostAtEachWeight_Annotation = numpy.zeros((rowNum, colNum), dtype = float)
                matrix_emissionCostOPEXRatioAtEachWeight = numpy.zeros((rowNum, colNum), dtype = float)
                matrix_emissionCostTotalCostRatioAtEachWeight = numpy.zeros((rowNum, colNum), dtype = float)
                for i in range(rowNum): ## SMR design index
                    SMRdesign = dataMatrix[i]
                    for j in range(colNum): ## carbon tax index
                        results_sameCarbonTaxAndSameWeather = SMRdesign[j][k]
                        emissionCost = results_sameCarbonTaxAndSameWeather[0]
                        emissionCostOPEXRatio = results_sameCarbonTaxAndSameWeather[1]
                        emissionCostTotalCostRatio = results_sameCarbonTaxAndSameWeather[2]
                        matrix_emissionCostAtEachWeight[i,j] = emissionCost[m]
                        matrix_emissionCostAtEachWeight_Annotation[i,j] = emissionCost[m] / 1E8
                        matrix_emissionCostOPEXRatioAtEachWeight[i,j] = emissionCostOPEXRatio[m]
                        matrix_emissionCostTotalCostRatioAtEachWeight[i,j] = emissionCostTotalCostRatio[m]
                        
            ## Draw the heatmap of emission cost
            cmap = seaborn.diverging_palette(200,20,sep=20,as_cmap=True)
            seaborn.heatmap(matrix_emissionCostAtEachWeight, linewidth=0.004, cmap=cmap, annot=matrix_emissionCostAtEachWeight_Annotation, fmt=".2f", square = False, xticklabels = CarbonTaxForOPFList, yticklabels = NumberOfSMRUnitList, center = 0.9, annot_kws={'size':7}, vmin=0, vmax=5E8)
            title = "Emission cost at weather condition " + weatherConditionList[k][2] + " (weight = " + str(round(weighterList[m], 2)) + ")"
            plt.title(title)
            plt.xlabel("Carbon tax (£)", fontsize = labelFontSize)
            plt.ylabel("SMR Number", fontsize = labelFontSize) 
            plt.tight_layout()
            label_png = 'EmissionCost_Heatmap_' + str(weatherConditionList[k][2]) + '_weight_' + str(round(weighterList[m], 2)) + '.pdf'
            ## label_svg = 'SMROutput_Heatmap_' + str(weatherConditionList[k][2]) + '_weight_' + str(round(weighterList[m], 2)) + '.svg'
            self.mkdirFig()
            plt.savefig(self.diagramPath + label_png, dpi = 1200, bbox_inches='tight')
            # plt.show()
            # plt.close()
            plt.clf()
            plt.cla()

            ## Draw the heatmap of emission cost and OPEX ratio
            seaborn.heatmap(matrix_emissionCostOPEXRatioAtEachWeight, linewidth=0.004, cmap="crest", annot = matrix_emissionCostOPEXRatioAtEachWeight, fmt=".2f", square = False, xticklabels = CarbonTaxForOPFList, yticklabels = NumberOfSMRUnitList, center = 0.9, annot_kws={'size':7.5}, vmin=0, vmax=1)
            title = "Emission cost and OPEX ratio at weather condition " + weatherConditionList[k][2] + " (weight = " + str(round(weighterList[m], 2)) + ")"
            plt.title(title, fontsize = 11)
            plt.xlabel("Carbon tax (£)", fontsize = labelFontSize)
            plt.ylabel("SMR Number", fontsize = labelFontSize) 
            plt.tight_layout()
            label_png = 'emissionCostOPEXRatio_Heatmap_' + str(weatherConditionList[k][2]) + '_weight_' + str(round(weighterList[m], 2)) + '.pdf'
            ## label_svg = 'SMROperationalRatio_Heatmap_' + str(weatherConditionList[k][2]) + '_weight_' + str(round(weighterList[m], 2)) + '.svg'
            self.mkdirFig()
            plt.savefig(self.diagramPath + label_png, dpi = 1200, bbox_inches='tight')
            # plt.show()
            # plt.close()
            plt.clf()
            plt.cla()

            ## Draw the heatmap of emission cost and total cost ratio
            seaborn.heatmap(matrix_emissionCostTotalCostRatioAtEachWeight, linewidth=0.004, cmap="crest", annot = matrix_emissionCostTotalCostRatioAtEachWeight, fmt=".2f", square = False, xticklabels = CarbonTaxForOPFList, yticklabels = NumberOfSMRUnitList, center = 0.9, annot_kws={'size':7.5}, vmin=0, vmax=1)
            title = "Emission cost and total cost ratio at weather condition " + weatherConditionList[k][2] + " (weight = " + str(round(weighterList[m], 2)) + ")"
            plt.title(title,fontsize = 10)
            plt.xlabel("Carbon tax (£)", fontsize = labelFontSize)
            plt.ylabel("SMR Number", fontsize = labelFontSize) 
            plt.tight_layout()
            label_png = 'emissionCostTotalCostRatio_Heatmap_' + str(weatherConditionList[k][2]) + '_weight_' + str(round(weighterList[m], 2)) + '.pdf'
            ## label_svg = 'SMROperationalRatio_Heatmap_' + str(weatherConditionList[k][2]) + '_weight_' + str(round(weighterList[m], 2)) + '.svg'
            self.mkdirFig()
            plt.savefig(self.diagramPath + label_png, dpi = 1200, bbox_inches='tight')
            # plt.show()
            # plt.close()
            plt.clf()
            plt.cla()
        return
        

    """Develope the data matrix"""
    def resultsSheetCreator(self, weighterList, NumberOfSMRUnitList, weatherConditionList, CarbonTaxForOPFList, dataMatrix, fileName:str = None):
        rowNum = len(NumberOfSMRUnitList) * len(weighterList) + 3 
        colNum = 2 * len(weatherConditionList) * len(CarbonTaxForOPFList) + 2
        resultSheet = numpy.zeros((rowNum, colNum), dtype = object)

        ## Header line row[0]
        resultSheet[0,0] = 'Design'
        resultSheet[0,1] = 'Weight'

        i = 0
        l = 2 * len(weatherConditionList) * len(CarbonTaxForOPFList)
        while i < l:
            carbonTax_i = i // (2 * len(weatherConditionList))
            carbonTax = str(CarbonTaxForOPFList[carbonTax_i])
            resultSheet[0, i + 2] = carbonTax
            i += 1

        ## Second line row[1]
        resultSheet[1,0] = '----'
        resultSheet[1,1] = '----'

        i = 0
        while i < l:
            weather_i = (i % (2 * len(weatherConditionList))) // 2
            weather = str(weatherConditionList[weather_i][2])
            resultSheet[1, i + 2] = weather
            i += 1

        ## Third line row[2]
        resultSheet[2,0] = '----'
        resultSheet[2,1] = '----'

        i = 0
        while i < l:
            costName_i = i % 2 
            if costName_i == 0:
                costName = 'Total_Cost'
            else:
                costName = 'CO2_Emission'
            resultSheet[2, i + 2] = costName
            i += 1

        ## Fill the first two rows
        row_i = 3 ## the data row starts from row 3
        while row_i < rowNum:
            smr_i = (row_i - 3) // len(weighterList)
            smrNum = str(NumberOfSMRUnitList[smr_i]) + '_SMR'
            resultSheet[row_i, 0] = smrNum

            weight_i = (row_i - 3) % len(weighterList)
            weight = str(weighterList[weight_i])
            resultSheet[row_i, 1] = weight
            row_i += 1

        ## Data sheet starts form row 3 and col 2
        row_i = 3 ## the data row starts from row 3
        col_i = 2
        for i in range(len(NumberOfSMRUnitList)): 
            SMRdesign = dataMatrix[i]
            for j in range(len(CarbonTaxForOPFList)):
                results_sameCarbonTax = SMRdesign[j]
                for k in range(len(weatherConditionList)):
                    results_sameWeather = results_sameCarbonTax[k]
                    totalCost = results_sameWeather[0]
                    CO2Emission = results_sameWeather[1]
                    for m in range(len(weighterList)):
                        resultSheet[row_i, col_i] = totalCost[m]
                        resultSheet[row_i, col_i + 1] = CO2Emission[m]
                        row_i += 1
                    row_i -= len(weighterList)
                    col_i += 2
            row_i += len(weighterList)
            col_i = 2
        
        ## File name 
        if fileName is None:
            fileName = 'SMR_Design_'
            for smr in NumberOfSMRUnitList:
                fileName += str(smr) + '_'
            fileName = fileName[:-1]   
            fileName += '.xlsx'

        # with open(fileName, 'w') as f:
        #     write = csv.writer(f)
        #     write.writerow(resultSheet)

        resultSheet = pd.DataFrame(resultSheet)
        writer = pd.ExcelWriter(fileName)
        resultSheet.to_excel(writer, sheet_name='Results', float_format = '%.5f', index=False)
        writer.close()
        print("============The Results Sheet is created====================")
        return

    """Create the multiple lines diagram reflecting the weather condition impact under the same weight 0.5"""
    def lineGraph_weatherImpact(self, pickedWeight, weighterList, summary_eachSMRDesign, NumberOfSMRUnitList, CarbonTaxForOPFList, weatherConditionList):     
        ## will generate 3 figs and each fig has 8 lines (SMR diagram only has 4 lines and the rest: 4 lines for 4 weather condition and 4 line for their corresponding base cases.)
        if not pickedWeight in weighterList:
            raiseExceptions("The given weight is not in the list!")
        else:
            weight_index = weighterList.index(pickedWeight)

        if len(summary_eachSMRDesign) != len(NumberOfSMRUnitList):
            raiseExceptions("The length of the result list should equal to the number of the SMR list number!")

        bestCostResult = []
        bestSMRNumberResult = []
        bestCO2EmissionResult = []

        baseCostResult = []
        baseCO2EmissionResult = []

        for w in range(len(weatherConditionList)):
            bestCost_UnderSameWeatherCondition = []
            bestSMRNumber_UnderSameWeatherCondition = []
            bestCO2Emission_UnderSameWeatherCondition = []

            baseCost_UnderSameWeatherCondition = []
            baseCO2Emission_UnderSameWeatherCondition = []

            for c in range(len(CarbonTaxForOPFList)):
                costResult_sameCarbonTaxDifferentSMRNumber = []
                CO2EmissionResult_sameCarbonTaxDifferentSMRNumber = []
                for result_sameSMR in summary_eachSMRDesign:
                    cost = result_sameSMR[c][w][0][weight_index]
                    CO2Emission = result_sameSMR[c][w][1][weight_index]
                    costResult_sameCarbonTaxDifferentSMRNumber.append(cost)
                    CO2EmissionResult_sameCarbonTaxDifferentSMRNumber.append(CO2Emission)
                
                bestCost_underSameCarbonTax = min(costResult_sameCarbonTaxDifferentSMRNumber)
                index_ = costResult_sameCarbonTaxDifferentSMRNumber.index(bestCost_underSameCarbonTax)
                bestNumberOfSMR = NumberOfSMRUnitList[index_]
                bestCO2Emission_underSameCarbonTax = CO2EmissionResult_sameCarbonTaxDifferentSMRNumber[index_]
                
                ## Best solution for the same weather 
                bestCost_UnderSameWeatherCondition.append(bestCost_underSameCarbonTax)
                bestSMRNumber_UnderSameWeatherCondition.append(bestNumberOfSMR)
                bestCO2Emission_UnderSameWeatherCondition.append(bestCO2Emission_underSameCarbonTax)

                ## Base case (0 SMR)
                baseCost_UnderSameWeatherCondition.append(costResult_sameCarbonTaxDifferentSMRNumber[0])
                baseCO2Emission_UnderSameWeatherCondition.append(CO2EmissionResult_sameCarbonTaxDifferentSMRNumber[0])

            bestCostResult.append(bestCost_UnderSameWeatherCondition)
            bestSMRNumberResult.append(bestSMRNumber_UnderSameWeatherCondition)
            bestCO2EmissionResult.append(bestCO2Emission_UnderSameWeatherCondition)

            baseCostResult.append(baseCost_UnderSameWeatherCondition)
            baseCO2EmissionResult.append(baseCO2Emission_UnderSameWeatherCondition)

        ## set up the clourmap
        cmap = mpl.cm.get_cmap("viridis", len(weatherConditionList))
        colors = cmap(numpy.linspace(0, 1, len(weatherConditionList)))
                
        ##-- Plot the multiple line chart of the SMR number vs carbon tax --##
        fig, ax1 = plt.subplots()
        ax2 = ax1.twinx()
        ax1.set_xlabel("Carbon Tax (£/t)", fontsize = labelFontSize)
        ax1.set_ylabel("Nunmber of SMR", fontsize = labelFontSize)
        ax2.set_ylabel("Nunmber of SMR", fontsize = labelFontSize)

        for i in range(len(bestSMRNumberResult)):
            ax1.plot(CarbonTaxForOPFList, bestSMRNumberResult[i], marker = markersList[i], label = weatherConditionList[i][2], color = colors[i])
            ax2.plot(CarbonTaxForOPFList, bestSMRNumberResult[i], color = colors[i], alpha=0)
        pos = ax1.get_position()
        ax1.set_position([pos.x0, pos.y0, pos.width, pos.height * 0.85])
        ax1.legend(
            loc="upper center",
            ## fontsize = legendFontSize,
            ncol=4,
            bbox_to_anchor=(0.5, 1.15),
            ## bbox_transform=fig.transFigure 
        )
        ## plt.tight_layout()
        self.mkdirFig()
        plt.savefig(self.diagramPath + 'SMRvsCarbonTax_(weatherImpact)__weight_%s.pdf' % str(pickedWeight), dpi = 1200, bbox_inches='tight')
        # plt.show() ## show must come after the savefig
        # plt.close()
        plt.clf()
        plt.cla()

        ##-- Plot the multiple line chart of the total cost vs carbon tax --##
        # plt.title("Total Cost vs. Carbon Tax")
        fig, ax1 = plt.subplots()
        ax2 = ax1.twinx()
        ax1.set_xlabel("Carbon Tax (£/t)", fontsize = labelFontSize)
        ax1.set_ylabel("Annualised Total Cost (£)", fontsize = labelFontSize)
        ax2.set_ylabel("Annualised Total Cost (£)", fontsize = labelFontSize)
        
        for i in range(len(bestCostResult)):
            ax1.plot(CarbonTaxForOPFList, bestCostResult[i], marker = markersList[i], label = weatherConditionList[i][2], color = colors[i], linestyle = lineStyleList[i])
            ax2.plot(CarbonTaxForOPFList, bestCostResult[i], color = colors[i], alpha=0)
            ax1.plot(CarbonTaxForOPFList, baseCostResult[i], label = weatherConditionList[i][2] + '(base case)', color = colors[i], linestyle = lineStyleList[i], alpha=0.6)
            ax2.plot(CarbonTaxForOPFList, baseCostResult[i], color = colors[i], alpha=0)

            for x, y, label in zip(CarbonTaxForOPFList, bestCostResult[i], bestSMRNumberResult[i]):
                ax1.text(x, y, label, fontsize = dotLabel, alpha = 0.85)   
        
        pos = ax1.get_position()
        ax1.set_position([pos.x0, pos.y0, pos.width, pos.height * 0.85])
        ax1.legend(
            loc="upper center",
            ## fontsize = legendFontSize,
            ncol=3,
            bbox_to_anchor=(0.5, 1.25),
            ## bbox_transform=fig.transFigure 
        )

        plt.tight_layout()
        self.mkdirFig()
        plt.savefig(self.diagramPath + 'CostvsCarbonTax_(weatherImpact)__weight_%s.pdf' % str(pickedWeight), dpi = 1200, bbox_inches='tight')
        # plt.show() ## show must come after the savefig
        # plt.close()
        plt.clf()
        plt.cla()

        ##-- Plot the multiple line chart of the CO2 emission vs carbon tax --##
        fig, ax1 = plt.subplots()
        ax2 = ax1.twinx()
        ax1.set_xlabel("Carbon Tax (£/t)", fontsize = labelFontSize)
        ax1.set_ylabel("Annualised ${CO_2}$ emission (t)", fontsize = labelFontSize)
        ax2.set_ylabel("Annualised ${CO_2}$ emission (t)", fontsize = labelFontSize)

        for i in range(len(bestCO2EmissionResult)):
            ax1.plot(CarbonTaxForOPFList, bestCO2EmissionResult[i], marker = markersList[i], label = weatherConditionList[i][2], color = colors[i], linestyle = lineStyleList[i])
            ax2.plot(CarbonTaxForOPFList, bestCO2EmissionResult[i], color = colors[i], alpha=0)
            ax1.plot(CarbonTaxForOPFList, baseCO2EmissionResult[i], label = weatherConditionList[i][2] + '(base case)', color = colors[i], linestyle = lineStyleList[i], alpha=0.6)
            ax2.plot(CarbonTaxForOPFList, baseCO2EmissionResult[i], alpha=0)

            for x, y, label in zip(CarbonTaxForOPFList, bestCO2EmissionResult[i], bestSMRNumberResult[i]):
                ax1.text(x, y, label, fontsize = dotLabel, alpha = 0.85)     
        
        pos = ax1.get_position()
        ax1.set_position([pos.x0, pos.y0, pos.width, pos.height * 0.85])
        ax1.legend(
            loc="upper center",
            ## fontsize = legendFontSize,
            ncol=3,
            bbox_to_anchor=(0.5, 1.25),
            ## bbox_transform=fig.transFigure 
        )

        plt.tight_layout()
        self.mkdirFig()
        plt.savefig(self.diagramPath + 'CO2EmissionvsCarbonTax_(weatherImpact)_weight_%s.pdf' % str(pickedWeight), dpi = 1200, bbox_inches='tight')
        # plt.show() ## show must come after the savefig
        # plt.close()
        plt.clf()
        plt.cla()
        return

    """Create the multiple lines diagram reflecting the weight impact under different weather conditions"""
    def lineGraph_weightImpact(self, weighterList, summary_eachSMRDesign, NumberOfSMRUnitList, CarbonTaxForOPFList, weatherConditionList):
        ## 3 (smr, cost, co2) * 4 (weather condotions) figs in total: each fig has 6 lines (1 base line 5 lines for different weight)    
        if len(summary_eachSMRDesign) != len(NumberOfSMRUnitList):
            raiseExceptions("The length of the result list should equal to the number of the SMR list number!")

        ## set up the clourmap
        cmap = mpl.cm.get_cmap("viridis", len(weighterList))
        colors = cmap(numpy.linspace(0, 1, len(weighterList)))

        for w in range(len(weatherConditionList)):
            bestCost_UnderSameWeatherCondition = []
            bestSMRNumber_UnderSameWeatherCondition = []
            bestCO2Emission_UnderSameWeatherCondition = []

            baseCost_UnderSameWeatherCondition = []
            baseCO2Emission_UnderSameWeatherCondition = []

            for weight_index in range(len(weighterList)):
                bestCost_UnderSameWeatherAndSameWeight = []
                bestSMRNumber_UnderSameWeatherAndSameWeight = []
                bestCO2Emission_UnderSameWeatherAndSameWeight = []

                baseCost_UnderSameWeatherAndSameWeight = []
                baseCO2Emission_UnderSameWeatherAndSameWeight = [] 
                
                for c in range(len(CarbonTaxForOPFList)):
                    costResult_sameCarbonTaxDifferentSMRNumber = []
                    CO2EmissionResult_sameCarbonTaxDifferentSMRNumber = []
                    for result_sameSMR in summary_eachSMRDesign:
                        cost = result_sameSMR[c][w][0][weight_index]
                        CO2Emission = result_sameSMR[c][w][1][weight_index]
                        costResult_sameCarbonTaxDifferentSMRNumber.append(cost)
                        CO2EmissionResult_sameCarbonTaxDifferentSMRNumber.append(CO2Emission)

                    ## pick the minimum 
                    bestCost_underSameCarbonTax = min(costResult_sameCarbonTaxDifferentSMRNumber)
                    index_ = costResult_sameCarbonTaxDifferentSMRNumber.index(bestCost_underSameCarbonTax)
                    bestNumberOfSMR = NumberOfSMRUnitList[index_]
                    bestCO2Emission_underSameCarbonTax = CO2EmissionResult_sameCarbonTaxDifferentSMRNumber[index_]
                    
                    bestCost_UnderSameWeatherAndSameWeight.append(bestCost_underSameCarbonTax)
                    bestSMRNumber_UnderSameWeatherAndSameWeight.append(bestNumberOfSMR)
                    bestCO2Emission_UnderSameWeatherAndSameWeight.append(bestCO2Emission_underSameCarbonTax)

                    ## Pick the base case
                    baseCost_UnderSameWeatherAndSameWeight.append(costResult_sameCarbonTaxDifferentSMRNumber[0])
                    baseCO2Emission_UnderSameWeatherAndSameWeight.append(CO2EmissionResult_sameCarbonTaxDifferentSMRNumber[0])

                ## Optima under the same weight
                bestCost_UnderSameWeatherCondition.append(bestCost_UnderSameWeatherAndSameWeight)
                bestSMRNumber_UnderSameWeatherCondition.append(bestSMRNumber_UnderSameWeatherAndSameWeight)
                bestCO2Emission_UnderSameWeatherCondition.append(bestCO2Emission_UnderSameWeatherAndSameWeight)

                ## Base case under the same weight
                baseCost_UnderSameWeatherCondition.append(baseCost_UnderSameWeatherAndSameWeight)
                baseCO2Emission_UnderSameWeatherCondition.append(baseCO2Emission_UnderSameWeatherAndSameWeight)
      
            ##-- Plot the multiple line chart of the SMR number vs carbon tax of each weight under the same weather condition --##
            fig, ax1 = plt.subplots()
            ax2 = ax1.twinx()
            ax1.set_xlabel("Carbon Tax (£/t)", fontsize = labelFontSize)
            ax1.set_ylabel("Nunmber of SMR", fontsize = labelFontSize)
            ax2.set_ylabel("Nunmber of SMR", fontsize = labelFontSize)
       
            for i in range(len(bestSMRNumber_UnderSameWeatherCondition)):
                weightLabel = 'weight: ' + str(round(weighterList[i], 2))
                ax1.plot(CarbonTaxForOPFList, bestSMRNumber_UnderSameWeatherCondition[i], marker = markersList[i], label = weightLabel, color = colors[i])
                ax2.plot(CarbonTaxForOPFList, bestSMRNumber_UnderSameWeatherCondition[i], color = colors[i], alpha=0)

            pos = ax1.get_position()
            ax1.set_position([pos.x0, pos.y0, pos.width, pos.height * 0.85])
            ax1.legend(
                loc="upper center",
                ## fontsize = legendFontSize,
                ncol=3,
                bbox_to_anchor=(0.5, 1.25),
                ## bbox_transform=fig.transFigure 
            )
            plt.tight_layout()
            self.mkdirFig() ##TODO: why there is no other weather condition?
            plt.savefig(self.diagramPath + 'SMRvsCarbonTax_(weightImpact)_weather_%s.pdf' % (weatherConditionList[w][2]), dpi = 1200, bbox_inches='tight')
            # plt.show() ## show must come after the savefig
            # plt.close()
            plt.clf()
            plt.cla()
            
            ##-- Plot the multiple line chart of the Annualised Total Cost vs carbon tax of each weight under the same weather condition --##
            fig, ax1 = plt.subplots()
            ax2 = ax1.twinx()
            ax1.set_xlabel("Carbon Tax (£/t)", fontsize = labelFontSize)
            ax1.set_ylabel("Annualised Total Cost (£)", fontsize = labelFontSize)
            ax2.set_ylabel("Annualised Total Cost (£)", fontsize = labelFontSize)

            ## base case for the current weather condition, the base case is not changed with the weight
            ax1.plot(CarbonTaxForOPFList, baseCost_UnderSameWeatherCondition[0], label = 'Base case', color = 'red', linestyle = lineStyleList[1], alpha=0.6, linewidth = 1.7)
            ax2.plot(CarbonTaxForOPFList, baseCost_UnderSameWeatherCondition[0], alpha=0)

            for i in range(len(bestCost_UnderSameWeatherCondition)):
                weightLabel = 'weight: ' + str(round(weighterList[i], 2))
                ax1.plot(CarbonTaxForOPFList, bestCost_UnderSameWeatherCondition[i], marker = markersList[i], label = weightLabel, color = colors[i], linestyle = lineStyleList[i])
                ax2.plot(CarbonTaxForOPFList, bestCost_UnderSameWeatherCondition[i], color = colors[i], alpha=0)        
                for x, y, label in zip(CarbonTaxForOPFList, bestCost_UnderSameWeatherCondition[i], bestSMRNumber_UnderSameWeatherCondition[i]):
                    ax1.text(x, y, label, fontsize = dotLabel, alpha = 0.85)
            
            pos = ax1.get_position()
            ax1.set_position([pos.x0, pos.y0, pos.width, pos.height * 0.85])
            ax1.legend(
                loc="upper center",
                ## fontsize = legendFontSize,
                ncol=3,
                bbox_to_anchor=(0.5, 1.25),
                ## bbox_transform=fig.transFigure 
            )
            plt.tight_layout()
            self.mkdirFig()
            plt.savefig(self.diagramPath + 'CostvsCarbonTax_(weightImpact)_weather_%s.pdf' % (weatherConditionList[w][2]), dpi = 1200, bbox_inches='tight')
            # plt.show() ## show must come after the savefig
            # plt.close()
            plt.clf()
            plt.cla()

            ##-- Plot the multiple line chart of the CO2 emission vs carbon tax --##
            fig, ax1 = plt.subplots()
            ax2 = ax1.twinx()
            ax1.set_xlabel("Carbon Tax (£/t)", fontsize = labelFontSize)
            ax1.set_ylabel("Annualised ${CO_2}$ emission (t)", fontsize = labelFontSize)
            ax2.set_ylabel("Annualised ${CO_2}$ emission (t)", fontsize = labelFontSize)

            ax1.plot(CarbonTaxForOPFList, baseCO2Emission_UnderSameWeatherCondition[0], label = 'Base case', color = 'red', linestyle = lineStyleList[1], alpha=0.6, linewidth = 1.7)
            ax2.plot(CarbonTaxForOPFList, baseCO2Emission_UnderSameWeatherCondition[0], alpha=0)

            for i in range(len(bestCO2Emission_UnderSameWeatherCondition)):
                weightLabel = 'weight: ' + str(round(weighterList[i], 2))
                ax1.plot(CarbonTaxForOPFList, bestCO2Emission_UnderSameWeatherCondition[i], marker = markersList[i], label = weightLabel, color = colors[i], linestyle = lineStyleList[i])
                ax2.plot(CarbonTaxForOPFList, bestCO2Emission_UnderSameWeatherCondition[i], color = colors[i], alpha=0)        
                for x, y, label in zip(CarbonTaxForOPFList, bestCO2Emission_UnderSameWeatherCondition[i], bestSMRNumber_UnderSameWeatherCondition[i]):
                    ax1.text(x, y, label, fontsize = dotLabel, alpha = 0.85)
        
            pos = ax1.get_position()
            ax1.set_position([pos.x0, pos.y0, pos.width, pos.height * 0.85])
            ax1.legend(
                loc="upper center",
                ## fontsize = legendFontSize,
                ncol=3,
                bbox_to_anchor=(0.5, 1.25),
                ## bbox_transform=fig.transFigure 
            )
            plt.tight_layout()
            self.mkdirFig()
            plt.savefig(self.diagramPath + 'CO2EmissionvsCarbonTax_(weightImpact)_weather_%s.pdf' % (weatherConditionList[w][2]), dpi = 1200, bbox_inches='tight')
            # plt.show() ## show must come after the savefig
            # plt.close()
            plt.clf()
            plt.cla()
        return

    """Multiple line chart demonstrate the carbon emission reduction due to both the carbon tax and SMR introduction under the same weather"""
    def lineGraph_SMRImpactForCO2Emission(self, weighterList:list, summary_eachSMRDesign:list, NumberOfSMRUnitList:list, CarbonTaxForOPFList:list, weatherConditionList:list):    
        if len(summary_eachSMRDesign) != len(NumberOfSMRUnitList):
            raiseExceptions("The length of the result list should equal to the number of the SMR list number!")
        
        for w in range(len(weatherConditionList)):
            for weight_index in range(len(weighterList)):
                CO2Emission_sameWeatherSameWeight = []
                zeroFlag = False
                for sei, result_sameSMR in enumerate(summary_eachSMRDesign):
                    CO2Emission_sameSMRNumber = []
                    for c in range(len(CarbonTaxForOPFList)):
                        CO2Emission = result_sameSMR[c][w][1][weight_index]
                        CO2Emission_sameSMRNumber.append(CO2Emission)
                    if round(CO2Emission_sameSMRNumber[0], 0) == 0 and zeroFlag is False:
                        zeroFlag = True
                    elif round(CO2Emission_sameSMRNumber[0], 0) == 0 and zeroFlag is True:
                        smrNumber = NumberOfSMRUnitList[sei]
                        print('The saturated number of the SMR for the weather condition', weatherConditionList[w][2], 'is', str(smrNumber))
                        break
                    CO2Emission_sameWeatherSameWeight.append(CO2Emission_sameSMRNumber)
                ## set up the clourmap
                cmap = mpl.cm.get_cmap("viridis", len(CO2Emission_sameWeatherSameWeight))
                colors = cmap(numpy.linspace(0, 1, len(CO2Emission_sameWeatherSameWeight)))
                ##-- Plot the multiple line chart of the CO2 emission vs carbon tax of different SMR numbers at differnt Weight and different weather condition --##
                fig, ax1 = plt.subplots()
                ax2 = ax1.twinx()
                ax1.set_xlabel("Carbon Tax (£/t)", fontsize = labelFontSize)
                ax1.set_ylabel("Annualised ${CO_2}$ emission (t)", fontsize = labelFontSize)
                ax2.set_ylabel("Annualised ${CO_2}$ emission (t)", fontsize = labelFontSize)
 
                text = weatherConditionList[w][2] + " (" + str(weighterList[weight_index]) + ")"
                for i in range(len(CO2Emission_sameWeatherSameWeight)):   
                    ax1.plot(CarbonTaxForOPFList, CO2Emission_sameWeatherSameWeight[i], color = colors[i], label = str(NumberOfSMRUnitList[i]) + " SMRs")
                    ax2.plot(CarbonTaxForOPFList, CO2Emission_sameWeatherSameWeight[i], color = colors[i], alpha=0) 
                ## ax1.legend(fontsize = legendFontSize, loc='upper left')
                ax1.legend(
                    loc="upper center",
                    fontsize = legendFontSize,
                    ncol=3,
                    bbox_to_anchor=(0.5, 1.05),
                    bbox_transform=fig.transFigure 
                )   
                plt.tight_layout()
                self.mkdirFig()
                plt.savefig(self.diagramPath + 'EmissionvsCarbonTaxOfDifferentSMRNumber_(SMRImpact)_%s.pdf' % text, dpi = 1200, bbox_inches='tight')
                # plt.show() ## show must come after the savefig
                # plt.close()
                plt.clf()
                plt.cla()
        return

    def stackAreaGraph_EnergyBreakDown(self, weighterList, energyBreakdown_eachSMRDesign, genTypeLabel, NumberOfSMRUnitList, CarbonTaxForOPFList, weatherConditionList):
        if len(energyBreakdown_eachSMRDesign) != len(NumberOfSMRUnitList):
            raiseExceptions("The length of the result list should equal to the number of the SMR list number!")
        ## set up the clourmap
        cmap = mpl.cm.get_cmap("viridis", len(genTypeLabel))
        colors = cmap(numpy.linspace(0, 1, len(genTypeLabel)))

        for w in range(len(weatherConditionList)):
            for weight_index in range(len(weighterList)):
                for result_sameSMR in energyBreakdown_eachSMRDesign:
                    energyBreakdown_sameSMRNumber = [ [] for i in range(len(genTypeLabel))]
                    smrNumber = NumberOfSMRUnitList[energyBreakdown_eachSMRDesign.index(result_sameSMR)]
                    for c in range(len(CarbonTaxForOPFList)):
                        for eb_index, eb in enumerate(result_sameSMR[c][w][weight_index]):
                            energyBreakdown_sameSMRNumber[eb_index].append(eb)

                    fig, ax1 = plt.subplots()
                    #ax2 = ax1.twinx()
                    ax1.set_xlabel("Carbon Tax (£/t)", fontsize = labelFontSize)
                    ax1.set_ylabel("Generation Breakdown (kW)", fontsize = labelFontSize)
                    #ax2.set_ylabel("Generation Breakdown (-)", fontsize = labelFontSize)

                    ax1.stackplot(CarbonTaxForOPFList, energyBreakdown_sameSMRNumber, labels = genTypeLabel, colors = colors)
                    #ax2.stackplot(CarbonTaxForOPFList, energyBreakdown_sameSMRNumber, alpha=0) 
                    # def to_percent(temp, position):
                    #     return '%1.0f'%(10*temp) + '%'
                    # plt.gca().yaxis.set_major_formatter(ticker.FuncFormatter(to_percent))
                    
                    pos = ax1.get_position()
                    ax1.set_position([pos.x0, pos.y0, pos.width, pos.height * 0.85])
                    ax1.legend(
                        loc="upper center",
                        ## fontsize = legendFontSize,
                        # ncol=3,
                        bbox_to_anchor=(0.5, 1.05),
                        ## bbox_transform=fig.transFigure 
                    )

                    plt.tight_layout()
                    self.mkdirFig()
                    text = 'SMR_' + str(smrNumber) + '_' + weatherConditionList[w][2] + " (" + str(weighterList[weight_index]) + ")"        
                    plt.savefig(self.diagramPath + 'EnergyBreakdown_%s.pdf' % text, dpi = 1200, bbox_inches='tight')
                    # plt.show() ## show must come after the savefig
                    # plt.close()
                    plt.clf()
                    plt.cla()
        return

if __name__ == '__main__':        
    topologyNodeIRI_10Bus = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerGridTopology_b22aaffa-fd51-4643-98a3-ff72ee04e21e" 
    topologyNodeIRI_29Bus = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerGridTopology_6017554a-98bb-4896-bc21-e455cb6b3958" 
    
    AgentIRI = "http://www.example.com/triplestore/agents/Service__XXXAgent#Service"
    slackBusNodeIRI = "http://www.theworldavatar.com/kb/ontopowsys/BusNode_1f3c4462-3472-4949-bffb-eae7d3135591"  ## the slack bus is recongnised by its latlon
    slackBusNodeIRI_29Bus = "http://www.theworldavatar.com/kb/ontopowsys/BusNode_bc386bcb-33ab-4569-80c5-00dc9d0bffb8"  

    queryEndpointLabel = "ukdigitaltwin_test2"
    geospatialQueryEndpointLabel = "ukdigitaltwin_pd"
    updateEndPointURL = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test3/sparql"
    loadAllocatorName = "regionalDemandLoad"
    loadAllocatorName_29Bus = "closestDemandLoad"

    EBusModelVariableInitialisationMethodName = "defaultInitialisation"
    EBusModelVariableInitialisationMethodName_29Bus = "preSpecified"

    ELineInitialisationMethodName = "defaultBranchInitialiser"
    ELineInitialisationMethodName_29Bus = "preSpecifiedBranchInitialiser"
    
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

    discountRate = 0.02
    bankRate = 0.0125
    projectLifeSpan = 40
    SMRCapitalCost = 1800000000
    MonetaryValuePerHumanLife = 2400000
    NeighbourhoodRadiusForSMRUnitOf1MW = 200
    ProbabilityOfReactorFailure = 0.002985
    SMRCapability = 470
    maxmumSMRUnitAtOneSite = 4
    SMRIntergratedDiscount = 0.9
    DecommissioningCostEstimatedLevel = 1
    slackFactor = 1.1
    safeDistance = 20
    ## TODO: stop generating the JSON files
    generateVisualisationJSON = True
    pop_size = 1000
    n_offsprings = 1000
    numberOfGenerations = 400

    pickedWeight = 0.5

    NumberOfSMRUnitList = [0, 5, 10, 25, 30, 40, 45, 50, 54] #[0, 1, 5, 10, 15, 20, 22, 24, 25, 28, 30, 35, 40, 45, 47, 50, 54, 60]
    weighterList = [0, 0.25, 0.5, 0.75, 0.85, 0.9, 1]
    CarbonTaxForOPFList = [0, 5, 10, 20, 40, 60, 70, 80, 100] #, 120, 150, 200] 
    weatherConditionList = [[0.67, 0.74, "WHSH"], [0.088, 0.74, "WLSH"], [0.67, 0.033, "WHSL"], [0.088, 0.033, "WLSL"]]

    ###FORTEST###
    # NumberOfSMRUnitList = [5] 
    # weighterList = [0.5]
    # CarbonTaxForOPFList = [40]
    # weatherConditionList = [[0.67, 0.74, "WHSH"]]
    
    ifReadLocalResults = True
#############10 BUS Model#################################################################################################################################################################
    # testOPF1 = OptimalPowerFlowAnalysis(topologyNodeIRI_10Bus, AgentIRI, "2017-01-31", slackBusNodeIRI, loadAllocatorName, EBusModelVariableInitialisationMethodName, ELineInitialisationMethodName,
    #     piecewiseOrPolynomial, pointsOfPiecewiseOrcostFuncOrder, baseMVA, withRetrofit, retrofitGenerator, retrofitGenerationFuelOrTechType, newGeneratorType, discountRate, bankRate, 
    #     projectLifeSpan, SMRCapitalCost, MonetaryValuePerHumanLife, NeighbourhoodRadiusForSMRUnitOf1MW, ProbabilityOfReactorFailure, SMRCapability, maxmumSMRUnitAtOneSite, SMRIntergratedDiscount,
    #     DecommissioningCostEstimatedLevel, queryEndpointLabel, geospatialQueryEndpointLabel, updateEndPointURL)
    
    # testOPF1.retrofitGeneratorInstanceFinder()
    # testOPF1.ModelPythonObjectInputInitialiser(CarbonTaxForOPF)
    # testOPF1.OPFModelInputFormatter()
    # testOPF1.OptimalPowerFlowAnalysisSimulation()
    # testOPF1.ModelOutputFormatter()
    # testOPF1.CarbonEmissionCalculator()
    # testOPF1.EnergySupplyBreakDownPieChartCreator()
    # testOPF1.ModelPythonObjectOntologiser()

    def visulasitionOfCluster(list, file_label):
        geojson_file = """
        {
            "type": "FeatureCollection",
            "features": ["""
        for extant_gen in list:
            feature = """{
                "type": "Feature",
                "properties": {
                "marker-color": "#000000",
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
                },"""%( extant_gen['LatLon'][1], extant_gen['LatLon'][0])
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
############29 Bus model##################################################################################################################################################################
    testOPF_29BusModel = OptimalPowerFlowAnalysis(topologyNodeIRI_29Bus, AgentIRI, "2017-01-31", slackBusNodeIRI_29Bus, loadAllocatorName_29Bus, 
        EBusModelVariableInitialisationMethodName_29Bus, ELineInitialisationMethodName_29Bus, piecewiseOrPolynomial, pointsOfPiecewiseOrcostFuncOrder, 
        baseMVA, withRetrofit, retrofitGenerator, retrofitGenerationFuelOrTechType, newGeneratorType, discountRate, bankRate, projectLifeSpan, 
        SMRCapitalCost, MonetaryValuePerHumanLife, NeighbourhoodRadiusForSMRUnitOf1MW, ProbabilityOfReactorFailure, SMRCapability, maxmumSMRUnitAtOneSite, 
        SMRIntergratedDiscount, DecommissioningCostEstimatedLevel, safeDistance, pop_size, n_offsprings, numberOfGenerations, updateEndPointURL)  
        
####================NEW demanding assessment method Pre-OPF method================####
    # ## Pre-OPF to determing the demanding indicator of each area without retrofitting
    # testOPF_29BusModel.powerPlantAndDemandingAreasMapper()
    # testOPF_29BusModel.ModelPythonObjectInputInitialiser_BusAndBranch()
    # testOPF_29BusModel.ModelPythonObjectInputInitialiser_Generator(CarbonTaxForOPF, False)
    # testOPF_29BusModel.OPFModelInputFormatter()
    # testOPF_29BusModel.OptimalPowerFlowAnalysisSimulation()
    # testOPF_29BusModel.ModelOutputFormatter()
    # testOPF_29BusModel.CarbonEmissionCalculator()
    # testOPF_29BusModel.netDemandingCalculator()
    # re = [CarbonTaxForOPF, testOPF_29BusModel.totalCost, testOPF_29BusModel.annualisedOPEX, testOPF_29BusModel.retrofittingCost, testOPF_29BusModel.totalCO2Emission]
    # summary.append(re)
    # print(summary)
    # ## Initialising the site selection
    # testOPF_29BusModel.retrofitGeneratorInstanceFinder()
    # for smrUnit in NumberOfSMRUnitList:
    #     testOPF_29BusModel.siteSelector(int(smrUnit))
    #     testOPF_29BusModel.ModelPythonObjectInputInitialiser_Generator(CarbonTaxForOPF, True)
    #     testOPF_29BusModel.OPFModelInputFormatter()
    #     testOPF_29BusModel.OptimalPowerFlowAnalysisSimulation()
    #     testOPF_29BusModel.ModelOutputFormatter()
    #     testOPF_29BusModel.CarbonEmissionCalculator()
    #     re = [CarbonTaxForOPF, testOPF_29BusModel.totalCost, testOPF_29BusModel.annualisedOPEX, testOPF_29BusModel.retrofittingCost, testOPF_29BusModel.totalCO2Emission]
    #     summary.append(re)
    #     ## testOPF_29BusModel.EnergySupplyBreakDownPieChartCreator()
    #     testOPF_29BusModel.visualisationFileCreator_ExtantGenerator('pre-opf_29BusModel_' + str(smrUnit) + '_SMRs_retrofitted_ExtantGenerator_CarbonTax' + str(CarbonTaxForOPF))
    #     testOPF_29BusModel.visualisationFileCreator_AddedGenerator('pre-opf_29BusModel_' + str(smrUnit) + '_SMRs_retrofitted_SMR_CarbonTax' + str(CarbonTaxForOPF))   
    #     testOPF_29BusModel.visualisationFileCreator_ClosedGenerator('pre-opf_29BusModel_' + str(smrUnit) + '_SMRs_retrofitted_ClosedPlants_CarbonTax' + str(CarbonTaxForOPF))
    #     print(summary)
    # print(summary)
###TODO: check all of the index in the for loop. avoid using the same index
####================ OLD demanding assessment method: without pre-OPF ================####
    if not ifReadLocalResults:
        testOPF_29BusModel.retrofitGeneratorInstanceFinder() ## determine the retrofitListBeforeSelection, population_list and weightedDemandingDistance_list
        ## visulasitionOfCluster(testOPF_29BusModel.retrofitListBeforeSelection, 'clusterResults')
        testOPF_29BusModel.ModelPythonObjectInputInitialiser_BusAndBranch()
        summary_eachSMRDesign = []
        SMROutputAndOperationalRatio_eachSMRDesign = []
        ratio_eachSMRDesign = []
        emission_eachSMRDesign = []
        energyBreakdown_eachSMRDesign = []
        for numberOfSMRToBeIntroduced in NumberOfSMRUnitList:
            print('===The number of SMR is: ', str(numberOfSMRToBeIntroduced))
            testOPF_29BusModel.siteSelector(numberOfSMRToBeIntroduced)
            testOPF_29BusModel.optimaPicker(weighterList)
            ##  testOPF_29BusModel.ModelPythonObjectInputInitialiser_BusAndBranch()
            summary_eachCarbonTax = []
            SMR_eachCarbonTax = []
            ratio_eachCarbonTax = []
            emission_eachCarbonTax = []
            energyBreakdown_eachCarbonTax = []
            for CarbonTaxForOPF in CarbonTaxForOPFList:
                ## generatorNameList = []
                ## before decommssion: find the potential decommssion power plant 
                summary_eachWeather = []
                ratio_eachWeather = []
                SMR_eachWeather = []
                emission_eachWeather = []
                energyBreakdown_eachWeather = []
                for weatherCondition in weatherConditionList:
                    testOPF_29BusModel.ModelPythonObjectInputInitialiser_Generator(CarbonTaxForOPF, True, weatherCondition[0], weatherCondition[1], weatherCondition[2], False)
                    testOPF_29BusModel.OPFModelInputFormatter()
                    testOPF_29BusModel.OptimalPowerFlowAnalysisSimulation()
                    testOPF_29BusModel.ModelOutputFormatter(generateVisualisationJSON)
                    testOPF_29BusModel.CarbonEmissionCalculator()
                    ## generatorNameList.append(testOPF_29BusModel.GeneratorObjectList)
                    re_totalCostAndTotalEmission = [testOPF_29BusModel.totalCostList, testOPF_29BusModel.totalCO2EmissionList]
                    re_OPEXRatio = [testOPF_29BusModel.OPEXRatioList]
                    smr_outputAndRatio = [testOPF_29BusModel.SMRTotalOutputList, testOPF_29BusModel.SMRTotalOperationalRationList]
                    re_emission = [testOPF_29BusModel.annualisedTotalEmissionCostList, testOPF_29BusModel.emissionCostContributionList_OPEX, testOPF_29BusModel.emissionCostContributionList_TotalCost]
                    summary_eachWeather.append(re_totalCostAndTotalEmission)
                    ratio_eachWeather.append(re_OPEXRatio)
                    SMR_eachWeather.append(smr_outputAndRatio)
                    emission_eachWeather.append(re_emission)
                    energyBreakdown_eachWeight, genTypeLabel = testOPF_29BusModel.EnergySupplyBreakDownPieChartCreator(weighterList, CarbonTaxForOPF, weatherCondition, numberOfSMRToBeIntroduced)
                    energyBreakdown_eachWeather.append(energyBreakdown_eachWeight)
                summary_eachCarbonTax.append(summary_eachWeather)
                ratio_eachCarbonTax.append(ratio_eachWeather)
                SMR_eachCarbonTax.append(SMR_eachWeather)
                emission_eachCarbonTax.append(emission_eachWeather)
                energyBreakdown_eachCarbonTax.append(energyBreakdown_eachWeather)
            summary_eachSMRDesign.append(summary_eachCarbonTax)
            ratio_eachSMRDesign.append(ratio_eachCarbonTax)
            SMROutputAndOperationalRatio_eachSMRDesign.append(SMR_eachCarbonTax)
            emission_eachSMRDesign.append(emission_eachCarbonTax)
            energyBreakdown_eachSMRDesign.append(energyBreakdown_eachCarbonTax)

        np_summary_eachSMRDesign = numpy.array(summary_eachSMRDesign)
        np_SMROutputAndOperationalRatio_eachSMRDesign = numpy.array(SMROutputAndOperationalRatio_eachSMRDesign)
        np_emission_eachSMRDesign = numpy.array(emission_eachSMRDesign) 
        np_energyBreakdown_eachSMRDesign = numpy.array(energyBreakdown_eachSMRDesign) 
        np_genTypeLabel = numpy.array(genTypeLabel) 

        numpy.save("np_summary_eachSMRDesign.npy", np_summary_eachSMRDesign)
        numpy.save("np_SMROutputAndOperationalRatio_eachSMRDesign.npy", np_SMROutputAndOperationalRatio_eachSMRDesign)
        numpy.save("np_emission_eachSMRDesign.npy", np_emission_eachSMRDesign)
        numpy.save("np_energyBreakdown_eachSMRDesign.npy", np_energyBreakdown_eachSMRDesign)
        numpy.save("np_genTypeLabel.npy", np_genTypeLabel)
    else:
        summary_eachSMRDesign = numpy.load("np_summary_eachSMRDesign.npy")
        SMROutputAndOperationalRatio_eachSMRDesign = numpy.load("np_SMROutputAndOperationalRatio_eachSMRDesign.npy")
        emission_eachSMRDesign = numpy.load("np_emission_eachSMRDesign.npy")
        energyBreakdown_eachSMRDesign = numpy.load("np_energyBreakdown_eachSMRDesign.npy")
        genTypeLabel = numpy.load("np_genTypeLabel.npy")

        summary_eachSMRDesign = summary_eachSMRDesign.tolist()
        SMROutputAndOperationalRatio_eachSMRDesign = SMROutputAndOperationalRatio_eachSMRDesign.tolist()
        emission_eachSMRDesign = emission_eachSMRDesign.tolist()
        energyBreakdown_eachSMRDesign = energyBreakdown_eachSMRDesign.tolist()
        genTypeLabel = genTypeLabel.tolist()      

    # testOPF_29BusModel.resultsSheetCreator(weighterList, NumberOfSMRUnitList, weatherConditionList, CarbonTaxForOPFList, summary_eachSMRDesign)
    # testOPF_29BusModel.dataHeatmapCreator_totalCostAndEmission(summary_eachSMRDesign, CarbonTaxForOPFList, NumberOfSMRUnitList, weatherConditionList, weighterList)   
    # testOPF_29BusModel.dataHeatmapCreator_SMROutputAndOperationalRatio(SMROutputAndOperationalRatio_eachSMRDesign, CarbonTaxForOPFList, NumberOfSMRUnitList, weatherConditionList, weighterList)   
    # testOPF_29BusModel.dataHeatmapCreator_CO2Emission(emission_eachSMRDesign, CarbonTaxForOPFList, NumberOfSMRUnitList, weatherConditionList, weighterList)   
     
    ## The line charts showing the procesed results 
    testOPF_29BusModel.lineGraph_weatherImpact(pickedWeight, weighterList, summary_eachSMRDesign, NumberOfSMRUnitList, CarbonTaxForOPFList, weatherConditionList)    
    testOPF_29BusModel.lineGraph_weightImpact(weighterList, summary_eachSMRDesign, NumberOfSMRUnitList, CarbonTaxForOPFList, weatherConditionList)
    testOPF_29BusModel.lineGraph_SMRImpactForCO2Emission(weighterList, summary_eachSMRDesign, NumberOfSMRUnitList, CarbonTaxForOPFList, weatherConditionList)
    testOPF_29BusModel.stackAreaGraph_EnergyBreakDown(weighterList, energyBreakdown_eachSMRDesign, genTypeLabel, NumberOfSMRUnitList, CarbonTaxForOPFList, weatherConditionList)


    # #testOPF_29BusModel.dataHeatmapCreator_OPEXRatio(ratio_eachSMRDesign, CarbonTaxForOPFList, NumberOfSMRUnitList, weatherConditionList, weighterList)         
            # ## find the decommssioned power plant
            # testOPF_29BusModel.decommissionPowerPlantDecider(numberOfSMRToBeIntroduced, slackFactor, generatorNameList)   

            # ## after decommssion
            # for weatherCondition in weatherConditionList:
            #     testOPF_29BusModel.ModelPythonObjectInputInitialiser_Generator(CarbonTaxForOPF, True, weatherCondition[0], weatherCondition[1], weatherCondition[2], True)
            #     testOPF_29BusModel.OPFModelInputFormatter()
            #     testOPF_29BusModel.OptimalPowerFlowAnalysisSimulation()
            #     testOPF_29BusModel.ModelOutputFormatter()
            #     testOPF_29BusModel.CarbonEmissionCalculator()
            #     generatorNameList.append(testOPF_29BusModel.GeneratorObjectList)
                
            #     re = [weighter, CarbonTaxForOPF, weatherCondition, testOPF_29BusModel.totalCost, testOPF_29BusModel.annualisedOPEX, testOPF_29BusModel.retrofittingCost, str(testOPF_29BusModel.totalCO2Emission) + "\\n"]
            #     summary_afterdecommission.append(re)

        # fileName_beforeDecommission = 'Results_SMR_' + str(numberOfSMRToBeIntroduced) + '_beforeDecommission.csv'
        # fileName_afterDecommission = 'Results_SMR_' + str(numberOfSMRToBeIntroduced) + 'weight_' + str(weighterList[0]) + '_afterDecommission.csv'
        # with open(fileName_beforeDecommission, 'w') as f:
        #             write = csv.writer(f)
        #             write.writerow(summary_beforedecommission)
        # with open(fileName_afterDecommission, 'w') as f:
        #             write = csv.writer(f)
        #             write.writerow(summary_afterdecommission)
    print('Terminal')

#     # print("*****This are EBus results*****")
#     # for attr in testOPF1.ObjectSet.get('EBus-7').__dir__():
#     #     print(attr, getattr(testOPF1.ObjectSet.get('EBus-7'), attr))
#     # for attr in testOPF1.ObjectSet.get('EBus-8').__dir__():
#     #     print(attr, getattr(testOPF1.ObjectSet.get('EBus-8'), attr))

#     # print("*****This are ELine results*****")
#     # for attr in testOPF1.ObjectSet.get('ELine-0').__dir__():
#     #     print(attr, getattr(testOPF1.ObjectSet.get('ELine-0'), attr)) 
        
#     # print("*****This are EGen results*****")
#     # for attr in testOPF1.ObjectSet.get('EGen-1134').__dir__():
#     #     print(attr, getattr(testOPF1.ObjectSet.get('EGen-1134'), attr)) 

    # print("*****This are EGen toberetrofitted results*****")
    # for attr in testOPF1.ObjectSet.get('EGenRetrofit-11').__dir__():
    #     print(attr, getattr(testOPF1.ObjectSet.get('EGenRetrofit-11'), attr)) 