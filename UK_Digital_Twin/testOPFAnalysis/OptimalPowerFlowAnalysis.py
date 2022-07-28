##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 28 July 2022         #
##########################################

"""
Optimal Flow Analysis
"""
from ast import Str
from cgi import test
from logging import raiseExceptions
import sys, os, numpy, uuid
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

## create configuration objects
SLASH = '/'
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
        CarbonTax:float, piecewiseOrPolynomial:int, pointsOfPiecewiseOrcostFuncOrder:int, baseMVA: float,  
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
        demandCapacityRatio:float,
        DiscommissioningCostEstimatedLevel:int,
        shutNonRetrofittedGenerator:bool,
        queryEndpointLabel:str, geospatialQueryEndpointLabel:str, endPointURL:str, endPointUser:str = None, endPointPassWord:str = None,
        OWLFileStoragePath = None, updateLocalPowerPlantOWLFileFlag:bool = True
        ):
       
        ##--1. newly created--##
        ## create the power system model node IRI
        self.powerSystemModelIRI = UK_PG.ontopowsys_namespace + UK_PG.powerSystemModelKey + str(uuid.uuid4())
        ## create the timeStamp, e.x. 2022-06-15T16:24:29.371941+00:00
        self.timeStamp = datetime.now(pytz.utc).isoformat()
        ## query the number of the bus under the topology node IRI, and the bus node IRI, branch node IRI and generator node IRI
        self.numOfBus, self.busNodeList = query_model.queryBusTopologicalInformation(topologyNodeIRI, queryEndpointLabel) ## ?BusNodeIRI ?BusLatLon
        self.branchNodeList, self.branchVoltageLevel = query_model.queryELineTopologicalInformation(topologyNodeIRI, queryEndpointLabel) ## ?ELineNode ?From_Bus ?To_Bus ?Value_Length_ELine ?Num_OHL_400 or 275 
        self.generatorNodeList = query_model.queryEGenInfo(topologyNodeIRI, queryEndpointLabel) ## ?PowerGeneratorIRI ?FixedMO ?VarMO ?FuelCost ?CO2EmissionFactor ?Bus ?Capacity ?PrimaryFuel ?Latlon       
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
        ## specify the CarbonTax
        self.CarbonTax = float(CarbonTax)
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
        ##--4. specify the query/update endpoint information--##
        self.queryEndpointLabel = queryEndpointLabel
        self.geospatialQueryEndpointLabel = geospatialQueryEndpointLabel
        self.endPointURL = endPointURL
        self.endPointUser = endPointUser
        self.endPointPassWord = endPointPassWord
        ## specify the local storage path (to be deleted)
        self.OWLFileStoragePath = OWLFileStoragePath
        self.updateLocalPowerPlantOWLFileFlag = updateLocalPowerPlantOWLFileFlag       
        ##--5. JVM module view and use it to import the required java classes--##
        self.jpsBaseLib_view = jpsBaseLibGW.createModuleView()
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")
        jpsBaseLibGW.importPackages(self.jpsBaseLib_view,"uk.ac.cam.cares.jps.base.derivation.*")
        ## initialise the storeClient with SPARQL Query and Update endpoint
        if self.endPointUser is None:
            self.storeClient = self.jpsBaseLib_view.RemoteStoreClient(self.endPointURL, self.endPointURL)
        else:
            self.storeClient = self.jpsBaseLib_view.RemoteStoreClient(self.endPointURL, self.endPointURL, self.endPointUser, self.endPointPassWord)
        ## initialise the derivationClient
        self.derivationClient = self.jpsBaseLib_view.DerivationClient(self.storeClient, derivationInstanceBaseURL)  
        ##--6. Identify the retrofitting generators--##
        if type(withRetrofit) is not bool:
            raiseExceptions("withRetrofit has to be a bool number")
        else:
            self.withRetrofit = withRetrofit 
        if type(shutNonRetrofittedGenerator) is not bool:
            raiseExceptions("shutNonRetrofittedGenerator has to be a bool number")
        else:
            self.shutNonRetrofittedGenerator = shutNonRetrofittedGenerator
        self.retrofitGenerator = retrofitGenerator # GeneratorIRI, location, capacity
        self.retrofitGenerationFuelOrGenType = retrofitGenerationTechTypeOrGenerationTechnology    
        ##--7. Identify the generators type used to replace the exisiting generators--##
        self.newGeneratorType = newGeneratorType
        ##--8. initialise the bus/branch/generator objects name list--##
        self.BusObjectList: list = []
        self.BranchObjectList: list = []
        self.GeneratorObjectList: list = []
        self.GeneratorToBeRetrofittedObjectList: list = []
        ##--9. site selection factors--##
        self.discountRate = discountRate
        self.bankRate = bankRate
        self.projectLifeSpan = projectLifeSpan
        self.SMRCapitalCost = SMRCapitalCost
        self.maxmumSMRUnitAtOneSite = maxmumSMRUnitAtOneSite
        self.MonetaryValuePerHumanLife = MonetaryValuePerHumanLife 
        self.NeighbourhoodRadiusForSMRUnitOf1MW = NeighbourhoodRadiusForSMRUnitOf1MW
        self.ProbabilityOfReactorFailure = ProbabilityOfReactorFailure
        self.SMRCapability = SMRCapability
        self.demandCapacityRatio = demandCapacityRatio
        self.DiscommissioningCostEstimatedLevel = DiscommissioningCostEstimatedLevel

    """This method is called to select the site to be replaced by SMR"""
    def retrofitGeneratorInstanceFinder(self):
        if self.withRetrofit is True: 
            if len(self.retrofitGenerator) == 0 and len(self.retrofitGenerationFuelOrGenType) == 0:  
                print("***As there is not specific generator assigned to be retrofitted by SMR, all generators located in GB will be treated as the potential sites.***")
                ## retrofitList：[0]PowerGenerator, [1]Bus, [2]Capacity, [3]LatLon, [4]fuelOrGenType, [5]annualOperatingHours, [6] CO2EmissionFactor ([7]numberOfSMR added after SMR site selection)
                retrofitListBeforeSelection = queryOPFInput.queryGeneratorToBeRetrofitted_AllPowerPlant(self.topologyNodeIRI, self.queryEndpointLabel)
            elif not len(self.retrofitGenerator) == 0:
                for iri in self.retrofitGenerator:
                    parse(iri, rule='IRI')
                retrofitListBeforeSelection = queryOPFInput.queryGeneratorToBeRetrofitted_SelectedGenerator(self.retrofitGenerator, self.queryEndpointLabel) 
            elif not len(self.retrofitGenerationFuelOrGenType) == 0:
                for iri in self.retrofitGenerationFuelOrGenType:
                    parse(iri, rule='IRI')
                retrofitListBeforeSelection = queryOPFInput.queryGeneratorToBeRetrofitted_SelectedFuelOrGenerationTechnologyType(self.retrofitGenerationFuelOrGenType, self.topologyNodeIRI, self.queryEndpointLabel)
            
            self.retrofitListBeforeSelection = retrofitListBeforeSelection
            retrofitListBeforeSelection_ = retrofitListBeforeSelection.copy()

            print(self.geospatialQueryEndpointLabel, self.discountRate, self.projectLifeSpan, self.SMRCapitalCost, \
                self.MonetaryValuePerHumanLife, self.NeighbourhoodRadiusForSMRUnitOf1MW, self.ProbabilityOfReactorFailure, self.SMRCapability, self.demandCapacityRatio, \
                self.bankRate, self.CarbonTax, self.shutNonRetrofittedGenerator, self.maxmumSMRUnitAtOneSite, self.DiscommissioningCostEstimatedLevel)

            ## Perform site pre-selection analysis
            siteSelector = sp.SitePreSelection(self.geospatialQueryEndpointLabel, retrofitListBeforeSelection, self.discountRate, self.projectLifeSpan, self.SMRCapitalCost, \
                self.MonetaryValuePerHumanLife, self.NeighbourhoodRadiusForSMRUnitOf1MW, self.ProbabilityOfReactorFailure, self.SMRCapability, self.demandCapacityRatio, \
                self.bankRate, self.CarbonTax, self.shutNonRetrofittedGenerator, self.maxmumSMRUnitAtOneSite, self.DiscommissioningCostEstimatedLevel)
            siteSelector.SMRSitePreSelector()
            
            self.siteToBeReplaced = siteSelector.siteSelected

            siteToBeReplaced_copy = []
            for item in siteSelector.siteSelected:
                item_ = item.copy()
                siteToBeReplaced_copy.append(item_)

            for site in siteToBeReplaced_copy:
                retrofitListBeforeSelection_.remove(site)

            self.siteNotSelected = retrofitListBeforeSelection_ ## those not to be replaced generator will keep running or shut down

        print("The carbon tax is £/tCO2", self.CarbonTax)
        print("The total number of generator is", len(self.generatorNodeList))
        print("The total number of protential sites is", len(self.retrofitListBeforeSelection))
        print("The total number of sites to be replcced by SMR is", len(self.siteToBeReplaced))
        print("The total number of sites not to be replaced (shut-down)", len(self.siteNotSelected))
        return 

    """This method is called to initialize the model entities objects: model input"""
    def ModelPythonObjectInputInitialiser(self): 
        ##-- create model bus, branch and generator objects dynamically --##
        ObjectSet = locals()  

        ###=== Initialisation of the Bus Model Entities ===###
        ## create an instance of class demandLoadAllocator
        dla = DLA.demandLoadAllocator()
        ## get the load allocation method via getattr function 
        allocator = getattr(dla, self.loadAllocatorName)
        ## pass the arrguments to the cluster method
        EBus_Load_List, aggregatedBusFlag = allocator(self.busNodeList, self.startTime_of_EnergyConsumption, self.numOfBus) # busNodeList[0]: EquipmentConnection_EBus, busNodeList[1]: v_TotalELecConsumption 

        ## check if the allocator method is applicable
        while EBus_Load_List == None:
            loadAllocatorName = str(input('The current allocator is not applicable. Please choose another allocator: '))
            # get the load allocation method via getattr function 
            allocator = getattr(dla, loadAllocatorName)
            # pass the arrguments to the cluster method
            EBus_Load_List, aggregatedBusFlag = allocator(self.busNodeList, self.startTime_of_EnergyConsumption, self.numOfBus) # busNodeList[0]: EquipmentConnection_EBus, busNodeList[1]: v_TotalELecConsumption 
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
            ObjectSet[objectName] = initialiser(uk_ebus_model, ebus, self.busNodeList.index(ebus), self.slackBusNodeIRI) 
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
            # ## TODO: when initialise the 29-bus model, please check if ELineNodeIRI is the right node to use
            ObjectSet[objectName] = initialiser(eline['ELineNode'], uk_eline_model, eline, self.branchVoltageLevel, self.OrderedBusNodeIRIList, self.queryEndpointLabel) 
            self.BranchObjectList.append(objectName)

        ###=== Initialisation of the Generator Model Entities ===###
        capa_demand_ratio = model_EGenABoxGeneration.demandAndCapacityRatioCalculator(self.generatorNodeList, self.topologyNodeIRI, self.startTime_of_EnergyConsumption)
        ## remove the shut down generator from the generatorNodeList
        print('The total number of generators:', len(self.generatorNodeList))
        generatorNodeListNonDeleted = self.generatorNodeList.copy()

        if self.shutNonRetrofittedGenerator:
            for egen in generatorNodeListNonDeleted:
                if egen[0] in [gen["PowerGenerator"] for gen in self.retrofitListBeforeSelection]:
                    self.generatorNodeList.remove(egen)
        else:
            for egen in generatorNodeListNonDeleted:
                if egen[0] in [gen["PowerGenerator"] for gen in self.siteToBeReplaced]:
                    self.generatorNodeList.remove(egen)

        print("The number of the existing power plant is", len(self.generatorNodeList))

        for egen in self.generatorNodeList:
            objectName = UK_PG.UKEGenModel.EGenKey + str(self.generatorNodeList.index(egen)) ## egen model python object name
            uk_egen_OPF_model = UK_PG.UKEGenModel_CostFunc(int(self.numOfBus), str(egen[0]), float(egen[4]), str(egen[7]), None, self.CarbonTax, self.piecewiseOrPolynomial, self.pointsOfPiecewiseOrcostFuncOrder)
            uk_egen_OPF_model = costFuncPara(uk_egen_OPF_model, egen)
            ###add EGen model parametor###
            ObjectSet[objectName] = model_EGenABoxGeneration.initialiseEGenModelVar(uk_egen_OPF_model, egen, self.OrderedBusNodeIRIList, capa_demand_ratio)
            self.GeneratorObjectList.append(objectName)
        
        ### Initialisation of the SMR Generator Model Entities ###
        if self.withRetrofit is True:
            for i in range(len(modelFactorArrays)):
                if str(self.newGeneratorType) in modelFactorArrays[i]:
                    factorArray = modelFactorArrays[i]
                    break
            if not 'factorArray' in locals():
                raiseExceptions("The given generator type which used to retrofit the existing genenrators cannot be found in the factor list, please check the generator type.")

            for egen_re in self.siteToBeReplaced:
                objectName = UK_PG.UKEGenModel.EGenRetrofitKey + str(self.siteToBeReplaced.index(egen_re)) ## egen_toBeRetrofitted model python object name 
                newGeneratorNodeIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + ukpp.RealizationAspectKey + str(uuid.uuid4()) 
                uk_egen_re_OPF_model = UK_PG.UKEGenModel_CostFunc(int(self.numOfBus), newGeneratorNodeIRI, 0, str(self.newGeneratorType), str(egen_re["PowerGenerator"]), self.CarbonTax, self.piecewiseOrPolynomial, self.pointsOfPiecewiseOrcostFuncOrder)
                egen_re = [egen_re["PowerGenerator"], float(factorArray[1]), float(factorArray[2]), float(factorArray[3]), float(factorArray[4]), egen_re["Bus"], self.SMRCapability * egen_re["numberOfSMR"], self.newGeneratorType] ## ?PowerGenerator ?FixedMO ?VarMO ?FuelCost ?CO2EmissionFactor ?Bus ?Capacity ?fuel type
                uk_egen_re_OPF_model = costFuncPara(uk_egen_re_OPF_model, egen_re)
                ###add EGen model parametor###
                ObjectSet[objectName] = model_EGenABoxGeneration.initialiseEGenModelVar(uk_egen_re_OPF_model, egen_re, self.OrderedBusNodeIRIList, capa_demand_ratio)
                self.GeneratorToBeRetrofittedObjectList.append(objectName)
                #self.GeneratorObjectList.append(objectName)
        else: 
            self.GeneratorToBeRetrofittedObjectList = []
        ## print(len(self.siteToBeReplaced), len(self.GeneratorToBeRetrofittedObjectList), len(self.GeneratorObjectList))
        self.ObjectSet = ObjectSet
        return     
    
        
    def OPFModelInputFormatter(self):
        """
        The OPFModelInputFormatter is used to created the pypower OPF model input from the model objects which are created from the F{ModelObjectInputInitialiser}.
        This function will be called abfer F{ModelObjectInputInitialiser}.
        
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
            for key in UK_PG.UKEbusModel.INPUT_VARIABLE_KEYS:
                index = int(UK_PG.UKEbusModel.INPUT_VARIABLE[key])
                ppc["bus"][index_bus][index] = getattr(self.ObjectSet.get(UK_PG.UKEbusModel.EBusKey + str(index_bus)), key)
            index_bus += 1
            
        ## branch data
        # fbus, tbus, r, x, b, rateA, rateB, rateC, ratio, angle, status, angmin, angmax     
        ppc["branch"] = numpy.zeros((len(self.BranchObjectList), len(UK_PG.UKElineModel.INPUT_VARIABLE_KEYS)), dtype = float)
        index_br  = 0
        while index_br < len(self.BranchObjectList):
            for key in UK_PG.UKElineModel.INPUT_VARIABLE_KEYS:
                index = int(UK_PG.UKElineModel.INPUT_VARIABLE[key])
                ppc["branch"][index_br][index] = getattr(self.ObjectSet.get(UK_PG.UKElineModel.ELineKey + str(index_br)), key)
            index_br += 1
        
        ## generator data
        # bus, Pg, Qg, Qmax, Qmin, Vg, mBase, status, Pmax, Pmin, Pc1, Pc2,
        # Qc1min, Qc1max, Qc2min, Qc2max, ramp_agc, ramp_10, ramp_30, ramp_q, apf

        self.numOfExistAndRetrofittedGenerators = len(self.GeneratorObjectList) + len(self.GeneratorToBeRetrofittedObjectList)
        ppc["gen"] = numpy.zeros((self.numOfExistAndRetrofittedGenerators, len(UK_PG.UKEGenModel.INPUT_VARIABLE_KEYS)), dtype = float)
        index_gen  = 0
        while index_gen < len(self.GeneratorObjectList):
            for key in UK_PG.UKEGenModel.INPUT_VARIABLE_KEYS:
                index = int(UK_PG.UKEGenModel.INPUT_VARIABLE[key])
                ppc["gen"][index_gen][index] = getattr(self.ObjectSet.get(UK_PG.UKEGenModel.EGenKey + str(index_gen)), key)
            index_gen += 1

        index_regen = 0
        while index_gen < self.numOfExistAndRetrofittedGenerators and self.withRetrofit is True:
            for key in UK_PG.UKEGenModel.INPUT_VARIABLE_KEYS:
                index = int(UK_PG.UKEGenModel.INPUT_VARIABLE[key])
                ppc["gen"][index_gen][index] = getattr(self.ObjectSet.get(UK_PG.UKEGenModel.EGenRetrofitKey + str(index_regen)), key)
            index_gen += 1
            index_regen += 1

        ## generator COST data
        # MODEL, STARTUP, SHUTDOWN, NCOST, COST[a, b]
        columnNum = len(UK_PG.UKEGenModel_CostFunc.INPUT_VARIABLE_KEYS) + self.pointsOfPiecewiseOrcostFuncOrder -1
        ppc["gencost"] = numpy.zeros((self.numOfExistAndRetrofittedGenerators, columnNum), dtype = float)
        index_gen  = 0
        while index_gen < len(self.GeneratorObjectList):
            for key in UK_PG.UKEGenModel_CostFunc.INPUT_VARIABLE_KEYS:
                index = int(UK_PG.UKEGenModel_CostFunc.INPUT_VARIABLE[key])
                if key == "COST":
                    for para in getattr(self.ObjectSet.get(UK_PG.UKEGenModel.EGenKey + str(index_gen)), key):
                        ppc["gencost"][index_gen][index] = para
                        index += 1
                else:
                    ppc["gencost"][index_gen][index] = getattr(self.ObjectSet.get(UK_PG.UKEGenModel.EGenKey + str(index_gen)), key)
            index_gen += 1

        index_regen = 0
        while index_gen < self.numOfExistAndRetrofittedGenerators and self.withRetrofit is True:
            for key in UK_PG.UKEGenModel_CostFunc.INPUT_VARIABLE_KEYS:
                index = int(UK_PG.UKEGenModel_CostFunc.INPUT_VARIABLE[key])
                if key == "COST":
                    for para in getattr(self.ObjectSet.get(UK_PG.UKEGenModel.EGenRetrofitKey + str(index_regen)), key):
                        ppc["gencost"][index_gen][index] = para
                        index += 1
                else:
                    ppc["gencost"][index_gen][index] = getattr(self.ObjectSet.get(UK_PG.UKEGenModel.EGenRetrofitKey + str(index_regen)), key)
            index_gen += 1
            index_regen += 1
        self.ppc = ppc
        return 
            
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
         
        if not hasattr(self, 'ppc'): 
            if ppc is None or not isinstance(ppc, list):
                raise Exception("The model input has not been reformatted, please run the function ModelInputFormatter at first.")
            else:
                self.ppc = ppc
    
        ##-- starts opf analysis --## 
        # set up numerical method: Newton's Method
        self.ppopt = ppoption(OUT_ALL = 0, VERBOSE = 2) 
        self.results = runopf(self.ppc, self.ppopt)

        ## TODO: there are several different ways to do OPF analysis
        ## self.results = runuopf(self.ppc, self.ppopt)

        self.totalCost = round(self.results["f"], 2)

        print("***Total cost (£/hr): ", self.totalCost)
        
        self.ConvergeFlag = self.results["success"]
        if self.ConvergeFlag:
            print('-----The OPF model is converged.-----')
        else:
            print('!!!!!!The OPF model is diverged.!!!!!')
        return
     
    def ModelOutputFormatter(self):
        """
        Reformat the result and add attributes into the objects.

        Returns
        -------
        None.

        """
        if not self.ConvergeFlag:  
            raise Exception("!!!!!!The OPF has not converged.!!!!!!")
        
        ## the bus, gen, branch and loss result
        bus = self.results["bus"]
        branch = self.results["branch"]
        gen = self.results["gen"]
        
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
            for key in UK_PG.UKEbusModel.OUTPUT_VARIABLE_KEYS:
                index = int(UK_PG.UKEbusModel.OUTPUT_VARIABLE[key])
                setattr(self.ObjectSet.get(UK_PG.UKEbusModel.EBusKey + str(index_bus)), key, busPostResult[index_bus][index])        
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
            for key in UK_PG.UKElineModel.OUTPUT_VARIABLE_KEYS:
                index = int(UK_PG.UKElineModel.OUTPUT_VARIABLE[key])
                setattr(self.ObjectSet.get(UK_PG.UKElineModel.ELineKey + str(index_br)), key, branchPostResult[index_br][index])        
            index_br += 1   
        
        ##--Generator--##    
        ## PG_OUTPUT, QG_OUTPUT
        ## post processsing of the generator results   
        generatorPostResult = numpy.zeros((self.numOfExistAndRetrofittedGenerators, len(UK_PG.UKEGenModel.OUTPUT_VARIABLE_KEYS)), dtype = float) 
        for i in range(self.numOfExistAndRetrofittedGenerators):
            if (gen[i, GEN_STATUS] > 0) & logical_or(gen[i, PG], gen[i, QG]):
                generatorPostResult[i][UK_PG.UKEGenModel.OUTPUT_VARIABLE["PG_OUTPUT"]] = gen[i][PG]
                generatorPostResult[i][UK_PG.UKEGenModel.OUTPUT_VARIABLE["QG_OUTPUT"]] = gen[i][QG]
        ## update the object attributes with the model results
        index_gen  = 0
        while index_gen < len(self.GeneratorObjectList):
            for key in UK_PG.UKEGenModel.OUTPUT_VARIABLE_KEYS:
                index = int(UK_PG.UKEGenModel.OUTPUT_VARIABLE[key])
                setattr(self.ObjectSet.get(UK_PG.UKEGenModel.EGenKey + str(index_gen)), key, generatorPostResult[index_gen][index])        
            index_gen += 1   
        
        index_regen = 0
        while index_gen < self.numOfExistAndRetrofittedGenerators and self.withRetrofit is True:
            for key in UK_PG.UKEGenModel.OUTPUT_VARIABLE_KEYS:
                index = int(UK_PG.UKEGenModel.OUTPUT_VARIABLE[key])
                setattr(self.ObjectSet.get(UK_PG.UKEGenModel.EGenRetrofitKey + str(index_regen)), key, generatorPostResult[index_gen][index])        
            index_regen += 1
            index_gen += 1 
        return 
    
    def ModelPythonObjectOntologiser(self):
        """
        create KG representation for all model objects
        
        """
        ## create the power system model node IRI
        self.powerSystemModelIRI = UK_PG.ontopowsys_namespace + UK_PG.powerSystemModelKey + str(uuid.uuid4())
        ## create the timeStamp, e.x. 2022-06-15T16:24:29.371941+00:00
        self.timeStamp = datetime.now(pytz.utc).isoformat()

        BusModelKGInstanceCreator(self.ObjectSet, self.BusObjectList, self.numOfBus, self.topologyNodeIRI, self.powerSystemModelIRI, \
            self.timeStamp, self.agentIRI, self.derivationClient, self.endPointURL, self.OWLFileStoragePath)
        BranchModelKGInstanceCreator(self.ObjectSet, self.BranchObjectList, self.numOfBus, self.topologyNodeIRI, self.powerSystemModelIRI, \
            self.timeStamp, self.agentIRI, self.derivationClient, self.endPointURL, self.OWLFileStoragePath)
        ##TODO: check if self.GeneratorToBeRetrofittedObjectList affect the code 
        GeneratorModelKGInstanceCreator(self.ObjectSet, self.GeneratorObjectList, self.GeneratorToBeRetrofittedObjectList, self.OPFOrPF, self.newGeneratorType, self.numOfBus, self.topologyNodeIRI, self.powerSystemModelIRI, \
        self.timeStamp, self.agentIRI, self.derivationClient, self.endPointURL, self.OWLFileStoragePath)  
        
        return 
    
    def CarbonEmissionCalculator(self):
        self.totalCO2Emission = 0
        for gen in self.GeneratorObjectList:
            self.totalCO2Emission += float(self.ObjectSet[gen].PG_OUTPUT) * float(self.ObjectSet[gen].CO2EmissionFactor)
        return 
    
    def energySupplyBreakDownPieChartCreator(self):
        genTypeLabel = []
        outPutData = []
        for gen in self.GeneratorObjectList:
            if not self.ObjectSet[gen].fueltype in genTypeLabel:
                genTypeLabel.append(self.ObjectSet[gen].fueltype)
                outPutData.append(float(self.ObjectSet[gen].PG_OUTPUT))
            else:
                i = genTypeLabel.index(self.ObjectSet[gen].fueltype) 
                outPutData[i] += float(self.ObjectSet[gen].PG_OUTPUT)
        totalOutputOfSMR = 0
        for regen in self.GeneratorToBeRetrofittedObjectList:
            totalOutputOfSMR += regen.ObjectSet[gen].PG_OUTPUT
        
        genTypeLabel.append(self.newGeneratorType)
        outPutData.append(totalOutputOfSMR)

        plt.pie(outPutData, labels=genTypeLabel, autopct='%1.1f%%', shadow=True, startangle=90)
        plt.title('Energy Supply BreakDown')
        plt.axis('equal')
        plt.show()
        
        return 
            

if __name__ == '__main__':        
    topologyNodeIRI_10Bus = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerGridTopology_b22aaffa-fd51-4643-98a3-ff72ee04e21e" 
    # powerSystemModelIRI = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerSystemModel_22fe8504-f3bb-403c-9363-34b258d59712"
    AgentIRI = "http://www.example.com/triplestore/agents/Service__XXXAgent#Service"
    slackBusNodeIRI = "http://www.theworldavatar.com/kb/ontopowsys/BusNode_1f3c4462-3472-4949-bffb-eae7d3135591"   
    queryEndpointLabel = "ukdigitaltwin_test2"
    geospatialQueryEndpointLabel = "ukdigitaltwin_pd"
    updateEndPointURL = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test3/sparql"
    loadAllocatorName = "regionalDemandLoad"
    EBusModelVariableInitialisationMethodName= "defaultInitialisation"
    ELineInitialisationMethodName = "defaultBranchInitialiser"
    CarbonTax = 100
    piecewiseOrPolynomial = 2
    pointsOfPiecewiseOrcostFuncOrder = 2
    baseMVA = 150
    withRetrofit = True
    retrofitGenerator = []
    retrofitGenerationFuelOrTechType = ["http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal", 
    "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Oil",
    "http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas"]
    ##retrofitGenerationTechType = ["http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Nuclear"]
    newGeneratorType = "SMR"
    updateEndPointURL = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test3/sparql"

    shutNonRetrofittedGenerator = False

    discountRate = 0.02
    bankRate = 0.0125
    projectLifeSpan = 40
    SMRCapitalCost = 1800000000
    MonetaryValuePerHumanLife = 2400000
    NeighbourhoodRadiusForSMRUnitOf1MW = 200
    ProbabilityOfReactorFailure = 0.002985
    SMRCapability = 470
    maxmumSMRUnitAtOneSite = 4
    demandCapacityRatio = 0.5
    DiscommissioningCostEstimatedLevel = 1


    testOPF1 = OptimalPowerFlowAnalysis(topologyNodeIRI_10Bus, AgentIRI, "2017-01-31", slackBusNodeIRI, loadAllocatorName, EBusModelVariableInitialisationMethodName, ELineInitialisationMethodName,
        CarbonTax, piecewiseOrPolynomial, pointsOfPiecewiseOrcostFuncOrder, baseMVA, withRetrofit, retrofitGenerator, retrofitGenerationFuelOrTechType, newGeneratorType, discountRate, bankRate, 
        projectLifeSpan, SMRCapitalCost, MonetaryValuePerHumanLife, NeighbourhoodRadiusForSMRUnitOf1MW, ProbabilityOfReactorFailure, SMRCapability, maxmumSMRUnitAtOneSite, demandCapacityRatio, DiscommissioningCostEstimatedLevel,
        shutNonRetrofittedGenerator, queryEndpointLabel, geospatialQueryEndpointLabel, updateEndPointURL)
    
    testOPF1.retrofitGeneratorInstanceFinder()
    testOPF1.ModelPythonObjectInputInitialiser()
    testOPF1.OPFModelInputFormatter()
    testOPF1.OptimalPowerFlowAnalysisSimulation()
    testOPF1.ModelOutputFormatter()
    ## testOPF1.ModelPythonObjectOntologiser()

    def powerPlantgeoJSONCreator(retrofitResults, class_label): 
        geojson_file = """
        {
            "type": "FeatureCollection",
            "features": ["""
        # iterating over features (rows in results array)
        for i in range(len(retrofitResults)):
            # creating point feature 
            feature = """{
                "type": "Feature",
                "properties": {
                "Original Fuel Type": "%s",
                "Original Design Capacity": "%s",
                "Output": "%s",
                "Single SMR Design Capacity": "470MW",
                "Number of SMR Units": "%s",
                "Total SMR Output": "%s",
                "marker-color": "%s",
                "marker-size": "small",
                "marker-symbol": "marker"
                },
                "geometry": {
                "type": "Point",
                "coordinates": [
                    %s,
                    %s
                ]
                }
            },"""%(retrofitResults[i][5], retrofitResults[i][3], retrofitResults[i][2], retrofitResults[i][6], retrofitResults[i][0], retrofitResults[i][1], retrofitResults[i][4][1], retrofitResults[i][4][0])
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
        geojson_written = open(class_label+'.geojson','w')
        geojson_written.write(geojson_file)
        geojson_written.close() 
        print('---GeoJSON written successfully---')
        return

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

    retrofitResults = [] 
    totalCapacityBeingReplaced = 0
    for smrname in testOPF1.GeneratorToBeRetrofittedObjectList:
        i = testOPF1.GeneratorToBeRetrofittedObjectList.index(smrname)
        if "#" in testOPF1.siteToBeReplaced[i]['fuelOrGenType']:
            testOPF1.siteToBeReplaced[i]['fuelOrGenType'] = testOPF1.siteToBeReplaced[i]['fuelOrGenType'].split('#')[1]
        elif "/" in testOPF1.siteNotSelected:
            testOPF1.siteToBeReplaced[i]['fuelOrGenType'] = testOPF1.siteToBeReplaced[i]['fuelOrGenType'].split('/')[-1]
        existRetroPairResult = [ round((testOPF1.ObjectSet[smrname].PG_OUTPUT),2), "#ed2400", 'Replaced', testOPF1.siteToBeReplaced[i]['Capacity'], testOPF1.siteToBeReplaced[i]['LatLon'], testOPF1.siteToBeReplaced[i]['fuelOrGenType'], testOPF1.siteToBeReplaced[i]['numberOfSMR']]
        retrofitResults.append(existRetroPairResult)
        totalCapacityBeingReplaced += float(testOPF1.siteToBeReplaced[i]['Capacity'])
    print("Total capacity being replaced: ", totalCapacityBeingReplaced)
    # print(len(retrofitResults))
    # print(len(testOPF1.GeneratorToBeRetrofittedObjectList))
    # print(len(testOPF1.siteToBeReplaced))
    ##TODO: how to deal with the visulisation of the non-chosen generators
    if testOPF1.shutNonRetrofittedGenerator is True:
        for shutdown in testOPF1.siteNotSelected:
            if "#" in shutdown['fuelOrGenType']:
                shutdown['fuelOrGenType'] = shutdown['fuelOrGenType'].split('#')[1]
            elif "/" in testOPF1.siteNotSelected:
                shutdown['fuelOrGenType'] = shutdown['fuelOrGenType'].split('/')[-1]
            existRetroPairResult = [ 'N/A', "#99A3A4", 'Shut Down', shutdown['Capacity'], shutdown['LatLon'], shutdown['fuelOrGenType'], 'N/A']
            retrofitResults.append(existRetroPairResult)
    else:
        i = 0
        for stillOpen in testOPF1.siteNotSelected:
            for gen in testOPF1.GeneratorObjectList:
                if testOPF1.ObjectSet[gen].generatorNodeIRI == stillOpen['PowerGenerator']:
                    if "#" in stillOpen['fuelOrGenType']:
                        stillOpen['fuelOrGenType'] = stillOpen['fuelOrGenType'].split('#')[1]
                    elif "/" in testOPF1.siteNotSelected:
                        stillOpen['fuelOrGenType'] = stillOpen['fuelOrGenType'].split('/')[-1]
                    existRetroPairResult = [ 'N/A', "#99A3A4", round((testOPF1.ObjectSet[gen].PG_OUTPUT),2), stillOpen['Capacity'], stillOpen['LatLon'], stillOpen['fuelOrGenType'], 'N/A']
                    retrofitResults.append(existRetroPairResult)
                    testOPF1.GeneratorObjectList.remove(gen)
                    i += 1
                    break
    print("still open but not be replaced", i)
    # print(len(retrofitResults))
    # print(len(testOPF1.siteNotSelected))

    # for gen in testOPF1.GeneratorObjectList:
    #     i = testOPF1.GeneratorObjectList.index(gen)
    #     existRetroPairResult = [ 'N/A', "#1B2631", round((testOPF1.ObjectSet[gen].PG_OUTPUT),2), testOPF1.generatorNodeList[i][6], testOPF1.generatorNodeList[i][8], testOPF1.generatorNodeList[i][7], 'N/A']
    #     retrofitResults.append(existRetroPairResult)

    print(len(retrofitResults))

    powerPlantgeoJSONCreator(retrofitResults, 'NewSelector_'+ str(CarbonTax) + "ifShutDown" + str(shutNonRetrofittedGenerator))