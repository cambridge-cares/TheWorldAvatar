##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 21 June 2022         #
##########################################

"""
Optimal Flow Analysis
"""
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
import UK_Power_Grid_Model_Generator.initialiseEBusModelVariable as InitialiseEbus
from pypower.api import ppoption, runopf, isload
import UK_Digital_Twin_Package.PYPOWER.pypower.runpf as pf
from numpy import r_, c_, ix_, zeros, pi, ones, exp, union1d, array, linalg, where, logical_or, arange, \
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


dt = UKDT.UKDigitalTwin()
## set up the derivationInstanceBaseURL
derivationInstanceBaseURL = dt.baseURL + '/' + dt.topNode + '/'

class OptimalPowerFlowAnalysis:
    
    def __init__(self, \
 
        topologyNodeIRI:str, agentIRI:str, \
        startTime_of_EnergyConsumption:str, \
        slackBusNodeIRI:str, loadAllocatorName:str, EBusModelVariableInitialisationMethodName:str, \

        ELineInitialisationMethodName:str, \

        CarbonTax:float, piecewiseOrPolynomial:int, pointsOfPiecewiseOrcostFuncOrder:int, baseMVA: float, \

        retrofitGenerator: list, retrofitGenerationTechType: list, \

        queryEndpointLabel:str, \
        endPointURL:str, endPointUser:str = None, endPointPassWord:str = None, \
        powerPlantOWLFileLocalPath = None, updateLocalPowerPlantOWLFileFlag:bool = True):
       
        ## 1. newly created
        ## create the power system model node IRI
        self.powerSystemModelIRI = UK_PG.ontopowsys_namespace + UK_PG.powerSystemModelKey + str(uuid.uuid4())
        ## create the timeStamp, e.x. 2022-06-15T16:24:29.371941+00:00
        self.timeStamp = datetime.now(pytz.utc).isoformat()
        ## query the number of the bus under the topology node IRI, and the bus node IRI, branch node IRI and generator node IRI
        self.numOfBus, self.busNodeList = query_model.queryBusTopologicalInformation(topologyNodeIRI, queryEndpointLabel) ## ?BusNodeIRI ?BusLatLon
        self.branchNodeList, self.branchVoltageLevel = query_model.queryELineTopologicalInformation(topologyNodeIRI, queryEndpointLabel) ## ?ELineNode ?From_Bus ?To_Bus ?Value_Length_ELine ?Num_OHL_400 or 275 
        self.generatorNodeList = query_model.queryEGenInfo(topologyNodeIRI, queryEndpointLabel) ## ?PowerGenerator ?FixedMO ?VarMO ?FuelCost ?CO2EmissionFactor ?Bus ?Capacity ?PrimaryFuel
        
        ## 2. passing arguments
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
        self.OrderedBusNodeIRIList = []
        self.ELineInitialisationMethodName = str(ELineInitialisationMethodName)
       
        ## 3. specify the OPF model factors
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

        ## 4. specify the query/update endpoint information
        self.queryEndpointLabel = queryEndpointLabel
        self.endPointURL = endPointURL
        self.endPointUser = endPointUser
        self.endPointPassWord = endPointPassWord
        ## specify the local storage path (to be deleted)
        self.powerPlantOWLFileLocalPath = powerPlantOWLFileLocalPath
        self.updateLocalPowerPlantOWLFileFlag = updateLocalPowerPlantOWLFileFlag       
        
        ## 5. JVM module view and use it to import the required java classes
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
        
        ## 6. Identify the retrofitting generators
        self.retrofitGenerator = retrofitGenerator # GeneratorIRI, location, capacity
        self.retrofitGenerationTechType = retrofitGenerationTechType
        
        self.BusObjectList: list = []
        self.BranchObjectList: list = []
        self.GeneratorObjectList: list = []

    def retrofitGeneratorInstanceFinder(self):
        if len(self.retrofitGenerator) == 0 and len(self.retrofitGeneratorType) == 0:  
            print("***As there is not specific generator assigned to be retrofitted by SMR, all generators located in GB will be treated as the potential sites.***")
            retrofitList = queryOPFInput.queryGeneratorToBeRetrofitted_AllPowerPlant(self.topologyNodeIRI, self.queryEndpointLabel) ## PowerGenerator, Bus, Capacity
        elif not len(self.retrofitGenerator) == 0:
            for iri in self.retrofitGenerator:
                parse(iri, rule='IRI')
            retrofitList = queryOPFInput.queryGeneratorToBeRetrofitted_SelectedGenerator(self.retrofitGenerator, self.queryEndpointLabel) 
        elif not len(self.retrofitGenerationTechType) == 0:
            for iri in self.retrofitGenerationTechType:
                parse(iri, rule='IRI')
            retrofitList = queryOPFInput.queryGeneratorToBeRetrofitted_SelectedGenerationTechnologyType(self.retrofitGenerationTechType, self.topologyNodeIRI, self.queryEndpointLabel)
        self.retrofitList = retrofitList  
        return 

    """This method is called to initialize the model entities objects: model input"""
    def ModelObjectInputInitialiser(self): 
        ##-- create model bus, branch and generator objects dynamically --##
        ObjectSet = locals()        
        ### Initialisation of the Bus Model Entities ###
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

        
        ### Initialisation of the Branch Model Entities ###
        for eline in self.branchNodeList:
            objectName = UK_PG.UKElineModel.ELineKey + str(self.branchNodeList.index(eline)) ## bus model python object name
            uk_eline_model = UK_PG.UKElineModel(int(self.numOfBus), str(eline["ELineNode"]), self.ELineInitialisationMethodName)
            ###1. create an instance of the BranchPropertyInitialisation class and get the initialiser method by applying the 'getattr' function 
            initialisation = BPI.BranchPropertyInitialisation()
            initialiser = getattr(initialisation, self.ELineInitialisationMethodName)
            ###2. execute the initialiser with the branch model instance as the function argument 
            # ## TODO: when initialise the 29-bus model, please check if ELineNodeIRI is the right node to use
            ObjectSet[objectName] = initialiser(eline['ELineNode'], uk_eline_model, eline, self.branchVoltageLevel, self.OrderedBusNodeIRIList, self.queryEndpointLabel) 
            self.BranchObjectList.append(objectName)
            
        ### Initialisation of the Generator Model Entities ###
        capa_demand_ratio = model_EGenABoxGeneration.demandAndCapacityRatioCalculator(self.generatorNodeList, self.topologyNodeIRI, self.startTime_of_EnergyConsumption)
        for egen in self.generatorNodeList:
            objectName = UK_PG.UKEGenModel.EGenKey + str(self.branchNodeList.index(egen)) ## bus model python object name
            # uk_egen_model = UK_PG.UKEGenModel(int(self.numOfBus), str(egen[0]))
            uk_egen_OPF_model = UK_PG.UKEGenModel_CostFunc(int(self.numOfBus), str(egen[0]), self.CarbonTax, self.piecewiseOrPolynomial, self.pointsOfPiecewiseOrcostFuncOrder)
            uk_egen_OPF_model = costFuncPara(uk_egen_OPF_model, egen)
            ###add EGen model parametor###
            ObjectSet[objectName] = model_EGenABoxGeneration.initialiseEGenModelVar(uk_egen_OPF_model, egen, self.OrderedBusNodeIRIList, capa_demand_ratio)
            self.GeneratorObjectList.append(objectName)
        
        #TODO: initialise the to-be retrofitted generator objects, need to record the original gen node IRI

        for egen in self.retrofitList:
            objectName = UK_PG.UKEGenModel.EGenKey + str(self.branchNodeList.index(egen)) ## bus model python object name
            # uk_egen_model = UK_PG.UKEGenModel(int(self.numOfBus), str(egen[0]))
            uk_egen_OPF_model = UK_PG.UKEGenModel_CostFunc(int(self.numOfBus), str(egen[0]), self.CarbonTax, self.piecewiseOrPolynomial, self.pointsOfPiecewiseOrcostFuncOrder)
            uk_egen_OPF_model = costFuncPara(uk_egen_OPF_model, egen)
            ###add EGen model parametor###
            ObjectSet[objectName] = model_EGenABoxGeneration.initialiseEGenModelVar(uk_egen_OPF_model, egen, self.OrderedBusNodeIRIList, capa_demand_ratio)
            self.GeneratorObjectList.append(objectName)


        self.ObjectSet = ObjectSet

        return     
    
        
    def ModelInputFormatter(self):
        """
        The ModelInputFormatter is used to created the pypower OPF model input from the model objects which are created from the F{ModelObjectInputInitialiser}.
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
        ppc["gen"] = numpy.zeros((len(self.GeneratorObjectList), len(UK_PG.UKEGenModel.INPUT_VARIABLE_KEYS)), dtype = float)
        index_gen  = 0
        while index_gen < len(self.GeneratorObjectList):
            for key in UK_PG.UKEGenModel.INPUT_VARIABLE_KEYS:
                index = int(UK_PG.UKEGenModel.INPUT_VARIABLE[key])
                ppc["gen"][index_gen][index] = getattr(self.ObjectSet.get(UK_PG.UKEGenModel.EGenKey + str(index_gen)), key)
            index_gen += 1

        ## generator COST data
        # MODEL, STARTUP, SHUTDOWN, NCOST, COST[a, b]
        columnNum = len(UK_PG.UKEGenModel_CostFunc.INPUT_VARIABLE_KEYS) + self.pointsOfPiecewiseOrcostFuncOrder -1
        ppc["gencost"] = numpy.zeros((len(self.GeneratorObjectList), columnNum), dtype = float)
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
        self.ppopt = ppoption(OUT_ALL = 1) 
        self.results = runopf(self.ppc, self.ppopt)
        self.totalCost = self.results["cost"]

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
        generatorPostResult = numpy.zeros((len(self.GeneratorObjectList), len(UK_PG.UKEGenModel.OUTPUT_VARIABLE_KEYS)), dtype = float) 
        for i in range(len(self.GeneratorObjectList)):
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
        return 
         
    
    


    def ModelResultUpdater(self):
         
         
         
         
        return 
         
         

if __name__ == '__main__':        
    test_PowerFlowAnalysis_1 = powerFlowAnalysis(1,1,1, 100, True)
    busList = [{'BUS': '1', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '4305.753', 'VMIN': '0.95', 'TYPE': '3'}, {'BUS': '2', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '2690.713', 'VMIN': '0.95', 'TYPE': '2'}, {'BUS': '3', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '4349.411', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '4', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '2997.804', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '5', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '2354.482', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '6', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '2730.946', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '7', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '3493.016', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '8', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '3876.755', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '9', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '1696.433', 'VMIN': '0.95', 'TYPE': '1'}, {'BUS': '10', 'BASEKV': '400', 'ZONE': '1', 'VMAX': '1.05', 'GD_INPUT': '0.0', 'VM_INPUT': '1.0', 'VA_INPUT': '0.0', 'GS': '0', 'BS': '0', 'AREA': '1', 'PD_INPUT': '2756.565', 'VMIN': '0.95', 'TYPE': '2'}]
    branchList = [{'TOBUS': '2', 'B': '3.4347502328055', 'FROMBUS': '1', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '8400.792858', 'R': '0.0019538132483099997', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.0188834047633', 'ANGMAX': '360.0'}, {'TOBUS': '3', 'B': '1.887069984274', 'FROMBUS': '1', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '11201.057144', 'R': '6.038073872325E-4', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.005835736502474999', 'ANGMAX': '360.0'}, {'TOBUS': '6', 'B': '1.539446743787', 'FROMBUS': '2', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '5600.528572', 'R': '0.001970312333715', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.019042866732450002', 'ANGMAX': '360.0'}, {'TOBUS': '9', 'B': '1.3219533136480002', 'FROMBUS': '2', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '5410.911345', 'R': '9.259004261394184E-4', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.01010836111654295', 'ANGMAX': '360.0'}, {'TOBUS': '4', 'B': '2.9436182372009996', 'FROMBUS': '3', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '8400.792858', 'R': '0.00167443916442', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.016183282880599996', 'ANGMAX': '360.0'}, {'TOBUS': '5', 'B': '2.6635415720849998', 'FROMBUS': '3', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '8400.792858', 'R': '0.0015151211756999998', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.014643490850999998', 'ANGMAX': '360.0'}, {'TOBUS': '5', 'B': '2.0370593803409998', 'FROMBUS': '4', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '5600.528572', 'R': '0.002607198487245', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.02519830612035', 'ANGMAX': '360.0'}, {'TOBUS': '6', 'B': '2.0515595937679', 'FROMBUS': '5', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '8211.175631', 'R': '7.04271614929686E-4', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.007295864343325824', 'ANGMAX': '360.0'}, {'TOBUS': '8', 'B': '3.27753321567', 'FROMBUS': '5', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '11201.057144', 'R': '0.0010487150895375', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.010135723838625', 'ANGMAX': '360.0'}, {'TOBUS': '7', 'B': '2.1010674006375', 'FROMBUS': '6', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '8400.792858', 'R': '0.00119516501775', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.011551147382500002', 'ANGMAX': '360.0'}, {'TOBUS': '9', 'B': '1.7635110489349999', 'FROMBUS': '6', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '5600.528572', 'R': '0.0022570885185749998', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.02181452916225', 'ANGMAX': '360.0'}, {'TOBUS': '8', 'B': '3.7299925384703996', 'FROMBUS': '7', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '10821.82269', 'R': '6.531247444995235E-4', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.007130378802263265', 'ANGMAX': '360.0'}, {'TOBUS': '10', 'B': '1.826004669239', 'FROMBUS': '7', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '2800.264286', 'R': '0.00934829226342', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.0903502864506', 'ANGMAX': '360.0'}, {'TOBUS': '10', 'B': '3.5598669251920003', 'FROMBUS': '8', 'RATIO': '1.0', 'ANGLE': '0.0', 'ANGMIN': '-360.0', 'RateA': '5600.528572', 'R': '0.00455621458644', 'RateC': '0.0', 'STATUS': '1', 'RateB': '0.0', 'X': '0.044035346929200005', 'ANGMAX': '360.0'}]
    test_PowerFlowAnalysis_1.ModelObjectCreator(busList, branchList)
    test_PowerFlowAnalysis_1.ModelInputFormatter()
    test_PowerFlowAnalysis_1.PowerFlowAnalysisSimulation()
    test_PowerFlowAnalysis_1.ModelOutputFormatter()
    
    print("*****This are EBus results*****")
    for attr in test_PowerFlowAnalysis_1.ObjectSet.get('EBus-7').__dir__():
        print(attr, getattr(test_PowerFlowAnalysis_1.ObjectSet.get('EBus-7'), attr))
    for attr in test_PowerFlowAnalysis_1.ObjectSet.get('EBus-8').__dir__():
        print(attr, getattr(test_PowerFlowAnalysis_1.ObjectSet.get('EBus-8'), attr))
    
    print("*****This are ELine results*****")
    for attr in test_PowerFlowAnalysis_1.ObjectSet.get('ELine-0').__dir__():
        print(attr, getattr(test_PowerFlowAnalysis_1.ObjectSet.get('ELine-0'), attr)) 
        
    print("*****This are EGen results*****")
    for attr in test_PowerFlowAnalysis_1.ObjectSet.get('EGen-1134').__dir__():
        print(attr, getattr(test_PowerFlowAnalysis_1.ObjectSet.get('EGen-1134'), attr)) 
        
    
    # res = test_PowerFlowAnalysis_1.PowerFlowAnalysisSimulation(busList, branchList)
    # print(res.get('Generator_1').PG_INPUT)
    # print(res.get('Bus_1'). __dir__()[0])
    # print(res.get('Bus_1').BUS)
    #print(res.get('Branch_0').R)
    
        
    