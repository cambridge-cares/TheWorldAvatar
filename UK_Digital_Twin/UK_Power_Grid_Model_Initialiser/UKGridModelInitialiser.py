##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 15 June 2022        #
##########################################

from logging import raiseExceptions
import os, sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.jpsSingletons import jpsBaseLibGW
from datetime import datetime
import pytz
from UK_Digital_Twin_Package import UKPowerGridModel as UK_PG
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
import uuid
import SPARQLQueryUsedInModelInitialiser as queryModelInitialiser
from UK_Power_Grid_Model_Generator import model_EBusABoxGeneration, model_EGenABoxGeneration, model_ELineABoxGeneration

dt = UKDT.UKDigitalTwin()
## set up the derivationInstanceBaseURL
derivationInstanceBaseURL = dt.baseURL + '/' + dt.topNode + '/'

class gridModelInitialiser(object):
      
    def __init__(
        self, topologyNodeIRI:str, startTime_of_EnergyConsumption:str, agentIRI:str, slackBusNodeIRI:str, queryEndpointLabel:str, \
        loadAllocatorName:str, EBusModelVariableInitialisationMethodName:str, \
        ELineInitialisationMethodName:str, \
        OPFOrPF:bool, CarbonTax:float, piecewiseOrPolynomial:int, pointsOfPiecewiseOrcostFuncOrder:int,\
        endPointURL:str, endPointUser:str = None, endPointPassWord:str = None, \
        powerPlantOWLFileLocalPath = None, updateLocalPowerPlantOWLFileFlag:bool = True):
        ## 1. newly created
        ## create the power system model node IRI
        self.powerSystemModelIRI = UK_PG.ontopowsys_namespace + UK_PG.powerSystemModelKey + str(uuid.uuid4())
        ## create the timeStamp, e.x. 2022-06-15T16:24:29.371941+00:00
        self.timeStamp = datetime.now(pytz.utc).isoformat()
        ## query the number of the bus under the topology node IRI
        self.numOfBus = queryModelInitialiser.queryNumberOfBus(topologyNodeIRI, queryEndpointLabel)

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
        self.EBbusInitialisationMethodName = EBusModelVariableInitialisationMethodName
        self.ELineInitialisationMethodName = ELineInitialisationMethodName
        ## specify whether the OPF or PF analysis, true for OPF analysis while false for PF analysis
        if type(OPFOrPF) is not bool:
            raiseExceptions("OPFOrPF has to be a bool number")
        else:
            self.OPFOrPF = OPFOrPF  
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
        ## specify the query/update endpoint information
        self.endPointURL = endPointURL
        self.endPointUser = endPointUser
        self.endPointPassWord = endPointPassWord
        ## specify the local storage path (to be deleted)
        self.powerPlantOWLFileLocalPath = powerPlantOWLFileLocalPath
        self.updateLocalPowerPlantOWLFileFlag = updateLocalPowerPlantOWLFileFlag       
        
        ## 3. JVM module view and use it to import the required java classes
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

     ## Ebus should be initialised at the first time, EGen and ELine should be initialized after Ebus
    def ModelInputInitialiser(self):
        
        self.OrderedBusNodeIRIList = model_EBusABoxGeneration.createModel_EBus(self.numOfBus, self.topologyNodeIRI, self.powerSystemModelIRI, self.timeStamp, \
            self.agentIRI, self.slackBusNodeIRI, self.derivationClient, self.endPointURL, self.startTime_of_EnergyConsumption, self.loadAllocatorName, \
                self.EBbusInitialisationMethodName, " ", self.powerPlantOWLFileLocalPath, self.updateLocalPowerPlantOWLFileFlag)
        
        model_ELineABoxGeneration.createModel_ELine(self.numOfBus, self.topologyNodeIRI, self.powerSystemModelIRI, self.timeStamp, \
            self.agentIRI, self.OrderedBusNodeIRIList, self.derivationClient, self.endPointURL, self.ELineInitialisationMethodName, " ", self.powerPlantOWLFileLocalPath, self.updateLocalPowerPlantOWLFileFlag)

        model_EGenABoxGeneration.createModel_EGen(self.numOfBus, self.topologyNodeIRI, self.powerSystemModelIRI, self.timeStamp, \
            self.agentIRI, self.OrderedBusNodeIRIList, self.derivationClient, self.endPointURL, self.startTime_of_EnergyConsumption, self.OPFOrPF, self.CarbonTax,\
                self.piecewiseOrPolynomial, self.pointsOfPiecewiseOrcostFuncOrder, " ", self.powerPlantOWLFileLocalPath, self.updateLocalPowerPlantOWLFileFlag)       
        return self
    
if __name__ == '__main__':
    topologyNodeIRI_10Bus = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerGridTopology_b22aaffa-fd51-4643-98a3-ff72ee04e21e" 
    powerSystemModelIRI = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerSystemModel_22fe8504-f3bb-403c-9363-34b258d59712"
    AgentIRI = "http://www.example.com/triplestore/agents/Service__XXXAgent#Service"
    slackBusNodeIRI = "http://www.theworldavatar.com/kb/ontopowsys/BusNode_1f3c4462-3472-4949-bffb-eae7d3135591"   
    queryEndpointLabel = "ukdigitaltwin_test2"
    updateEndPointURL = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test3/sparql"
    loadAllocatorName = "regionalDemandLoad"
    EBusModelVariableInitialisationMethodName= "defaultInitialisation"

    ELineInitialisationMethodName = "defaultBranchInitialiser"

    testModelInitialisier = gridModelInitialiser(
        topologyNodeIRI_10Bus, "2017-01-31", AgentIRI, slackBusNodeIRI, queryEndpointLabel, \
            loadAllocatorName, EBusModelVariableInitialisationMethodName, \
            ELineInitialisationMethodName, \
            True, 18, 2, 2,\
            updateEndPointURL)
    
    res = testModelInitialisier.ModelInputInitialiser()

    print(res.powerSystemModelIRI)
