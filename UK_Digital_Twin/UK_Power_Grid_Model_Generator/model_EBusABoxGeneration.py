##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 21 April 2022        #
##########################################

"""This module is designed to generate and update the A-box of UK power grid model_EBus."""

import os
import owlready2
from rdflib.extras.infixowl import OWL_NS
from rdflib import Graph, URIRef, Literal, ConjunctiveGraph
from rdflib.namespace import RDF, RDFS
from rdflib.plugins.sleepycat import Sleepycat
from rdflib.store import NO_STORE, VALID_STORE
import sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from UK_Digital_Twin_Package import UKDigitalTwinTBox as T_BOX
from UK_Digital_Twin_Package import UKPowerGridModel as UK_PG
from UK_Digital_Twin_Package import UKPowerPlant as UKpp
from UK_Digital_Twin_Package import UKPowerGridTopology as UK_Topo
from UK_Digital_Twin_Package import UKEnergyConsumption as UKec
from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs, selectStoragePath, readFile, specifyValidFilePath
import UK_Power_Grid_Model_Generator.SPARQLQueryUsedInModel as query_model
import UK_Power_Grid_Model_Generator.initialiseEBusModelVariable as InitialiseEbus
from UK_Power_Grid_Topology_Generator.topologyABoxGeneration import checkaggregatedBus
from UK_Power_Grid_Model_Generator.AddModelVariables import AddModelVariable
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel as endpointList
from UK_Digital_Twin_Package.GraphStore import LocalGraphStore
from UK_Digital_Twin_Package import demandLoadAllocator as DLA
from UK_Digital_Twin_Package.derivationInterface import createMarkUpDerivation
from pyasyncagent.kg_operations import sparql_client # the import of this agent will need a parckage name werkzeug, install `pip install Werkzeug==2.0.2`, otherwise it will report the error message
import uuid
# from UK_Digital_Twin_Package import TopologicalInformationProperty as TopoInfo 


from py4jps.resources import JpsBaseLib

"""Notation used in URI construction"""
HASH = '#'
SLASH = '/'
UNDERSCORE = '_'
OWL = '.owl'

"""Create an instance of Class UKDigitalTwin"""
dt = UKDT.UKDigitalTwin()

"""Create an object of Class UKDigitalTwinTBox"""
t_box = T_BOX.UKDigitalTwinTBox()

"""Create an object of Class UKPowerPlantDataProperty"""
ukpp = UKpp.UKPowerPlant()

"""Create an object of Class UKEnergyConsumption"""
ukec = UKec.UKEnergyConsumption()

"""Blazegraph UK digital tiwn"""
endpoint_label = endpointList.ukdigitaltwin['lable'] # remote query
endpoint_iri = endpointList.ukdigitaltwin['queryendpoint_iri'] # federated query
ONS_JSON =  endpointList.ONS['endpoint_iri']

"""Sleepycat storage path"""
userSpecifiePath_Sleepycat = None # user specified path
userSpecified_Sleepycat = False # storage mode: False: default, True: user specified

"""T-Box URI"""
ontocape_upper_level_system     = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
ontocape_derived_SI_units       = owlready2.get_ontology(t_box.ontocape_derived_SI_units).load()
ontocape_mathematical_model     = owlready2.get_ontology(t_box.ontocape_mathematical_model).load()
ontopowsys_PowerSystemModel     = owlready2.get_ontology(t_box.ontopowsys_PowerSystemModel).load()

"""User specified folder path"""
filepath = None
userSpecified = False

"""EBus Conjunctive graph identifier"""
model_EBus_cg_id = "http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EBus"

### Functions ### 
"""Main function: create the named graph Model_EBus and their sub graphs each EBus"""

def createModel_EBus(topologyNodeIRI, powerSystemModelIRI, AgentIRI, slackBusNodeIRI, storeClient, startTime_of_EnergyConsumption, loadAllocatorName, EBusModelVariableInitialisationMethodName, OWLFileStoragePath, updateLocalOWLFile = True, storeType = "default"):
    ## Query the bus node IRI and GPS of the given topology entity
    res_queryBusTopologicalInformation, numOfBus = list(query_model.queryBusTopologicalInformation(topologyNodeIRI, endpoint_label))
    
    ## Initialise the model and topology python instance
    uk_ebus_model = UK_PG.UKEbusModel(numOfBus)
    uk_topo = UK_Topo.UKPowerGridTopology(numOfBus) 
    
    ## set up the storage path and Sleepycat
    defaultStoredPath = uk_ebus_model.StoreGeneratedOWLs
    topoAndConsumpPath_Sleepycat = uk_topo.SleepycatStoragePath
    defaultPath_Sleepycat = uk_ebus_model.SleepycatStoragePath
    filepath = specifyValidFilePath(defaultStoredPath, OWLFileStoragePath, updateLocalOWLFile)
    if filepath == None:
        return
    store = LocalGraphStore(storeType)
    global userSpecifiePath_Sleepycat, userSpecified_Sleepycat 
    if isinstance(store, Sleepycat): 
        print('The store is Sleepycat')
        cg_model_EBus = ConjunctiveGraph(store=store, identifier = model_EBus_cg_id)
        if userSpecifiePath_Sleepycat == None and userSpecified_Sleepycat:
            print('****Needs user to specify a Sleepycat storage path****')
            userSpecifiePath_Sleepycat = selectStoragePath()
            userSpecifiePath_Sleepycat_ = userSpecifiePath_Sleepycat + '\\' + 'ConjunctiveGraph_UKPowerGrid_EBus'
            sl = cg_model_EBus.open(userSpecifiePath_Sleepycat_, create = False) 
            
        elif os.path.exists(defaultPath_Sleepycat) and not userSpecified_Sleepycat:
            print('****Non user specified Sleepycat storage path, will use the default storage path****')
            sl = cg_model_EBus.open(defaultPath_Sleepycat, create = False)        
        else:
            print('****Create Sleepycat store with its default path****')
            sl = cg_model_EBus.open(defaultPath_Sleepycat, create = True)   
        
        if sl == NO_STORE:
        # There is no underlying Sleepycat infrastructure, so create it
            cg_model_EBus.open(defaultPath_Sleepycat, create = True)
        else:
            assert sl == VALID_STORE, "The underlying sleepycat store is corrupt"
    else:
        topoAndConsumpPath_Sleepycat = None
        print('Store is IOMemery')
     
    ## create an instance of class demandLoadAllocator
    dla = DLA.demandLoadAllocator()
    ## get the load allocation method via getattr function 
    allocator = getattr(dla, loadAllocatorName)
    ## pass the arrguments to the cluster method
    EBus_Load_List, aggregatedBusFlag = allocator(res_queryBusTopologicalInformation, startTime_of_EnergyConsumption, numOfBus) # EBus_Load_List[0]: EquipmentConnection_EBus, EBus_Load_List[1]: v_TotalELecConsumption 
    
    ## check if the allocator method is applicable
    while EBus_Load_List == None:
        loadAllocatorName = str(input('The current allocator is not applicable. Please choose another allocator: '))
        # get the load allocation method via getattr function 
        allocator = getattr(dla, loadAllocatorName)
        # pass the arrguments to the cluster method
        EBus_Load_List, aggregatedBusFlag = allocator(res_queryBusTopologicalInformation, startTime_of_EnergyConsumption, numOfBus) # EBus_Load_List[0]: EquipmentConnection_EBus, EBus_Load_List[1]: v_TotalELecConsumption 
             
    ##The sum up of the load of the aggegated bus is done in the loadAllocatorName
    if aggregatedBusFlag == True:
        EBus_Load_List = addUpConsumptionForAggregatedBus(EBus_Load_List) # sum up the demand of an AggregatedBus
    
    
    print('################START createModel_EBus#################')
    ontologyIRI = dt.baseURL + SLASH + dt.topNode + SLASH + str(uuid.uuid4())
    namespace = UK_PG.ontopowsys_namespace  
    ## ElectricalBusModel node IRI 
    ElectricalBusModelIRI = UK_PG.ontopowsys_namespace + uk_ebus_model.ModelEBusKey + str(uuid.uuid4()) # root node
    ## create a named graph
    g = Graph(store = store, identifier = URIRef(ontologyIRI))
    ## Import T-boxes
    g.set((g.identifier, RDF.type, OWL_NS['Ontology']))
    g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_mathematical_model)))
    g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_upper_level_system)))  
    g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontopowsys_PowerSystemModel))) 
    g.set((g.identifier, RDFS.comment, Literal('This ontology represents mathematical model of the electricity bus of the UK energy system.'))) 
    ## Link topologyNodeIRI with PowerSystemModel and ElectricalBusModelIRI
    g.add((URIRef(powerSystemModelIRI), URIRef(ontopowsys_PowerSystemModel.hasModelingPrinciple.iri), URIRef(topologyNodeIRI)))
    g.add((URIRef(ElectricalBusModelIRI), URIRef(ontocape_upper_level_system.isExclusivelySubsystemOf.iri), URIRef(powerSystemModelIRI)))
    g.add((URIRef(ElectricalBusModelIRI), RDF.type, URIRef(t_box.ontopowsys_PowerSystemModel + 'ElectricalBusModel')))
    g.add((URIRef(powerSystemModelIRI), RDF.type, URIRef(ontopowsys_PowerSystemModel.PowerSystemModel.iri)))
    
    ##The bus index number used in the model input
    BusNumber = 0
    
    for ebus in EBus_Load_List:         
    # if EBus_Load_List[0] != None: # test
    #     ebus = EBus_Load_List[0] # test  
        ## IRI construction
        BusNodeIRI = ebus["BusNodeIRI"] # topology bus node IRI
        
        ## link the ElectricalBusModelIRI with topology BusNodeIRI
        g.add((URIRef(BusNodeIRI), URIRef(ontocape_upper_level_system.isModeledBy.iri), URIRef(ElectricalBusModelIRI)))
        
        ###add EBus model parametor###
        # create an instance of class initialiseEBusModelVariable
        initialiseEbus = InitialiseEbus.initialiseEBusModelVariable()
        # get the initialiser via getattr function 
        initialiser = getattr(initialiseEbus, EBusModelVariableInitialisationMethodName)
        # pass the arrguments to the initialiser method
        uk_ebus_model = initialiser(uk_ebus_model, ebus, BusNumber, slackBusNodeIRI) 
        print('the bus type is ',uk_ebus_model.TYPE)
        
        ## Add model variables attributes 
        ModelInputVariableIRIList = []
                
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, uk_ebus_model.BUSNUMKey, int(uk_ebus_model.BUS), None, ontopowsys_PowerSystemModel.BusNumber.iri)
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, uk_ebus_model.BUSTYPEKey, int(uk_ebus_model.TYPE), None, ontopowsys_PowerSystemModel.BusType.iri)
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, uk_ebus_model.PD_INPUTKey, float(uk_ebus_model.PD_INPUT), ontocape_derived_SI_units.MW.iri, \
                             ontopowsys_PowerSystemModel.PdBus.iri)    
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, uk_ebus_model.GD_INPUTKey, float(uk_ebus_model.GD_INPUT), ontocape_derived_SI_units.Mvar.iri, \
                                 ontopowsys_PowerSystemModel.GdBus.iri) 
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, uk_ebus_model.GSKey, int(uk_ebus_model.GS), None, ontopowsys_PowerSystemModel.Gs.iri)
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, uk_ebus_model.BSKey, int(uk_ebus_model.BS), None, ontopowsys_PowerSystemModel.Bs.iri)   
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, uk_ebus_model.AREAKey, int(uk_ebus_model.AREA), None, ontopowsys_PowerSystemModel.Area.iri)
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, uk_ebus_model.VM_INPUTKey, float(uk_ebus_model.VM_INPUT), ontocape_derived_SI_units.kV.iri, \
                                 ontopowsys_PowerSystemModel.Vm.iri) 
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, uk_ebus_model.VA_INPUTKey, float(uk_ebus_model.VA_INPUT), ontocape_derived_SI_units.degree.iri, \
                                 ontopowsys_PowerSystemModel.Va.iri)  
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, uk_ebus_model.BASEKVKey, int(uk_ebus_model.BASEKV), ontocape_derived_SI_units.kV.iri, \
                                 ontopowsys_PowerSystemModel.baseKV.iri)    
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, uk_ebus_model.ZONEKey, int(uk_ebus_model.ZONE), None, ontopowsys_PowerSystemModel.Zone.iri)
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, uk_ebus_model.VMAXKey, float(uk_ebus_model.VMAX), ontocape_derived_SI_units.kV.iri, \
                                 ontopowsys_PowerSystemModel.VmMax.iri) 
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, uk_ebus_model.VMINKey, float(uk_ebus_model.VMIN), ontocape_derived_SI_units.kV.iri, \
                                 ontopowsys_PowerSystemModel.VmMin.iri)
        ModelInputVariableIRIList.append(varNode)
        
        print(g.serialize(format="pretty-xml").decode("utf-8"))
         
        ## add deviation  
        createMarkUpDerivation(ModelInputVariableIRIList, AgentIRI, BusNodeIRI, storeClient, False)
        
        BusNumber += 1 
    
    ## generate/update OWL files
    if updateLocalOWLFile == True:    
        # Store/update the generated owl files      
        if filepath[-2:] != "\\": 
            filepath_ = filepath + '\\' + 'Model_' + str(numOfBus) + '_Bus_Grid' + OWL
        else:
            filepath_ = filepath + 'Model_' + str(numOfBus) + '_Bus_Grid' + OWL
        storeGeneratedOWLs(g, filepath_)
    
    ## update the graph to endpoint
    sparql_client.uploadOntology(filepath_)
    
    if isinstance(store, Sleepycat):  
        cg_model_EBus.close()       
    return

# The demanding of an AggregatedBus is the sum of their regional consumption (elec demanding)
def addUpConsumptionForAggregatedBus(EBus_Load_List):
    bus_node  = []
    for ebus in EBus_Load_List:
        if ebus['BusNodeIRI'] in bus_node:
            counter_1 = bus_node.index(ebus['BusNodeIRI'])
            counter_2 = EBus_Load_List.index(ebus) 
            if counter_2 > counter_1:
                EBus_Load_List[counter_1]['v_TotalELecConsumption'] += EBus_Load_List[counter_2]['v_TotalELecConsumption']
                EBus_Load_List[counter_1]['v_TotalELecConsumption'] = round(float(EBus_Load_List[counter_1]['v_TotalELecConsumption']), 2)
                del EBus_Load_List[counter_2]
            else:
                print('counter_2 should be larger than counter_1')
                return None
        else:
            bus_node.append(ebus['BusNodeIRI'])
    return EBus_Load_List

if __name__ == '__main__':  
    # import os, sys
    # BASE = os.path.dirname(os.path.abspath(__file__))
    # sys.path.insert(0, BASE)
    # from py4jps.resources import JpsBaseLib

    # # jpsBaseLib resource gateway
    # # you may also wish to pass any **JGkwargs
    # jpsBaseLibGW = JpsBaseLib()

    # # you may also wish to pass any **LGkwargs
    # jpsBaseLibGW.launchGateway()
    # jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    # jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")
    # endPointURL = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test3/sparql"
    # storeClient = jpsBaseLib_view.RemoteStoreClient(endPointURL, endPointURL)
    
    jpsBaseLibGW = JpsBaseLib()
    jpsBaseLibGW.launchGateway()

    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

    endPointURL = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test3/sparql"
    storeClient = jpsBaseLib_view.RemoteStoreClient(endPointURL, endPointURL)

    topologyNodeIRI = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerGridTopology_10fe8504-f3bb-403c-9363-34b258d59711" 
    powerSystemModelIRI = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerSystemModel_22fe8504-f3bb-403c-9363-34b258d59712"
    AgentIRI = "http://www.example.com/triplestore/agents/Service__XXXAgent#Service"
    slackBusNodeIRI = "http://www.theworldavatar.com/kb/ontopowsys/BusNode_67e9e639-599c-4e6a-8941-ea939adeef39"
    
    createModel_EBus(topologyNodeIRI, powerSystemModelIRI, AgentIRI, slackBusNodeIRI, storeClient, "2017-01-31", "regionalDemandLoad", "defaultInitialisation", None, True, 'default')  
    
    
    # createModel_EBus('default', False, 2019, 29, 99, "2017-01-31", "closestDemandLoad", "preSpecified", None, True)  
    #createModel_EBus('default', False, 2019, 29, 99, "2017-01-31", "regionalDemandLoad", "preSpecified", None, True)           
    print('*****************Terminated*****************')