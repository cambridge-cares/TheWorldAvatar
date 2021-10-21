##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 21 Oct 2021          #
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
from UK_Power_Grid_Topology_Generator.SPARQLQueriesUsedInTopologyABox import queryBusTopologicalInformation
from UK_Power_Grid_Model_Generator.AddModelVariables import AddModelVariable
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel as endpointList
from UK_Digital_Twin_Package.GraphStore import LocalGraphStore
from UK_Digital_Twin_Package import demandLoadAllocator as DLA


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

"""Create an object of Class UKEbusModel"""
# uk_ebus_model = UK_PG.UKEbusModel()

"""Create an object of Class UKPowerGridTopology"""
uk_topo = UK_Topo.UKPowerGridTopology()

"""Create an object of Class UKEnergyConsumption"""
ukec = UKec.UKEnergyConsumption()

"""Remote Endpoint lable and queryendpoint_iri"""
topology_federated_query_Endpoint = uk_topo.endpoint['queryendpoint_iri']
energyConsumption_federated_query_Endpoint = ukec.endpoint['queryendpoint_iri']

"""Blazegraph UK digital tiwn"""
endpoint_label = endpointList.ukdigitaltwin['lable'] # remote query
endpoint_iri = endpointList.ukdigitaltwin['queryendpoint_iri'] # federated query
ONS_JSON =  endpointList.ONS['endpoint_iri']

"""Sleepycat storage path"""
# defaultPath_Sleepycat = uk_ebus_model.SleepycatStoragePath
topoAndConsumpPath_Sleepycat = uk_topo.SleepycatStoragePath
userSpecifiePath_Sleepycat = None # user specified path
userSpecified_Sleepycat = False # storage mode: False: default, True: user specified

"""OWL file storage path"""
# defaultStoredPath = uk_ebus_model.StoreGeneratedOWLs # default path

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
def createModel_EBus(storeType, localQuery, version_of_DUKES, numOfBus, startTime_of_EnergyConsumption, loadAllocatorName, defaultInitialisation, OWLFileStoragePath, updateLocalOWLFile = True):
    uk_ebus_model = UK_PG.UKEbusModel(version_of_DUKES, numOfBus)
    defaultStoredPath = uk_ebus_model.StoreGeneratedOWLs
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
        
    # Query the bus loaction
    res_queryBusLocation = list(queryBusTopologicalInformation(numOfBus, topoAndConsumpPath_Sleepycat, localQuery, endpoint_label)) # this query reused the one for creating topology
    # Query the local electricity consumption for both region and local areas
    res_queryElectricityConsumption_Region = list(query_model.queryElectricityConsumption_Region(startTime_of_EnergyConsumption,topoAndConsumpPath_Sleepycat, localQuery, endpoint_label))
    res_queryElectricityConsumption_LocalArea = list(query_model.queryElectricityConsumption_LocalArea(startTime_of_EnergyConsumption, endpoint_iri, ONS_JSON))
    
    # create an instance of class demandLoadAllocator
    dla = DLA.demandLoadAllocator()
    # get the load allocation method via getattr function 
    allocator = getattr(dla, loadAllocatorName)
    # pass the arrguments to the cluster method
    EBus_Load_List = allocator(res_queryBusLocation, res_queryElectricityConsumption_Region, res_queryElectricityConsumption_LocalArea) # EBus_Load_List[0]: EquipmentConnection_EBus, EBus_Load_List[1]: TotalELecConsumption 
    
    EBus_Load_List = checkAggregatedBus(EBus_Load_List) # sum up the demand of an AggregatedBus
    
    if EBus_Load_List == None:
        raise Exception('EBus_Load_List is empty')
        
    print('################START createModel_EBus#################')
    for ebus in EBus_Load_List:         
    # if EBus_Load_List[0] != None: # test
    #     ebus = EBus_Load_List[0] # test           
        root_uri = ebus[0].split('#')[0]
        namespace = root_uri + HASH
        node_locator = ebus[0].split('#')[1]
        root_node = namespace + 'Model_' + node_locator
        father_node = UKDT.nodeURIGenerator(4, dt.powerGridModel, numOfBus, "EBus")
        
        # create a named graph
        g = Graph(store = store, identifier = URIRef(root_uri))
        # Import T-boxes
        g.set((g.identifier, RDF.type, OWL_NS['Ontology']))
        g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_mathematical_model)))
        g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_upper_level_system)))  
        g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontopowsys_PowerSystemModel))) 
        # Add root node type and the connection between root node and its father node   
        g.add((URIRef(root_node), URIRef(ontocape_upper_level_system.isExclusivelySubsystemOf.iri), URIRef(father_node)))
        g.add((URIRef(father_node), RDFS.label, Literal("UK_Electrical_Grid_" + str(numOfBus) + "_Bus_Model")))  
        g.add((URIRef(root_node), RDF.type, URIRef(ontocape_mathematical_model.Submodel.iri)))
        g.add((URIRef(root_node), RDF.type, URIRef(ontopowsys_PowerSystemModel.PowerFlowModelAgent.iri)))
        g.add((URIRef(root_node), RDF.type, URIRef(t_box.ontopowsys_PowerSystemModel + 'BusModel'))) # undefined T-box class, the sub-class of PowerFlowModelAgent
        g.add((URIRef(father_node), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(root_node)))
        # link with EBus node in topology
        g.add((URIRef(root_node), URIRef(ontocape_upper_level_system.models.iri), URIRef(ebus[0])))
        g.add((URIRef(ebus[0]), URIRef(ontocape_upper_level_system.isModeledBy.iri), URIRef(root_node)))
            
        ###add EBus model parametor###
        # uk_ebus_model_ = UK_PG.UKEbusModel(version_of_DUKES, numOfBus)
        uk_ebus_model_ = initialiseEBusModelVar(uk_ebus_model, ebus, defaultInitialisation) 
        print('the bus type is ',uk_ebus_model_.TYPE)
        
        if uk_ebus_model_ != None:
            pass
        else: 
            print ('uk_ebus_model_ is none')
            return None 
        
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_ebus_model_.BUSNUMKey, int(uk_ebus_model_.BUS), None, \
                                 ontopowsys_PowerSystemModel.BusNumber.iri, ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_ebus_model_.BUSTYPEKey, int(uk_ebus_model_.TYPE), None, \
                                 ontopowsys_PowerSystemModel.BusType.iri, ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_ebus_model_.PD_INPUTKey, float(uk_ebus_model_.PD_INPUT), ontocape_derived_SI_units.MW.iri, \
                                 ontopowsys_PowerSystemModel.PdBus.iri, ontocape_mathematical_model.InputVariable.iri)    
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_ebus_model_.GD_INPUTKey, float(uk_ebus_model_.GD_INPUT), ontocape_derived_SI_units.Mvar.iri, \
                                 ontopowsys_PowerSystemModel.GdBus.iri, ontocape_mathematical_model.InputVariable.iri)       
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_ebus_model_.GSKey, int(uk_ebus_model_.GS), None,\
                                 ontopowsys_PowerSystemModel.Gs.iri, ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_ebus_model_.BSKey, int(uk_ebus_model_.BS), None,\
                                 ontopowsys_PowerSystemModel.Bs.iri, ontocape_mathematical_model.Parameter.iri)    
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_ebus_model_.AREAKey, int(uk_ebus_model_.AREA), None,\
                                 ontopowsys_PowerSystemModel.Area.iri, ontocape_mathematical_model.Parameter.iri)    
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_ebus_model_.VM_INPUTKey, float(uk_ebus_model_.VM_INPUT), ontocape_derived_SI_units.kV.iri, \
                                 ontopowsys_PowerSystemModel.Vm.iri, ontocape_mathematical_model.InputVariable.iri)         
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_ebus_model_.VA_INPUTKey, float(uk_ebus_model_.VA_INPUT), ontocape_derived_SI_units.degree.iri, \
                                 ontopowsys_PowerSystemModel.Va.iri, ontocape_mathematical_model.InputVariable.iri)         
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_ebus_model_.BASEKVKey, int(uk_ebus_model_.BASEKV), ontocape_derived_SI_units.kV.iri, \
                                 ontopowsys_PowerSystemModel.baseKV.iri, ontocape_mathematical_model.Parameter.iri)    
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_ebus_model_.ZONEKey, int(uk_ebus_model_.ZONE), None, \
                                 ontopowsys_PowerSystemModel.Zone.iri, ontocape_mathematical_model.Parameter.iri)   
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_ebus_model_.VMAXKey, float(uk_ebus_model_.VMAX), ontocape_derived_SI_units.kV.iri, \
                                 ontopowsys_PowerSystemModel.VmMax.iri, ontocape_mathematical_model.Parameter.iri) 
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_ebus_model_.VMINKey, float(uk_ebus_model_.VMIN), ontocape_derived_SI_units.kV.iri, \
                                 ontopowsys_PowerSystemModel.VmMin.iri, ontocape_mathematical_model.Parameter.iri)
        
        # print(g.serialize(format="pretty-xml").decode("utf-8"))
               
        # generate/update OWL files
        if updateLocalOWLFile == True:    
            # Store/update the generated owl files      
            if filepath[-2:] != "\\": 
                filepath_ = filepath + '\\' + 'Model_' + str(numOfBus) + '_Bus_Grid_' + node_locator + OWL
            else:
                filepath_ = filepath + 'Model_' + str(numOfBus) + '_Bus_Grid_' + node_locator + OWL
            storeGeneratedOWLs(g, filepath_)
            
    if isinstance(store, Sleepycat):  
        cg_model_EBus.close()       
    return

# The demanding of an AggregatedBus is the sum of their regional consumption (elec demanding)
def checkAggregatedBus(EBus):
    EBus_ = [ [str(ebus[0]), float(ebus[1])] for ebus in EBus]
    bus_node  = []
    for ebus in EBus_:
        if ebus[0] in bus_node:
            counter_1 = bus_node.index(ebus[0])
            counter_2 = EBus_.index(ebus) 
            if counter_2 > counter_1:
                EBus_[counter_1][1] += EBus_[counter_2][1]
                EBus_[counter_1][1] = round(EBus_[counter_1][1], 2)
                del EBus_[counter_2]
            else:
                print('counter_2 should be larger than counter_1')
                return None
        else:
            bus_node.append(ebus[0])
    return EBus_

def initialiseEBusModelVar(EBus_Model, EBus, defaultInitialisation):
    if isinstance (EBus_Model, UK_PG.UKEbusModel):
        pass
    else:
        print('The first argument should be an instence of UKEbusModel.')
        return None
    
    if defaultInitialisation == True:
        EBus_Model.BUS = int((EBus[0].split('#EBus-')[1]).split('_')[0])
        if EBus_Model.BUS == 1: # assign slack bus
            EBus_Model.TYPE = 3
    elif defaultInitialisation == False and os.path.exists(EBus_Model.BusModelInitialisation):
          BusModelInitialisationArrays = readFile(EBus_Model.BusModelInitialisation)  
          if BusModelInitialisationArrays[0] != EBus_Model.headerBusModel:
            print('The Bus Model Initialisation header is not matched, please check the data file')
            return None   
          EBus_Model.BUS = int((EBus[0].split('#EBus-')[1]).split('_')[0])
          EBus_Model.TYPE = BusModelInitialisationArrays[EBus_Model.BUS][1]
          EBus_Model.PD_INPUT = BusModelInitialisationArrays[EBus_Model.BUS][2]
          EBus_Model.GD_INPUT = BusModelInitialisationArrays[EBus_Model.BUS][3]
          EBus_Model.GS = BusModelInitialisationArrays[EBus_Model.BUS][4]
          EBus_Model.BS = BusModelInitialisationArrays[EBus_Model.BUS][5]
          EBus_Model.AREA = BusModelInitialisationArrays[EBus_Model.BUS][6]
          EBus_Model.VM_INPUT = BusModelInitialisationArrays[EBus_Model.BUS][7]
          EBus_Model.VA_INPUT = BusModelInitialisationArrays[EBus_Model.BUS][8]
          EBus_Model.BASEKV = BusModelInitialisationArrays[EBus_Model.BUS][9]
          EBus_Model.ZONE = BusModelInitialisationArrays[EBus_Model.BUS][10]
          EBus_Model.VMAX = BusModelInitialisationArrays[EBus_Model.BUS][11]
          EBus_Model.VMIN = BusModelInitialisationArrays[EBus_Model.BUS][12]     
    else:
        raise NotImplementedError('When the defaultInitialisation flag turns off, the assigment of sluck bus needs more information.')
    
    # initialise Pd
    EBus_Model.PD_INPUT = round((float(EBus[1]) * 1000 / (24 * 365)), 3) 
    
    return EBus_Model

if __name__ == '__main__':    
    createModel_EBus('default', False, 2019, 10, "2017-01-31", "regionalDemandLoad", True, None, True)  
    createModel_EBus('default', False, 2019, 29, "2017-01-31", "closestDemandLoad", False, None, True)            
    print('*****************Terminated*****************')