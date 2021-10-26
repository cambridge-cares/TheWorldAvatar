##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 21 Oct 2021          #
##########################################

"""This module is designed to generate and update the A-box of UK power grid model_ELine."""

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
from UK_Digital_Twin_Package import TopologicalInformationProperty as TopoInfo
from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs, selectStoragePath, readFile, specifyValidFilePath
import UK_Power_Grid_Model_Generator.SPARQLQueryUsedInModel as query_model
from UK_Power_Grid_Model_Generator.AddModelVariables import AddModelVariable
from UK_Power_Grid_Topology_Generator.topologyABoxGeneration import createTopologicalInformationPropertyInstance
from UK_Digital_Twin_Package.GraphStore import LocalGraphStore
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel as endpointList
from UK_Digital_Twin_Package import BranchPropertyInitialisation as BPI

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

"""Create an object of Class UKElineModel"""
uk_eline_model = UK_PG.UKElineModel()

"""Create an object of Class UKPowerGridTopology"""
uk_topo = UK_Topo.UKPowerGridTopology()

"""Remote Endpoint lable"""
topology_Endpoint = uk_topo.endpoint['lable']

"""Blazegraph UK digital tiwn"""
endpoint_label = endpointList.ukdigitaltwin['lable']
endpoint_url = endpointList.ukdigitaltwin['queryendpoint_iri']

"""Sleepycat storage path"""
defaultPath_Sleepycat = uk_eline_model.SleepycatStoragePath
topoAndConsumpPath_Sleepycat = uk_topo.SleepycatStoragePath
userSpecifiePath_Sleepycat = None # user specified path
userSpecified_Sleepycat = False # storage mode: False: default, True: user specified

"""OWL file storage path"""
defaultStoredPath = uk_eline_model.StoreGeneratedOWLs # default path

"""T-Box URI"""
ontocape_upper_level_system     = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
ontocape_derived_SI_units       = owlready2.get_ontology(t_box.ontocape_derived_SI_units).load()
ontocape_mathematical_model     = owlready2.get_ontology(t_box.ontocape_mathematical_model).load()
ontopowsys_PowerSystemModel     = owlready2.get_ontology(t_box.ontopowsys_PowerSystemModel).load()

"""User specified folder path"""
filepath = None
userSpecified = False

"""EBus Conjunctive graph identifier"""
model_ELine_cg_id = "http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_ELine"

### Functions ###
"""Main function: create the named graph Model_EBus and their sub graphs each ELine"""
def createModel_ELine(storeType, localQuery, numOfBus, numOfBranch, version_of_DUKES, initialiserMethod, OWLFileStoragePath, updateLocalOWLFile = True): 
    filepath = specifyValidFilePath(defaultStoredPath, OWLFileStoragePath, updateLocalOWLFile)
    if filepath == None:
        return    
    store = LocalGraphStore(storeType)
    topo_info, busInfoArrays, branchTopoInfoArrays, branchPropertyArrays = createTopologicalInformationPropertyInstance(numOfBus, numOfBranch)
    global defaultPath_Sleepycat, userSpecifiePath_Sleepycat, userSpecified_Sleepycat 
    # create conjunctive graph storing the generated graphs in a specified Sleepycat on-disc graph store
    if isinstance(store, Sleepycat): 
        print('The store is Sleepycat')
        cg_model_ELine = ConjunctiveGraph(store=store, identifier = model_ELine_cg_id)
        if userSpecifiePath_Sleepycat == None and userSpecified_Sleepycat:
            print('****Needs user to specify a Sleepycat storage path****')
            userSpecifiePath_Sleepycat = selectStoragePath()
            userSpecifiePath_Sleepycat_ = userSpecifiePath_Sleepycat + '\\' + 'ConjunctiveGraph_UKPowerGrid_ELine'
            sl = cg_model_ELine.open(userSpecifiePath_Sleepycat_, create = False) 
            
        elif os.path.exists(defaultPath_Sleepycat) and not userSpecified_Sleepycat:
            print('****Non user specified Sleepycat storage path, will use the default storage path****')
            sl = cg_model_ELine.open(defaultPath_Sleepycat, create = False)        
        else:
            print('****Create Sleepycat store with its default path****')
            sl = cg_model_ELine.open(defaultPath_Sleepycat, create = True)   
        
        if sl == NO_STORE:
        # There is no underlying Sleepycat infrastructure, so create it
            cg_model_ELine.open(defaultPath_Sleepycat, create = True)
        else:
            assert sl == VALID_STORE, "The underlying sleepycat store is corrupt"
    else:
        print('Store is IOMemery')
    
    
    # TODO: skip the shape geo of elins
    # ELineTopoInfo = list(query_model.queryELineTopologicalInformation(numOfBus, endpoint_label, topoAndConsumpPath_Sleepycat, localQuery))
    ELineTopoInfo = query_model.queryELineTopologicalInformation(numOfBus, endpoint_label, topoAndConsumpPath_Sleepycat, localQuery)
    
    if ELineTopoInfo == None:
        print('ELineTopoInfo is empty')
        return None

    for eline in ELineTopoInfo:         
    # if ELineTopoInfo[0] != None: # test
    #     eline = ELineTopoInfo[0] # test
    
        print('################START createModel_ELine#################')
        root_uri = eline['ELine'].split('#')[0]
        namespace = root_uri + HASH
        node_locator = eline['ELine'].split('#')[1]
        root_node = namespace + 'Model_' + node_locator
        father_node = UKDT.nodeURIGenerator(4, dt.powerGridModel, numOfBus, "ELine")
        
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
        g.add((URIRef(root_node), RDF.type, URIRef(t_box.ontopowsys_PowerSystemModel + 'ElectricalBranchModel'))) # undefined T-box class, the sub-class of PowerFlowModelAgent
        g.add((URIRef(father_node), URIRef(ontocape_upper_level_system.isComposedOfSubsystem .iri), URIRef(root_node)))
        # link with ELine node in topology
        g.add((URIRef(root_node), URIRef(ontocape_upper_level_system.models.iri), URIRef(eline['ELine'])))
        g.add((URIRef(eline['ELine']), URIRef(ontocape_upper_level_system.isModeledBy.iri), URIRef(root_node)))
        
        # specify the initialisation method for each branch instance of branch model
        ###1. create the instance of the ELine(branch) model
        uk_eline_model_ = UK_PG.UKElineModel(version_of_DUKES, numOfBus)
        ###2. create an instance of the BranchPropertyInitialisation class and get the initialiser method by applying the 'getattr' function 
        initialisation = BPI.BranchPropertyInitialisation()
        initialiser = getattr(initialisation, initialiserMethod)
        ###3. execute the initialiser with the branch model instance as the function argument  
        uk_eline_model_ = initialiser(uk_eline_model_, eline) 
        
        ###add ELine model parameter###
        #  uk_eline_model_ = UK_PG.UKElineModel(version_of_DUKES, numOfBus)
        
        # TODO: the BranchPropertyInitialisation has redefined the initialisation of the branch model variables 
        uk_eline_model_ = initialiseELineModelVar(topo_info, branchPropertyArrays, uk_eline_model_, eline) 
        
        if uk_eline_model_ != None:
            pass
        else: 
            print ('uk_eline_model_ is none')
            return None 
        
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_eline_model_.FROMBUSKey, int(uk_eline_model_.FROMBUS), None, \
                                 ontopowsys_PowerSystemModel.BusFrom.iri, ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_eline_model_.TOBUSKey, int(uk_eline_model_.TOBUS), None, \
                                 ontopowsys_PowerSystemModel.BusTo.iri, ontocape_mathematical_model.Parameter.iri)  
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_eline_model_.R_Key, float(uk_eline_model_.R), ontocape_derived_SI_units.ohm.iri, \
                                 ontopowsys_PowerSystemModel.R.iri, ontocape_mathematical_model.Parameter.iri)     
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_eline_model_.X_Key, float(uk_eline_model_.X), ontocape_derived_SI_units.ohm.iri, \
                                 ontopowsys_PowerSystemModel.X.iri, ontocape_mathematical_model.Parameter.iri) 
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_eline_model_.B_Key, float(uk_eline_model_.B), (t_box.ontocape_derived_SI_units + 'siemens'), \
                                 ontopowsys_PowerSystemModel.B.iri, ontocape_mathematical_model.Parameter.iri)    
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_eline_model_.RateAKey, float(uk_eline_model_.RateA), (t_box.ontocape_derived_SI_units + 'MVA'), \
                                 ontopowsys_PowerSystemModel.RateA.iri, ontocape_mathematical_model.Parameter.iri)       
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_eline_model_.RateBKey, float(uk_eline_model_.RateB), (t_box.ontocape_derived_SI_units + 'MVA'), \
                                 ontopowsys_PowerSystemModel.RateB.iri, ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_eline_model_.RateCKey, float(uk_eline_model_.RateB), (t_box.ontocape_derived_SI_units + 'MVA'), \
                                 ontopowsys_PowerSystemModel.RateC.iri, ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_eline_model_.RATIOKey, float(uk_eline_model_.RATIO), None, \
                                 ontopowsys_PowerSystemModel.RatioCoefficient.iri, ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_eline_model_.ANGLEKey, float(uk_eline_model_.ANGLE), ontocape_derived_SI_units.degree.iri, \
                                 ontopowsys_PowerSystemModel.Angle.iri, ontocape_mathematical_model.Parameter.iri)    
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_eline_model_.STATUSKey, int(uk_eline_model_.STATUS), None, \
                                 ontopowsys_PowerSystemModel.BranchStatus.iri, ontocape_mathematical_model.Parameter.iri)    
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_eline_model_.ANGMINKey, float(uk_eline_model_.ANGMIN), ontocape_derived_SI_units.degree.iri, \
                                 ontopowsys_PowerSystemModel.AngleMin.iri, ontocape_mathematical_model.Parameter.iri)   
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_eline_model_.ANGMAXKey, float(uk_eline_model_.ANGMAX), ontocape_derived_SI_units.degree.iri, \
                                 ontopowsys_PowerSystemModel.AngleMax.iri, ontocape_mathematical_model.Parameter.iri) 
                    
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
        cg_model_ELine.close()       
    return

# TODO: skip the calculation of B, R, X of 29_bus model
# check the header of the eline propery
# Eline = ['http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_ELine-001.owl#ELine-001', 1, 2, 184.8475, 3, 0] , 400kv, 275kv
def initialiseELineModelVar(topo_info, branchPropertyArrays, ELine_Model, ELine):
    if isinstance (ELine_Model, UK_PG.UKElineModel):
        pass
    else:
        print('The first argument should be an instence of UKElineModel')
        return None
    ELine_Model.FROMBUS = ELine[1]
    ELine_Model.TOBUS = ELine[2]
    if len(branchPropertyArrays) != 0:
    
    if branchPropertyArrays[0] != topo_info.headerBranchProperty or int(branchPropertyArrays[1][0]) != 275 or int(branchPropertyArrays[2][0]) != 400:
        print('The branch property data header is not matched, please check the data file')
        return None
    
    if ELine[5] == 0: # Only 400kV lines
        ELine_Model.R = ELine[3] * float(branchPropertyArrays[2][1]) / ELine[4]
        ELine_Model.X = ELine[3] * float(branchPropertyArrays[2][2]) / ELine[4]
        ELine_Model.B = ELine[3] * float(branchPropertyArrays[2][3]) * ELine[4]
    elif ELine[4] == 0: # Only 275kV lines
        ELine_Model.R = ELine[3] * float(branchPropertyArrays[1][1]) / ELine[5] 
        ELine_Model.X = ELine[3] * float(branchPropertyArrays[1][2]) / ELine[5] 
        ELine_Model.B = ELine[3] * float(branchPropertyArrays[1][3]) * ELine[5] 
    else: # 400kV and 275kV lines
        ELine_Model.R = 1 / ((1 / ( ELine[3] * float(branchPropertyArrays[2][1]) / ELine[4])) + (1 / (ELine[3] * float(branchPropertyArrays[1][1]) / ELine[5] )))
        ELine_Model.X = 1 / ((1 / ( ELine[3] * float(branchPropertyArrays[2][2]) / ELine[4])) + (1 / (ELine[3] * float(branchPropertyArrays[1][2]) / ELine[5] )))
        ELine_Model.B = (ELine[3] * float(branchPropertyArrays[2][3]) * ELine[4]) + (ELine[3] * float(branchPropertyArrays[1][3]) * ELine[5])
    
    ELine_Model.RateA = (float(branchPropertyArrays[2][4]) * ELine[4]) + (float(branchPropertyArrays[1][4]) * ELine[5])
    
    return ELine_Model

if __name__ == '__main__':    
    createModel_ELine('default', False, 10, 14, 2019, None, True)       
    print('Terminated')