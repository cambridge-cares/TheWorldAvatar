##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 08 June 2021         #
##########################################

"""This module is designed to generate the top node of UK digital twin."""

import os
import owlready2
from rdflib.extras.infixowl import OWL_NS
from rdflib import Graph, URIRef, ConjunctiveGraph
from rdflib.namespace import RDF
from rdflib.plugins.sleepycat import Sleepycat
from rdflib.store import NO_STORE, VALID_STORE
import sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from UK_Digital_Twin_Package import UKDigitalTwinTBox as T_BOX
from UK_Digital_Twin_Package import DUKESDataProperty as DUKES
from UK_Digital_Twin_Package import EnergyConsumptionDataProperty as EngConsump
from UK_Digital_Twin_Package import UKPowerPlant as UKpp
from UK_Digital_Twin_Package import UKPowerGridTopology as UK_Topo
from UK_Digital_Twin_Package import UKPowerGridModel as UK_PG
import SPARQLQueryUsedInTopNode as query_topNode
from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs, selectStoragePath, readFile

"""Notation used in URI construction"""
HASH = '#'
SLASH = '/'
UNDERSCORE = '_'
OWL = '.owl'

"""Local Graph store"""
# store = 'default'
store = Sleepycat()
store.__open = True
store.context_aware = True

"""Create an instance of Class UKDigitalTwin"""
dt = UKDT.UKDigitalTwin()

"""Create an object of Class UKDigitalTwinTBox"""
t_box = T_BOX.UKDigitalTwinTBox()

"""Create an object of Class DUKESDataProperty"""
dukes = DUKES.DUKESData()

"""Create an object of Class EnergyConsumptionDataProperty"""
engconsump = EngConsump.EnergyConsumptionData()

"""Create an object of Class UKPowerPlantDataProperty"""
ukpp = UKpp.UKPowerPlant()

"""Create an object of Class UKPowerGridTopology"""
uk_topo = UK_Topo.UKPowerGridTopology()

"""Create an object of Class UKPowerGridModel"""
uk_egen_model = UK_PG.UKEGenModel()
uk_ebus_model = UK_PG.UKEbusModel()
uk_eline_model = UK_PG.UKElineModel()

"""NodeURI"""
Top_Level_Node = {
    "UKDigitalTwin" : UKDT.nodeURIGenerator(1, dt.topNode, None)
    }

Second_Level_Node = {
    "UKPowerPlant" : UKDT.nodeURIGenerator(2, dt.powerPlant, None),
    "UKEnergyConsumption": UKDT.nodeURIGenerator(2, dt.energyConsumption, None),
    "UKGridTopology": UKDT.nodeURIGenerator(2, dt.gridTopology, None),
    "UKPowerGrid": UKDT.nodeURIGenerator(2, dt.powerGridModel, None)
    }

Third_Level_Node = {
    "UKPowerPlant2019" : UKDT.nodeURIGenerator(3, dt.powerPlant, dukes.VERSION),
    "UKEnergyConsumption2017" : UKDT.nodeURIGenerator(3, dt.energyConsumption, engconsump.VERSION),
    "UKTopology10Bus" : UKDT.nodeURIGenerator(3, dt.gridTopology, 10),
    "UKGrid10Bus" : UKDT.nodeURIGenerator(3, dt.powerGridModel, 10)
    }

Fourth_Level_Node = UKDT.nodeURIGenerator(4, dt.powerGridModel, 10)

"""T-Box URI"""
#OntoCAPE_upper_level_system
ontocape_upper_level_system = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
ontocape_mathematical_model = owlready2.get_ontology(t_box.ontocape_mathematical_model).load()
ontocape_network_system = owlready2.get_ontology(t_box.ontocape_network_system).load()

"""OWL file storage path"""
defaultStoredPath = dt.StoreGeneratedOWLs + dt.topNode + OWL # default path
filepath = None # user specified path
userSpecified = False # storage mode: False: default, True: user specified

"""Sleepycat storage config"""
userSpecifiePath_Sleepycat = None # user specified path
userSpecified_Sleepycat = False # storage mode: False: default, True: user specified
defaultPath_Sleepycat = dt.SleepycatStoragePath

"""Sleepycat storage path"""
powerPlant_Sleepycat = ukpp.SleepycatStoragePath
topoAndConsumpPath_Sleepycat = uk_topo.SleepycatStoragePath
uk_egen_model_Sleepycat = uk_egen_model.SleepycatStoragePath
uk_ebus_model_Sleepycat = uk_ebus_model.SleepycatStoragePath
uk_eline_model_Sleepycat = uk_eline_model.SleepycatStoragePath

"""UK digital twin top node Conjunctive graph identifier"""
ukdt_cg_id = "http://www.theworldavatar.com/kb/UK_Digital_Twin"

### Functions ###  
"""Add Top Level node"""
def addTopAndSecondLevelNode(graph):    
    # Import T-boxes
    graph.set((graph.identifier, RDF.type, OWL_NS['Ontology']))
    graph.set((graph.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_upper_level_system)))
    
    # Add topnode triples
    graph.set((URIRef(Top_Level_Node['UKDigitalTwin']), RDF.type, URIRef(ontocape_upper_level_system.TopLevelSystem.iri)))
    
    # Asign the location of UKDigitalTwin
    graph.add((URIRef(Top_Level_Node['UKDigitalTwin']), URIRef(ontocape_upper_level_system.hasAddress.iri), URIRef(t_box.UK)))  
    
    # Link Second Level Nodes
    for node in Second_Level_Node.keys():
        graph.add((URIRef(Top_Level_Node['UKDigitalTwin']), URIRef(ontocape_upper_level_system.hasDirectSubsystem.iri), URIRef(Second_Level_Node[node])))
        # Add system type of second level nodes  
        graph.add((URIRef(Second_Level_Node[node]), RDF.type, URIRef(ontocape_upper_level_system.FirstLevelSubsystem.iri))) 
    graph.add((URIRef(Second_Level_Node["UKPowerGrid"]), RDF.type, URIRef(ontocape_mathematical_model.ModeledObject.iri))) 
    return graph

"""Add Sub-graphs to Second Level node"""
def addThirdLevelNode(graph): 
    # Link second level nodes with third level nodes
    graph.add((URIRef(Second_Level_Node['UKPowerPlant']),\
               URIRef(ontocape_upper_level_system.hasDirectSubsystem.iri), URIRef(Third_Level_Node['UKPowerPlant2019'])))
    graph.add((URIRef(Second_Level_Node['UKEnergyConsumption']),\
               URIRef(ontocape_upper_level_system.hasDirectSubsystem.iri), URIRef(Third_Level_Node['UKEnergyConsumption2017'])))
    graph.add((URIRef(Second_Level_Node['UKGridTopology']),\
               URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(Third_Level_Node['UKTopology10Bus'])))
    graph.add((URIRef(Second_Level_Node['UKPowerGrid']),\
              URIRef(ontocape_upper_level_system.isModeledBy.iri), URIRef(Third_Level_Node['UKGrid10Bus'])))
    
    # Add system type of thrid level nodes 
    graph.add((URIRef(Third_Level_Node["UKPowerPlant2019"]), RDF.type, URIRef(ontocape_upper_level_system.CompositeSystem.iri))) 
    graph.add((URIRef(Third_Level_Node["UKEnergyConsumption2017"]), RDF.type, URIRef(ontocape_upper_level_system.CompositeSystem.iri)))
    graph.add((URIRef(Third_Level_Node["UKTopology10Bus"]), RDF.type, URIRef(ontocape_network_system.NetworkSystem.iri))) 
    graph.add((URIRef(Third_Level_Node["UKGrid10Bus"]), RDF.type, URIRef(ontocape_mathematical_model.MathematicalModel.iri)))
    return graph


#TODO: upload the energy consumption data to the como rdf4j and test the remote query
"""Add sub-graphs to UKPowerPlant and UKEnergyConsumption (third node)"""
def addFourthLevelNode_powerPlant_energyConsumption(graph, nodeName, localQuery, SleepycatPath = None):
    if localQuery == False:
        if nodeName == "UKPowerPlant2019": 
            nodeList = query_topNode.queryPowerPlantNodeURL(SleepycatPath, localQuery)
        elif nodeName == "UKEnergyConsumption2017": 
            nodeList = query_topNode.queryUKEnergyConsumptionNodeURL(SleepycatPath, localQuery)       
    elif SleepycatPath != None and localQuery == True:   
        if nodeName == "UKPowerPlant2019": 
            nodeList = list(query_topNode.queryPowerPlantNodeURL(SleepycatPath, localQuery))
        elif nodeName == "UKEnergyConsumption2017": 
            nodeList = list(query_topNode.queryUKEnergyConsumptionNodeURL(SleepycatPath, localQuery))       
    for node in nodeList:
        if SleepycatPath != None and localQuery == True:  
            node = node[0]
        graph.add((URIRef(Third_Level_Node[nodeName]), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri),\
                   URIRef(node))) 
        graph.add((URIRef(node), RDF.type, URIRef(ontocape_upper_level_system.ExclusiveSubsystem.iri))) 
    return graph

"""Add Fourth level nodes (Model_EGen, Model_Eline and Model_EBus) to Third Level node (grid model)"""
def addFourthLevelNode_gridModel(graph): 
    for fourtlevelnode in Fourth_Level_Node: 
        graph.add((URIRef(Third_Level_Node["UKGrid10Bus"]), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(fourtlevelnode)))
        graph.add((URIRef(fourtlevelnode), RDF.type, URIRef(ontocape_mathematical_model.Submodel.iri)))
    return graph

#TODO: up load the model KG to the remote triple store in como
"""Add Fifth level nodes (Model_EGen-001, Model_Eline-001, Model_EBus-001, etc.) to Fourth Level node (Model_EGen, Model_Eline and Model_EBus)"""
def addFifthLevelNode_gridModel(graph, nodeName, localQuery, SleepycatPath = None):     
   if localQuery == False:
        if nodeName == "EGen": 
            nodeList = query_topNode.queryEGenNodeURL(SleepycatPath, localQuery)            
        elif nodeName == "EBus": 
            nodeList = query_topNode.queryEBusNodeURL(SleepycatPath, localQuery)
        elif nodeName == "ELine": 
            nodeList = query_topNode.queryELineNodeURL(SleepycatPath, localQuery)
   elif SleepycatPath != None and localQuery == True:   
        if nodeName == "EGen": 
            nodeList = list(query_topNode.queryEGenNodeURL(SleepycatPath, localQuery))
        elif nodeName == "EBus": 
            nodeList = list(query_topNode.queryEBusNodeURL(SleepycatPath, localQuery))       
        elif nodeName == "ELine": 
            nodeList = list(query_topNode.queryELineNodeURL(SleepycatPath, localQuery))       
   for node in nodeList:
       if SleepycatPath != None and localQuery == True:  
            node = node[0]
       graph.add((URIRef(UKDT.nodeURIGenerator(4, dt.powerGridModel, 10, nodeName)), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri),\
                   URIRef(node))) 
       graph.add((URIRef(node), RDF.type, URIRef(ontocape_upper_level_system.ExclusiveSubsystem.iri))) 
   return graph
    

"""####Main function: Create or update the top node owl file####"""
def generateTopNodeOWL(store, localQuery, updateLocalOWLFile = True):
    baseURI = (Top_Level_Node['UKDigitalTwin'].split('#'))[0]
    
    global userSpecifiePath_Sleepycat, userSpecified_Sleepycat, defaultPath_Sleepycat
    if isinstance(store, Sleepycat):    
        # Create Conjunctive graph maintain all power plant graphs
        dtConjunctiveGraph = ConjunctiveGraph(store=store, identifier = ukdt_cg_id)
        if userSpecifiePath_Sleepycat == None and userSpecified_Sleepycat:
            print('****Needs user to specify a Sleepycat storage path****')
            userSpecifiePath_Sleepycat = selectStoragePath()
            userSpecifiePath_Sleepycat_ = userSpecifiePath_Sleepycat + '\\' + 'ConjunctiveGraph_UKDigitalTwinTopNode'
            sl = dtConjunctiveGraph.open(userSpecifiePath_Sleepycat_, create = False) 
            
        elif os.path.exists(defaultPath_Sleepycat) and not userSpecified_Sleepycat:
            print('****Non user specified Sleepycat storage path, will use the default storage path****')
            sl = dtConjunctiveGraph.open(defaultPath_Sleepycat, create = False)        
        else:
            sl = dtConjunctiveGraph.open(defaultPath_Sleepycat, create = True)   
        
        if sl == NO_STORE:
        # There is no underlying Sleepycat infrastructure, so create it
            dtConjunctiveGraph.open(defaultPath_Sleepycat, create=True)
        else:
            assert sl == VALID_STORE, "The underlying sleepycat store is corrupt"
    
    
    g = Graph(store = store, identifier = URIRef(baseURI)) # graph(store='default', identifier)
    g = addTopAndSecondLevelNode(g)
    g = addThirdLevelNode(g)
    g = addFourthLevelNode_powerPlant_energyConsumption(g, "UKPowerPlant2019", localQuery, powerPlant_Sleepycat)
    g = addFourthLevelNode_powerPlant_energyConsumption(g, "UKEnergyConsumption2017", localQuery, topoAndConsumpPath_Sleepycat)
    g = addFourthLevelNode_gridModel(g)    
    g = addFifthLevelNode_gridModel(g, "EGen", localQuery, uk_egen_model_Sleepycat)
    g = addFifthLevelNode_gridModel(g, "EBus", localQuery, uk_egen_model_Sleepycat)
    g = addFifthLevelNode_gridModel(g, "ELine", localQuery, uk_egen_model_Sleepycat)
    
    global filepath, userSpecified
    
    if updateLocalOWLFile == True: 
        # Store/update the generated owl files      
        if os.path.exists(dt.StoreGeneratedOWLs) and not userSpecified:
            print('****Non user specified storage path, will use the default storage path****')
            storeGeneratedOWLs(g, defaultStoredPath)
    
        elif filepath == None:
            print('****Needs user to specify a storage path****')
            filepath = selectStoragePath()
            filepath_ = filepath + '\\' + dt.topNode + OWL
            storeGeneratedOWLs(g, filepath_)
        else: 
            filepath_ = filepath + '\\' + dt.topNode + OWL
            storeGeneratedOWLs(g, filepath_)
            
    if isinstance(store, Sleepycat):  
        dtConjunctiveGraph.close()     
    return

if __name__ == '__main__':
   generateTopNodeOWL(store, False, True)
   