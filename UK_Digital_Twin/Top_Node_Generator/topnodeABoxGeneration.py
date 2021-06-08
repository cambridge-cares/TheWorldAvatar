##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 21 April 2021        #
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
import SPARQLQueryUsedInTopNode as query_topNode
from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs, selectStoragePath, readFile

"""Notation used in URI construction"""
HASH = '#'
SLASH = '/'
UNDERSCORE = '_'
OWL = '.owl'

"""Graph store"""
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

"""UK digital twin top node Conjunctive graph identifier"""
ukdt_cg_id = "http://www.theworldavatar.com/kb/UK_Digital_Twin"

### Functions ###  
"""Add Top Level node"""
def addTopLevelNode(graph):    
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
def addSubGraphtoSecondLevelNode(graph): 
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


#TODO: finish the addThirdLevelNode func
"""Add sub-graphs to UKPowerPlant and UKEnergyConsumption (third node)"""
def addThirdLevelNode(graph, nodeName, SleepycatPath = None, *localQuery):
    if nodeName == "UKPowerPlant2019": 
        nodeList = list(query_topNode.queryPowerPlantNodeURL())
        contentArrays = readFile(filepath)    
        for content in contentArrays:
            uriSplit = Third_Level_Node[nodeName].split('.owl') 
            uri = uriSplit[0] + SLASH + content[0].strip('\n') + OWL + HASH + content[0].strip('\n')        
            graph.add((URIRef(Third_Level_Node[nodeName]), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri),\
                       URIRef(uri))) 
            graph.add((URIRef(uri), RDF.type, URIRef(ontocape_upper_level_system.ExclusiveSubsystem.iri))) 
        return graph

# """Add sub-graphs to UKPowerPlant and UKEnergyConsumption (third node)"""
# def addThirdLevelNode(graph, filepath, nodeName):
    
#     contentArrays = readFile(filepath)    
#     for content in contentArrays:
#         uriSplit = Third_Level_Node[nodeName].split('.owl') 
#         uri = uriSplit[0] + SLASH + content[0].strip('\n') + OWL + HASH + content[0].strip('\n')        
#         graph.add((URIRef(Third_Level_Node[nodeName]), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri),\
#                    URIRef(uri))) 
#         graph.add((URIRef(uri), RDF.type, URIRef(ontocape_upper_level_system.ExclusiveSubsystem.iri))) 
#     return graph

"""Add Fourth level nodes (EGen, Eline and EBus) to Third Level node (grid model)"""
def addFourthLevelNode(graph): 
    # Link fourth level nodes with third level nodes
    for fourtlevelnode in Fourth_Level_Node: 
        graph.add((URIRef(Third_Level_Node["UKGrid10Bus"]), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(fourtlevelnode)))
        graph.add((URIRef(fourtlevelnode), RDF.type, URIRef(ontocape_mathematical_model.Submodel.iri)))
    return graph

"""####Main function: Create or update the top node owl file####"""
def generateTopNodeOWL(store, updateLocalOWLFile = True):
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
    g = addTopLevelNode(g)
    g = addSubGraphtoSecondLevelNode(g)
    g = addSubGraphs_fromRawData(g, dukes.PlantName,"UKPowerPlant2019")
    g = addSubGraphs_fromRawData(g, engconsump.RegionandAreas, "UKEnergyConsumption2017")
    g = addFourthLevelNode(g)
    
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
   generateTopNodeOWL(store, False)
   