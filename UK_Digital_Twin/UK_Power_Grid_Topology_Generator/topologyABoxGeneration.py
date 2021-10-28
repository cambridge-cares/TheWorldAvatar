##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 25 Oct 2021          #
##########################################

"""This module is designed to generate and update the A-box of UK power grid topology graph."""

import owlready2
# import numpy as np
from rdflib.extras.infixowl import OWL_NS
from rdflib import Graph, URIRef, Literal, ConjunctiveGraph
from rdflib.namespace import RDF, RDFS
from rdflib.plugins.sleepycat import Sleepycat
from rdflib.store import NO_STORE
import sys, os
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from UK_Digital_Twin_Package import UKDigitalTwinTBox as T_BOX
from UK_Digital_Twin_Package import TopologicalInformationProperty as TopoInfo
from UK_Digital_Twin_Package import UKPowerGridTopology as UK_Topo
from UK_Digital_Twin_Package import UKPowerGridModel as UK_PG
from UK_Digital_Twin_Package import UKPowerPlant as UKpp
from UK_Digital_Twin_Package import UKEnergyConsumption as UKec
from UK_Digital_Twin_Package import CO2FactorAndGenCostFactor as ModelFactor
from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs, selectStoragePath, readFile, specifyValidFilePath
from UK_Digital_Twin_Package.GraphStore import LocalGraphStore
from UK_Digital_Twin_Package.DistanceCalculator import DistanceBasedOnGPSLocation
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel as endpointList
from UK_Digital_Twin_Package import generatorCluster as genCluster

import UK_Power_Grid_Topology_Generator.SPARQLQueriesUsedInTopologyABox as query_topo

"""Notation used in URI construction"""
HASH = '#'
SLASH = '/'
UNDERSCORE = '_'
OWL = '.owl'
KV = 'kV'

"""Graph store"""
store = 'default'

"""Create an instance of Class UKDigitalTwin"""
dt = UKDT.UKDigitalTwin()

"""Create an object of Class UKDigitalTwinTBox"""
t_box = T_BOX.UKDigitalTwinTBox()

"""Create an object of Class UKPowerGridTopology"""
# uk_topo = UK_Topo.UKPowerGridTopology()

"""Create an object of Class UKPowerPlant"""
ukpp = UKpp.UKPowerPlant()

"""Create an object of Class CO2FactorAndGenCostFactor"""
ukmf = ModelFactor.ModelFactor()

"""Create an object of Class UKEnergyConsumption"""
ukec = UKec.UKEnergyConsumption()

"""Create an object of Class UKPowerGridModel"""
uk_ebus_model = UK_PG.UKEbusModel()
uk_eline_model = UK_PG.UKElineModel()
uk_egen_model = UK_PG.UKEGenModel()

"""OWL file storage path"""
#defaultStoredPath = uk_topo.StoreGeneratedOWLs

"""Sleepycat storage path"""
#defaultPath_Sleepycat = uk_topo.SleepycatStoragePath

# """Remote Endpoint lable"""
# powerPlant_Endpoint = ukpp.endpoint['lable']
# topology_Endpoint = uk_topo.endpoint['lable']
# topology_federated_query_ep = uk_topo.endpoint['queryendpoint_iri']
# energyConsumption_federated_query_ep = ukec.endpoint['queryendpoint_iri']

"""Blazegraph UK digital tiwn"""
endpoint_label = endpointList.ukdigitaltwin['lable']
endpoint_url = endpointList.ukdigitaltwin['queryendpoint_iri']

"""Root_node, root_uri (top node) and namespace creator based on different bus number"""
def rootNodeAndNameSpace(numOfBus): 
    root_node = UKDT.nodeURIGenerator(3, dt.gridTopology, numOfBus)
    gridModelNodeSegment = UKDT.nodeURIGenerator(3, dt.powerGridModel, numOfBus).split(OWL)[0] 
    root_uri = root_node.split('#')[0]
    tp_namespace = root_uri + HASH
    return root_node, root_uri, tp_namespace, gridModelNodeSegment

"""T-Box URI"""
ontocape_upper_level_system     = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
ontocape_derived_SI_units       = owlready2.get_ontology(t_box.ontocape_derived_SI_units).load()
ontoecape_space_and_time_extended = owlready2.get_ontology(t_box.ontoecape_space_and_time_extended).load()
ontocape_network_system         = owlready2.get_ontology(t_box.ontocape_network_system).load()
ontopowsys_PowSysFunction       = owlready2.get_ontology(t_box.ontopowsys_PowSysFunction).load()
ontoecape_technical_system      = owlready2.get_ontology(t_box.ontoecape_technical_system).load()
ontopowsys_PowSysRealization    = owlready2.get_ontology(t_box.ontopowsys_PowSysRealization).load()
ontocape_coordinate_system      = owlready2.get_ontology(t_box.ontocape_coordinate_system).load()
ontoecape_space_and_time        = owlready2.get_ontology(t_box.ontoecape_space_and_time).load()
ontocape_physical_dimension     = owlready2.get_ontology(t_box.ontocape_physical_dimension).load()
ontocape_SI_units               = owlready2.get_ontology(t_box.ontocape_SI_units).load()
ontocape_geometry               = owlready2.get_ontology(t_box.ontocape_geometry).load()
meta_model_topology             = owlready2.get_ontology(t_box.meta_model_topology).load()
ontopowsys_PowSysPerformance    = owlready2.get_ontology(t_box.ontopowsys_PowSysPerformance).load()
ontoeip_powerplant              = owlready2.get_ontology(t_box.ontoeip_powerplant).load()

"""Data Array"""
modelFactorArrays = readFile(ukmf.CO2EmissionFactorAndCostFactor)

"""User specified folder path"""
filepath = None
userSpecified = False

"""Topology Conjunctive graph identifier"""
topo_cg_id = "http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model"

### Functions ### 

""" Create the TopologicalInformationProperty Instance by specifying its numOfBus and numOfBranch"""
def createTopologicalInformationPropertyInstance(numOfBus, numOfBranch, voltageLevel):
    topo_info = TopoInfo.TopologicalInformation(numOfBus, numOfBranch, voltageLevel)   
    busInfoArrays = readFile(topo_info.BusInfo)
    branchTopoInfoArrays = readFile(topo_info.BranchInfo)
    return topo_info, busInfoArrays, branchTopoInfoArrays

"""Main function: create the sub graph represents the Topology"""
def createTopologyGraph(storeType, localQuery, numOfBus, numOfBranch, addEBusNodes, addELineNodes, addEGenNodes, generatorClusterFunctionName, voltageLevel, OWLFileStoragePath, updateLocalOWLFile = True):
    uk_topo = UK_Topo.UKPowerGridTopology(numOfBus)
    defaultStoredPath = uk_topo.StoreGeneratedOWLs
    defaultPath_Sleepycat = uk_topo.SleepycatStoragePath
    filepath = specifyValidFilePath(defaultStoredPath, OWLFileStoragePath, updateLocalOWLFile)
    if filepath == None:
        return
    store = LocalGraphStore(storeType) 
    # specify the topology properties according to the model type
    topo_info, busInfoArrays, branchTopoInfoArrays = createTopologicalInformationPropertyInstance(numOfBus, numOfBranch, voltageLevel)
    # specify the top node and name space
    root_node, root_uri, tp_namespace, gridModelNodeSegment = rootNodeAndNameSpace(numOfBus)
    print('Create the graph for ', topo_info.EBus_num, ' buses and ', topo_info.ELine_num, ' branches topology.')
    if isinstance(store, Sleepycat): 
        cg_topo_ukec = ConjunctiveGraph(store=store, identifier = topo_cg_id)
        sl = cg_topo_ukec.open(defaultPath_Sleepycat, create = False)
        if sl == NO_STORE:
            print('Cannot find the specified sleepycat store')
    else:
        cg_topo_ukec = None
    
    # create a named graph
    g = Graph(store = store, identifier = URIRef(root_uri))
       
    # Import T-boxes
    g.set((g.identifier, RDF.type, OWL_NS['Ontology']))
    g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_network_system)))
    g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_upper_level_system)))
    
    # Add root node type and the connection between root node and its father node   
    g.add((URIRef(root_node), URIRef(ontocape_upper_level_system.isExclusivelySubsystemOf.iri), URIRef(UKDT.nodeURIGenerator(2, dt.gridTopology, None))))
    g.add((URIRef(UKDT.nodeURIGenerator(2, dt.gridTopology, None)), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(root_node)))
    g.add((URIRef(root_node), RDF.type, URIRef(ontocape_network_system.NetworkSystem.iri)))
    g.add((URIRef(root_node), RDFS.label, Literal("UK_Topology_" + str(numOfBus) + "_Bus")))  
    
    if addEBusNodes != None:
        g, nodeName = addEBusNodes(g, topo_info.headerBusTopologicalInformation, busInfoArrays, numOfBus, root_node, root_uri, tp_namespace, uk_topo)
    if addELineNodes != None:    
        g, nodeName = addELineNodes(g, numOfBranch, branchTopoInfoArrays, topo_info.headerBranchTopologicalInformation, localQuery, root_node, root_uri, tp_namespace, gridModelNodeSegment, uk_topo)
    if addEGenNodes != None:
        g, nodeName = addEGenNodes(g, numOfBus, generatorClusterFunctionName, cg_topo_ukec, modelFactorArrays, localQuery, root_node, root_uri, tp_namespace, uk_topo)
    
    # generate/update OWL files
    if updateLocalOWLFile == True:  
        if filepath[-2:] != "\\": 
            filepath_ = filepath + '\\' + 'UK_' + topo_info.Name + "_" + nodeName + OWL
        else:
            filepath_ = filepath + 'UK_' + topo_info.Name + "_" + nodeName + OWL
        storeGeneratedOWLs(g, filepath_)
        
    if isinstance(store, Sleepycat):  
        cg_topo_ukec.close()       
    return

"""Add nodes represent Buses"""
def addEBusNodes(graph, header, dataArray, numOfBus, root_node, root_uri, tp_namespace, uk_topo): 
    print('****************Start adding bus node triples in the topology graph****************')
    nodeName = "EBus"
    if dataArray[0] != header:
        raise Exception('The bus topoinfo data header is not matched, please check the data file.') 
        
    counter = 1
    while counter < len(dataArray):   
        # print('Counter is ', counter)
        busTopoData = dataArray[counter]
        busTopoData[2] = str(busTopoData[2]).replace("|", ",")
        print("##Bus number ", counter, " located at ", busTopoData[2])
        # the EquipmentConnection_EBus node uri
        bus_context_locator = uk_topo.EquipmentConnection_EBusKey + busTopoData[0].zfill(3) 
        bus_node = tp_namespace + bus_context_locator
        Ebus_context_locator = uk_ebus_model.EBusKey + busTopoData[0].zfill(3) + UNDERSCORE + busTopoData[1]
        Model_EBus_context_locator = uk_ebus_model.ModelEBusKey + busTopoData[0].zfill(3) + UNDERSCORE + busTopoData[1]
        Ebus_node = UKDT.nodeURIGenerator(3, dt.powerGridModel, numOfBus).split(OWL)[0] + SLASH + Model_EBus_context_locator + OWL + HASH + Ebus_context_locator
        # Link bus node with root node and specify its type
        graph.add((URIRef(root_node), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(bus_node)))
        graph.add((URIRef(bus_node), RDF.type, URIRef(ontopowsys_PowSysFunction.PowerEquipmentConnection.iri)))
        # link EquipmentConnection_EBus node with EBus node
        graph.add((URIRef(bus_node), URIRef(ontoecape_technical_system.isRealizedBy.iri), URIRef(Ebus_node)))
        graph.add((URIRef(Ebus_node), RDF.type, URIRef(ontopowsys_PowSysRealization.BusNode.iri)))
        # Add location and GPS of bus        
        graph.add((URIRef(bus_node), URIRef(ontocape_upper_level_system.hasAddress.iri), URIRef(t_box.dbr + busTopoData[1]))) # region, rdf.type is t_box.dbo + 'Region'
        if busTopoData[5].strip('\n') != 'None': 
           graph.add((URIRef(bus_node), URIRef(ontocape_upper_level_system.hasAddress.iri), URIRef(t_box.dbr + busTopoData[5].strip('\n').strip('&')))) # Agrregated bus node
        graph.add((URIRef(bus_node), URIRef(ontocape_upper_level_system.hasAddress.iri), URIRef(t_box.dbr + busTopoData[2]))) # city, rdf.type is ontoecape_space_and_time_extended.AddressArea
        graph.add((URIRef(t_box.dbr + busTopoData[2]), URIRef(t_box.dbo + 'areaCode'), Literal(busTopoData[6].strip('\n')))) #the LA code of the local authority of where the bus located
        graph.add((URIRef(t_box.dbr + busTopoData[2]), RDF.type, URIRef(ontoecape_space_and_time_extended.AddressArea.iri))) 
        
        graph.add((URIRef(bus_node), URIRef(ontoecape_space_and_time_extended.hasGISCoordinateSystem.iri), URIRef(tp_namespace + uk_topo.CoordinateSystemKey + bus_context_locator)))
        graph.add((URIRef(tp_namespace + uk_topo.CoordinateSystemKey + bus_context_locator), RDF.type, URIRef(ontoecape_space_and_time_extended.ProjectedCoordinateSystem.iri)))       
        graph.add((URIRef(tp_namespace + uk_topo.CoordinateSystemKey + bus_context_locator), URIRef(ontoecape_space_and_time_extended.hasProjectedCoordinate_x.iri),\
                   URIRef(tp_namespace + uk_topo.LongitudeKey + bus_context_locator)))
        graph.add((URIRef(tp_namespace + uk_topo.CoordinateSystemKey + bus_context_locator), URIRef(ontoecape_space_and_time_extended.hasProjectedCoordinate_y.iri),\
                   URIRef(tp_namespace + uk_topo.LantitudeKey + bus_context_locator)))
        graph.add((URIRef(tp_namespace + uk_topo.LantitudeKey + bus_context_locator), RDF.type, URIRef(ontoecape_space_and_time.StraightCoordinate.iri)))   
        graph.add((URIRef(tp_namespace + uk_topo.LongitudeKey + bus_context_locator), RDF.type, URIRef(ontoecape_space_and_time.StraightCoordinate.iri)))       
        graph.add((URIRef(tp_namespace + uk_topo.LantitudeKey + bus_context_locator), URIRef(ontocape_upper_level_system.hasDimension.iri), URIRef(ontocape_physical_dimension.length.iri)))
        graph.add((URIRef(tp_namespace + uk_topo.LongitudeKey + bus_context_locator), URIRef(ontocape_upper_level_system.hasDimension.iri), URIRef(ontocape_physical_dimension.length.iri)))
        graph.add((URIRef(tp_namespace + uk_topo.LongitudeKey + bus_context_locator), URIRef(ontocape_coordinate_system.refersToAxis.iri), URIRef(t_box.ontoecape_space_and_time + 'y-axis')))
        graph.add((URIRef(tp_namespace + uk_topo.LantitudeKey + bus_context_locator), URIRef(ontocape_coordinate_system.refersToAxis.iri), URIRef(t_box.ontoecape_space_and_time + 'x-axis')))
        
        graph.add((URIRef(tp_namespace + uk_topo.LantitudeKey + bus_context_locator), URIRef(ontocape_upper_level_system.hasValue.iri),\
                   URIRef(tp_namespace + uk_topo.valueKey + uk_topo.LantitudeKey + bus_context_locator)))
        graph.add((URIRef(tp_namespace + uk_topo.valueKey + uk_topo.LantitudeKey + bus_context_locator), RDF.type, URIRef(ontocape_coordinate_system.CoordinateValue.iri)))
        graph.add((URIRef(tp_namespace + uk_topo.valueKey + uk_topo.LantitudeKey + bus_context_locator), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri),\
                   URIRef(ontocape_SI_units.m.iri)))
        graph.add((URIRef(tp_namespace + uk_topo.valueKey + uk_topo.LantitudeKey + bus_context_locator), URIRef(ontocape_upper_level_system.numericalValue.iri),\
                   Literal(float(busTopoData[4].strip('\n')))))
            
        graph.add((URIRef(tp_namespace + uk_topo.LongitudeKey + bus_context_locator), URIRef(ontocape_upper_level_system.hasValue.iri),\
                   URIRef(tp_namespace + uk_topo.valueKey + uk_topo.LongitudeKey + bus_context_locator)))
        graph.add((URIRef(tp_namespace + uk_topo.valueKey + uk_topo.LongitudeKey + bus_context_locator), RDF.type, URIRef(ontocape_coordinate_system.CoordinateValue.iri)))
        graph.add((URIRef(tp_namespace + uk_topo.valueKey + uk_topo.LongitudeKey + bus_context_locator), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri),\
                   URIRef(ontocape_SI_units.m.iri)))
        graph.add((URIRef(tp_namespace + uk_topo.valueKey + uk_topo.LongitudeKey + bus_context_locator), URIRef(ontocape_upper_level_system.numericalValue.iri),\
                   Literal(float(busTopoData[3].strip('\n')))))          
        
        counter += 1
        
    return graph, nodeName

"""Add nodes represent Branches"""
def addELineNodes(graph, numOfBranch, branchTopoArray, branchTopoHeader, localQuery, root_node, root_uri, tp_namespace, gridModelNodeSegment, uk_topo):  
    print("****************Adding the triples of ELine of the grid topology.****************")
    nodeName = "ELine"
    
    for header in branchTopoArray[0]:
        if not  header.strip('\n') in branchTopoHeader:   
            raise Exception('The branch topoinfo data header is not matched, please check the data file.')       
       
    counter = 1
    # Number of the Elines (branches)
    Num_Eline = len(branchTopoArray) - 1    
    if numOfBranch == Num_Eline:
        pass
    else:
        print("WARNING: the specified number of the ELine does not equal to the one providede from the branch topoplogy information.")
        print("The generation of the ELine topology will continue but please check the number of the ELine in the runing model.")
    while counter <= Num_Eline:
        branchTopoData = branchTopoArray[counter]
        FromBus_iri = tp_namespace + uk_topo.EquipmentConnection_EBusKey + branchTopoData[0].strip().replace('\n', '').replace('\r', '').zfill(3)
        ToBus_iri = tp_namespace + uk_topo.EquipmentConnection_EBusKey + branchTopoData[1].strip().replace('\n', '').replace('\r', '').zfill(3)
        # print(repr(branchTopoData[0])) # 'repr' used to transfer all the symbols into a printable way
        
        # PowerFlow_ELine node uri
        branch_context_locator = uk_topo.PowerFlow_ELineKey + str(counter).zfill(3) 
        branch_node = tp_namespace + branch_context_locator
        ELine_namespace = gridModelNodeSegment + SLASH + uk_eline_model.ModelELineKey + str(counter).zfill(3) + OWL + HASH
        Eline_context_locator = uk_eline_model.ELineKey + str(counter).zfill(3)
        Eline_node = ELine_namespace + Eline_context_locator
        
        # Link line node with root node and specify its type
        graph.add((URIRef(root_node), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(branch_node)))
        graph.add((URIRef(branch_node), RDF.type, URIRef(ontopowsys_PowSysFunction.PowerFlow.iri)))
        # link PowerFlow_ELine node with EBus node
        graph.add((URIRef(branch_node), URIRef(ontoecape_technical_system.isRealizedBy.iri), URIRef(Eline_node)))
        graph.add((URIRef(Eline_node), RDF.type, URIRef(ontopowsys_PowSysRealization.OverheadLine.iri)))
        # link with leaves(from) bus and enters(to) bus
        graph.add((URIRef(branch_node), URIRef(ontocape_network_system.leaves.iri), URIRef(FromBus_iri)))
        graph.add((URIRef(branch_node), URIRef(ontocape_network_system.enters.iri), URIRef(ToBus_iri)))
        # link buses with its hasOutput and hasInput
        graph.add((URIRef(FromBus_iri), URIRef(ontocape_network_system.hasOutput.iri), URIRef(branch_node)))
        graph.add((URIRef(ToBus_iri), URIRef(ontocape_network_system.hasInput.iri), URIRef(branch_node)))
              
        # Adding the nodes about the branches' ShapeRepresentation (length) and PARALLEL_CONNECTIONS (the different voltage level's branches) 
        # Query the FromBus and Tobus GPS location, gpsArray = [FromBus_long, FromBus_lat, Tobus_long, Tobus_lat]
        
        gpsArray = list(query_topo.queryConnectedBusGPS(endpoint_label, uk_topo.SleepycatStoragePath, FromBus_iri, ToBus_iri, localQuery))
        if len(gpsArray) == 0:
            raise Exception('The gpsArray is empty. Please check whether the query is performanced successflly or whether the bus topology graphs are uploaded to Blazegraph.')
        Eline_length = DistanceBasedOnGPSLocation(gpsArray[0])
        # print(Eline_length)      
        # ELine ShapeRepresentation node uri
        ELine_shape_node = ELine_namespace + uk_topo.ShapeKey + Eline_context_locator
        ELine_length_node = ELine_namespace + uk_topo.LengthKey + Eline_context_locator
        value_ELine_length_node = ELine_namespace + uk_topo.valueKey + uk_topo.LengthKey + Eline_context_locator
        
        # add ShapeRepresentation (length) of ELine
        graph.add((URIRef(Eline_node), URIRef(ontocape_geometry.hasShapeRepresentation.iri), URIRef(ELine_shape_node)))
        graph.add((URIRef(ELine_shape_node), RDF.type, URIRef(ontocape_geometry.Cylinder.iri)))
        graph.add((URIRef(ELine_shape_node), URIRef(ontocape_geometry.has_length.iri), URIRef(ELine_length_node)))
        graph.add((URIRef(ELine_length_node), RDF.type, URIRef(ontocape_geometry.Height.iri)))
        # value of branch length
        graph.add((URIRef(ELine_length_node), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(value_ELine_length_node)))
        graph.add((URIRef(value_ELine_length_node), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
        graph.add((URIRef(value_ELine_length_node), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(ontocape_derived_SI_units.km.iri)))
        graph.add((URIRef(value_ELine_length_node), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(Eline_length))))
        
        if len(branchTopoHeader) > 2: # indicates that there are PARALLEL_CONNECTIONS of the branches topology            
            headerIndex = 2
            while headerIndex < len(branchTopoHeader):
                # parallel connections node uri   
                OHLKey = uk_topo.OHLKey + str(branchTopoHeader[headerIndex].strip('\n')) + KV + UNDERSCORE
                PARALLEL_CONNECTIONS = ELine_namespace + OHLKey + Eline_context_locator
                num_of_PARALLEL_CONNECTIONS = ELine_namespace + uk_topo.NumberOfKey + OHLKey + Eline_context_locator
                # the parallel conection of each branch (400kV and 275kV)
                graph.add((URIRef(Eline_node), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(PARALLEL_CONNECTIONS)))
                graph.add((URIRef(PARALLEL_CONNECTIONS), RDF.type, URIRef(ontopowsys_PowSysRealization.OverheadLine.iri)))
                graph.add((URIRef(PARALLEL_CONNECTIONS), URIRef(t_box.ontopowsys_PowSysRealization + 'hasVoltageLevel'), Literal(str(branchTopoHeader[headerIndex].strip('\n')) + KV)))
                
                graph.add((URIRef(PARALLEL_CONNECTIONS), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(num_of_PARALLEL_CONNECTIONS)))
                graph.add((URIRef(num_of_PARALLEL_CONNECTIONS), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
                graph.add((URIRef(num_of_PARALLEL_CONNECTIONS), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(int(branchTopoData[headerIndex]))))
                
                headerIndex +=1

        counter += 1
        
    return graph, nodeName 

"""Add nodes represent Branches"""
#generatorClusterFunctionName is the name of the cluster function in the class generatorCluster 
def addEGenNodes(graph, numOfBus, generatorClusterFunctionName, ConjunctiveGraph, modelFactorArrays, localQuery, root_node, root_uri, tp_namespace, uk_topo): 
    print("Adding the triples of EGen of the grid topology.")
    print('#########START addEGenNodes for', numOfBus, '-bus model##############')
    nodeName = "EGen"
    counter = 1 
    if modelFactorArrays[0] == ukmf.headerModelFactor:
        pass
    else:
        print('The bus model factor data header is not matched, please check the data file')
        return None
   
    # query the Bus Topological Information and power plant attributes  
    #### Bus: Bus_node, Region, Bus_lat, Bus_lon ####
    res_queryBusTopologicalInformation = list(query_topo.queryBusTopologicalInformation(numOfBus, ConjunctiveGraph, localQuery, endpoint_label))
    #### power plant: PowerGenerator, Region, lat, lon, PrimaryFuel, GenerationTechnology ####
    res_queryPowerPlantAttributes = list(query_topo.queryPowerPlantAttributes(ConjunctiveGraph, localQuery, endpoint_label))
    
    # create an instance of class generatorCluster
    gc = genCluster.generatorCluster()
    # get the cluster method via getattr function 
    genClusterMethod = getattr(gc, generatorClusterFunctionName)
    # pass the arrguments to the cluster method
    bus_generator_assignment_list = genClusterMethod(res_queryBusTopologicalInformation, res_queryPowerPlantAttributes)
                             
    for busGen in bus_generator_assignment_list: # busGen: busGen[0]: generator; busGen[1]: bus node; busGen[2]: PrimaryFuel; busGen[3]: GenerationTechnology
        # build up iris
        generator_context_locator = uk_topo.PowerGeneration_EGenKey + str(counter).zfill(3) 
        generator_node = tp_namespace + generator_context_locator
        EGen_namespace = UKDT.nodeURIGenerator(3, dt.powerGridModel, numOfBus).split(OWL)[0] + SLASH + uk_egen_model.ModelEGenKey + str(counter).zfill(3) + OWL + HASH
        EGen_context_locator = uk_egen_model.EGenKey + str(counter).zfill(3)
        EGen_node = EGen_namespace + EGen_context_locator
        
        # link the generator_node, EGen_node and their PowerGenerator of the power plant
        graph.add((URIRef(root_node), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(generator_node)))
        graph.add((URIRef(generator_node), URIRef(meta_model_topology.isConnectedTo.iri), URIRef(busGen[1])))
        graph.add((URIRef(generator_node), RDF.type, URIRef(ontopowsys_PowSysFunction.PowerGeneration.iri)))
        graph.add((URIRef(generator_node), URIRef(ontoecape_technical_system.isRealizedBy.iri), URIRef(EGen_node)))
        graph.add((URIRef(EGen_node), RDF.type, URIRef(ontopowsys_PowSysRealization.PowerGenerator.iri)))
        graph.add((URIRef(EGen_node), URIRef(ontocape_upper_level_system.isExclusivelySubsystemOf.iri), URIRef(busGen[0])))
        # Add attributes: FixedOperatingCostandMaintenanceCost, VariableOperatingCostandMaintenanceCost, FuelCost, CarbonFactor
        graph = AddCostAttributes(graph, counter, busGen[2].split('#')[1], busGen[3].split('#')[1], modelFactorArrays, numOfBus, uk_topo)
        
        counter += 1               
    return graph, nodeName   


"""This function is designed for added the attributes to the generator cost function"""
def AddCostAttributes(graph, counter, fuelType, genTech, modelFactorArrays, numOfBus, uk_topo): 
    if fuelType in ukmf.Renewable:
        fuelTypeIndex = 1     # fuelTypeIndex, 1: Renewable, 2: Nuclear, 3: Bio, 4: Coal, 5: CCGT, 6: OCGT, 7: OtherPeaking
    elif fuelType == 'Nuclear':
        fuelTypeIndex = 2
    elif fuelType in ukmf.Bio:
        fuelTypeIndex = 3
    elif fuelType == 'Coal': 
        fuelTypeIndex = 4
    elif genTech in ukmf.CCGT:  
        fuelTypeIndex = 5
    elif genTech in ukmf.OCGT:  
        fuelTypeIndex = 6
    else:
        fuelTypeIndex = 7  

    EGen_namespace = UKDT.nodeURIGenerator(3, dt.powerGridModel, numOfBus).split(OWL)[0] + SLASH + uk_egen_model.ModelEGenKey + str(counter).zfill(3) + OWL + HASH
    EGen_context_locator = uk_egen_model.EGenKey + str(counter).zfill(3)
    EGen_node = EGen_namespace + EGen_context_locator
    EGen_FixedOandMCost_node = EGen_namespace + ukmf.headerModelFactor[1] + UNDERSCORE + EGen_context_locator
    value_EGen_FixedOandMCost_node = EGen_namespace + uk_topo.valueKey  + ukmf.headerModelFactor[1] + UNDERSCORE + EGen_context_locator
    EGen_VarOandMCost_node = EGen_namespace + ukmf.headerModelFactor[2] + UNDERSCORE + EGen_context_locator
    value_EGen_VarOandMCost_node = EGen_namespace + uk_topo.valueKey  + ukmf.headerModelFactor[2] + UNDERSCORE + EGen_context_locator
    EGen_FuelCost_node = EGen_namespace + ukmf.headerModelFactor[3] + UNDERSCORE + EGen_context_locator
    value_EGen_FuelCost_node = EGen_namespace + uk_topo.valueKey  + ukmf.headerModelFactor[3] + UNDERSCORE + EGen_context_locator
    EGen_CarbonFactor_node = EGen_namespace + ukmf.headerModelFactor[4].strip('\n').strip('&')  + UNDERSCORE + modelFactorArrays[fuelTypeIndex][0] + UNDERSCORE + EGen_context_locator
    value_EGen_CarbonFactor_node = EGen_namespace + uk_topo.valueKey  + ukmf.headerModelFactor[4].strip('\n').strip('&') + UNDERSCORE + modelFactorArrays[fuelTypeIndex][0] + UNDERSCORE + EGen_context_locator
    
    # add FixedOperatingCostandMaintenanceCost
    graph.add((URIRef(EGen_node), URIRef(ontopowsys_PowSysPerformance.hasFixedMaintenanceCost.iri), URIRef(EGen_FixedOandMCost_node)))
    graph.add((URIRef(EGen_FixedOandMCost_node), RDF.type, URIRef(ontopowsys_PowSysPerformance.FixMaintenanceCosts.iri)))
    graph.add((URIRef(EGen_FixedOandMCost_node), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(value_EGen_FixedOandMCost_node)))
    graph.add((URIRef(value_EGen_FixedOandMCost_node), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
    graph.add((URIRef(value_EGen_FixedOandMCost_node), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(t_box.ontocape_derived_SI_units + 'GBP_per_MWh'))) # undefined unit
    graph.add((URIRef(value_EGen_FixedOandMCost_node), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(modelFactorArrays[fuelTypeIndex][1]))))
    # add VariableOperatingCostandMaintenanceCost  
    graph.add((URIRef(EGen_node), URIRef(ontopowsys_PowSysPerformance.hasCost.iri), URIRef(EGen_VarOandMCost_node)))
    graph.add((URIRef(EGen_VarOandMCost_node), RDF.type, URIRef(ontopowsys_PowSysPerformance.OperationalExpenditureCosts.iri)))
    graph.add((URIRef(EGen_VarOandMCost_node), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(value_EGen_VarOandMCost_node)))
    graph.add((URIRef(value_EGen_VarOandMCost_node), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
    graph.add((URIRef(value_EGen_VarOandMCost_node), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(t_box.ontocape_derived_SI_units + 'GBP_per_MWh'))) # undefined unit
    graph.add((URIRef(value_EGen_VarOandMCost_node), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(modelFactorArrays[fuelTypeIndex][2]))))
    # add FuelCost  
    graph.add((URIRef(EGen_node), URIRef(ontopowsys_PowSysPerformance.hasFuelCost.iri), URIRef(EGen_FuelCost_node)))
    graph.add((URIRef(EGen_FuelCost_node), RDF.type, URIRef(ontopowsys_PowSysPerformance.FuelCosts.iri)))
    graph.add((URIRef(EGen_FuelCost_node), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(value_EGen_FuelCost_node)))
    graph.add((URIRef(value_EGen_FuelCost_node), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
    graph.add((URIRef(value_EGen_FuelCost_node), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(t_box.ontocape_derived_SI_units + 'GBP_per_MWh'))) # undefined unit
    graph.add((URIRef(value_EGen_FuelCost_node), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(modelFactorArrays[fuelTypeIndex][3]))))
    # add CarbonFactor 
    graph.add((URIRef(EGen_node), URIRef(ontoeip_powerplant.hasEmissionFactor.iri), URIRef(EGen_CarbonFactor_node)))
    graph.add((URIRef(EGen_CarbonFactor_node), RDF.type, URIRef(t_box.ontoeip_powerplant + 'CO2EmissionFactor'))) # undefined CO2EmissionFactor
    graph.add((URIRef(EGen_CarbonFactor_node), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(value_EGen_CarbonFactor_node)))
    graph.add((URIRef(value_EGen_CarbonFactor_node), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
    graph.add((URIRef(value_EGen_CarbonFactor_node), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(t_box.ontocape_derived_SI_units + 'GBP_per_MWh'))) # undefined unit
    graph.add((URIRef(value_EGen_CarbonFactor_node), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(modelFactorArrays[fuelTypeIndex][4].strip('\n')))))
   
    return graph

if __name__ == '__main__': 
    # createTopologyGraph('default', False, 10, 14, addEBusNodes, None, None, 'sameRegionWithBus', ["275", "400"], None, True)
    # createTopologyGraph('default', False, 10, 14, None, addELineNodes, None, 'sameRegionWithBus', ["275", "400"], None, True)
    createTopologyGraph('default', False, 10, 14, None, None, addEGenNodes, 'sameRegionWithBus', ["275", "400"], None, True)
    
    # createTopologyGraph('default', False, 29, 99, addEBusNodes, None, None, 'closestBus', [], None, True)
    # createTopologyGraph('default', False, 29, 99, None, addELineNodes, None, 'closestBus', [], None, True)
    createTopologyGraph('default', False, 29, 99, None, None, addEGenNodes, 'closestBus', [], None, True) 
    
    print('**************Terminated**************')