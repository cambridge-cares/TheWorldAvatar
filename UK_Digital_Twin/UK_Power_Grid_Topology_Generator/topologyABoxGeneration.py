##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 08 May 2021          #
##########################################

"""This module is designed to generate and update the A-box of UK power grid topology graph."""

import os
import owlready2
# import numpy as np
from rdflib.extras.infixowl import OWL_NS
from rdflib import Graph, URIRef, Literal, ConjunctiveGraph
from rdflib.namespace import RDF
from rdflib.plugins.sleepycat import Sleepycat
from rdflib.store import NO_STORE
import sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from UK_Digital_Twin_Package import UKDigitalTwinTBox as T_BOX
from UK_Digital_Twin_Package import TopologicalInformationProperty as TopoInfo
from UK_Digital_Twin_Package import UKPowerGridTopology as UK_Topo
from UK_Digital_Twin_Package import UKPowerGridModel as UK_PG
from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs, selectStoragePath, readFile
from UK_Digital_Twin_Package.DistanceCalculator import DistanceBasedOnGPDLocation
import SPARQLQueriesUsedInTopologyABox as query_topo

"""Notation used in URI construction"""
HASH = '#'
SLASH = '/'
UNDERSCORE = '_'
OWL = '.owl'

"""Graph store"""
store = 'default'

"""Create an /instance of Class UKDigitalTwin"""
dt = UKDT.UKDigitalTwin()

"""Create an object of Class UKDigitalTwinTBox"""
t_box = T_BOX.UKDigitalTwinTBox()

"""Create an object of Class UKPowerGridTopology"""
uk_topo = UK_Topo.UKPowerGridTopology()

"""Create an object of Class TopologicalInformationProperty"""
topo_info = TopoInfo.TopologicalInformation()

"""Create an object of Class UKPowerGridModel"""
uk_ebus_model = UK_PG.UKEbusModel()
uk_eline_model = UK_PG.UKElineModel()

"""Graph store"""
# store = 'default'
store = Sleepycat()
store.__open = True
store.context_aware = True

"""Sleepycat storage path"""
defaultPath_Sleepycat = uk_topo.SleepycatStoragePath

"""Root_node and root_uri"""
root_node = UKDT.namedGraphURIGenerator(3, dt.gridTopology, 10)
root_uri = root_node.split('#')[0]

"""NameSpace"""
tp_namespace = root_uri + HASH

"""T-Box URI"""
ontocape_upper_level_system     = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
ontocape_derived_SI_units       = owlready2.get_ontology(t_box.ontocape_derived_SI_units).load()
ontoecape_space_and_time_extended = owlready2.get_ontology(t_box.ontoecape_space_and_time_extended).load()
# ontoeip_system_function         = owlready2.get_ontology(t_box.ontoeip_system_function).load()
ontocape_network_system         = owlready2.get_ontology(t_box.ontocape_network_system).load()
ontopowsys_PowSysFunction       = owlready2.get_ontology(t_box.ontopowsys_PowSysFunction).load()
ontoecape_technical_system      = owlready2.get_ontology(t_box.ontoecape_technical_system).load()
ontopowsys_PowSysRealization    = owlready2.get_ontology(t_box.ontopowsys_PowSysRealization).load()
ontocape_coordinate_system      = owlready2.get_ontology(t_box.ontocape_coordinate_system).load()
ontoecape_space_and_time        = owlready2.get_ontology(t_box.ontoecape_space_and_time).load()
ontocape_physical_dimension     = owlready2.get_ontology(t_box.ontocape_physical_dimension).load()
ontocape_SI_units               = owlready2.get_ontology(t_box.ontocape_SI_units).load()
ontocape_geometry               = owlready2.get_ontology(t_box.ontocape_geometry).load()

"""Data Array"""
busInfoArrays = readFile(topo_info.Topo_10_bus['BusInfo'])
branchTopoInfoArrays = readFile(topo_info.Topo_10_bus['BranchInfo'])
branchPropArrays = readFile(topo_info.Topo_10_bus['BranchProperty'])

"""User specified folder path"""
filepath = None
userSpecified = False

### Functions ### 

"""Main function: create the sub graph represents the Topology"""
def createTopologyGraph(store, updateLocalOWLFile = True):
    print('Create the graph for ', topo_info.Topo_10_bus['Name'])
    global filepath, userSpecified, defaultPath_Sleepycat   
    if isinstance(store, Sleepycat): 
        cg_topo_ukec = ConjunctiveGraph(store=store)
        sl = cg_topo_ukec.open(defaultPath_Sleepycat, create = False)
        if sl == NO_STORE:
            print('Cannot find the specified sleepycat store')
    
    # create a named graph
    g = Graph(store = store, identifier = URIRef(root_uri))
       
    # Import T-boxes
    g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_network_system)))
    g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_upper_level_system)))
    
    # Add root node type and the connection between root node and its father node   
    g.add((URIRef(root_node), URIRef(ontocape_upper_level_system.isExclusivelySubsystemOf.iri),\
                        URIRef(UKDT.namedGraphURIGenerator(2, dt.gridTopology, None))))
    g.add((URIRef(root_node), RDF.type, URIRef(ontocape_network_system.NetworkSystem.iri)))
       
    g = addEBusNodes(g, busInfoArrays) 
    
    g = addELineNodes(g, branchTopoInfoArrays, branchPropArrays)
    
    g = addEGenNodes(g, cg_topo_ukec)
    
    # generate/update OWL files
    if updateLocalOWLFile == True:    
        # specify the owl file storage path
        defaultStoredPath = uk_topo.StoreGeneratedOWLs + 'UK_' + topo_info.Topo_10_bus['Name'] + OWL #default path
    
        # Store/update the generated owl files      
        if os.path.exists(uk_topo.StoreGeneratedOWLs) and not userSpecified:
            print('****Non user specified storage path, will use the default storage path****')
            storeGeneratedOWLs(g, defaultStoredPath)
    
        elif filepath == None:
            print('****Needs user to specify a storage path****')
            filepath = selectStoragePath()
            filepath_ = filepath + '\\' + 'UK_' + topo_info.Topo_10_bus['Name'] + OWL
            storeGeneratedOWLs(g, filepath_)
        else: 
            filepath_ = filepath + '\\' +'UK_' + topo_info.Topo_10_bus['Name'] + OWL
            storeGeneratedOWLs(g, filepath_)
    if isinstance(store, Sleepycat):  
        cg_topo_ukec.close()       
    return g

"""Add nodes represent Buses"""
def addEBusNodes(graph, dataArray):  
    if dataArray[0] == topo_info.headerBusTopologicalInformation:
        pass
    else:
        print('The bus topoinfo data header is not matched, please check the data file')
        return None
    counter = 1
    while counter < len(dataArray):
        # print('Counter is ', counter)
        busTopoData = dataArray[counter]
        
        # the EquipmentConnection_EBus node uri
        bus_context_locator = uk_topo.EquipmentConnection_EBusKey + busTopoData[0].zfill(3) 
        bus_node = tp_namespace + bus_context_locator
        Ebus_context_locator = uk_ebus_model.EBusKey + busTopoData[0].zfill(3) + UNDERSCORE + busTopoData[1]
        Model_EBus_context_locator = uk_ebus_model.ModelEBusKey + busTopoData[0].zfill(3) + UNDERSCORE + busTopoData[1]
        Ebus_node = UKDT.namedGraphURIGenerator(3, dt.powerGridModel, 10).split(OWL)[0] + SLASH + Model_EBus_context_locator + OWL + HASH + Ebus_context_locator
        # Link bus node with root node and specify its type
        graph.add((URIRef(root_node), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(bus_node)))
        graph.add((URIRef(bus_node), RDF.type, URIRef(ontopowsys_PowSysFunction.PowerEquipmentConnection.iri)))
        # link EquipmentConnection_EBus node with EBus node
        graph.add((URIRef(bus_node), URIRef(ontoecape_technical_system.isRealizedBy.iri), URIRef(Ebus_node)))
        graph.add((URIRef(Ebus_node), RDF.type, URIRef(ontopowsys_PowSysRealization.BusNode.iri)))
        # Add location and GPS of bus
        graph.add((URIRef(bus_node), URIRef(ontocape_upper_level_system.hasAddress.iri), URIRef(t_box.dbr + busTopoData[1]))) # region, rdf.type is t_box.dbo + 'Region'
        graph.add((URIRef(bus_node), URIRef(ontocape_upper_level_system.hasAddress.iri), URIRef(busTopoData[2]))) # city, rdf.type is ontoecape_space_and_time_extended.AddressArea
        graph.add((URIRef(bus_node), URIRef(ontoecape_space_and_time_extended.hasGISCoordinateSystem.iri), URIRef(tp_namespace + uk_topo.CoordinateSystemKey + bus_context_locator)))
        graph.add((URIRef(tp_namespace + uk_topo.CoordinateSystemKey + bus_context_locator), RDF.type, URIRef(ontoecape_space_and_time_extended.ProjectedCoordinateSystem.iri)))       
        graph.add((URIRef(tp_namespace + uk_topo.CoordinateSystemKey + bus_context_locator), URIRef(ontoecape_space_and_time_extended.hasProjectedCoordinate_y.iri),\
                   URIRef(tp_namespace + uk_topo.LongitudeKey + bus_context_locator)))
        graph.add((URIRef(tp_namespace + uk_topo.CoordinateSystemKey + bus_context_locator), URIRef(ontoecape_space_and_time_extended.hasProjectedCoordinate_x.iri),\
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
                   Literal(float(busTopoData[3].strip('\n')))))
            
        graph.add((URIRef(tp_namespace + uk_topo.LongitudeKey + bus_context_locator), URIRef(ontocape_upper_level_system.hasValue.iri),\
                   URIRef(tp_namespace + uk_topo.valueKey + uk_topo.LongitudeKey + bus_context_locator)))
        graph.add((URIRef(tp_namespace + uk_topo.valueKey + uk_topo.LongitudeKey + bus_context_locator), RDF.type, URIRef(ontocape_coordinate_system.CoordinateValue.iri)))
        graph.add((URIRef(tp_namespace + uk_topo.valueKey + uk_topo.LongitudeKey + bus_context_locator), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri),\
                   URIRef(ontocape_SI_units.m.iri)))
        graph.add((URIRef(tp_namespace + uk_topo.valueKey + uk_topo.LongitudeKey + bus_context_locator), URIRef(ontocape_upper_level_system.numericalValue.iri),\
                   Literal(float(busTopoData[4].strip('\n')))))        
        counter += 1
    return graph

"""Add nodes represent Branches"""
def addELineNodes(graph, branchTopoArray, branchPropArray):  
    if branchTopoArray[0] == topo_info.headerBranchTopologicalInformation and branchPropArray[0] == topo_info.headerBranchProperty:
        pass
    else:
        print('The branch topoinfo data header is not matched, please check the data file')
        return None   
    counter = 1
    # Number of the Elines (branches)
    Num_Eline = len(branchTopoArray) -1    
    while counter <= Num_Eline:
        branchTopoData = branchTopoArray[counter]
        FromBus_iri = tp_namespace + uk_topo.EquipmentConnection_EBusKey + branchTopoData[0].zfill(3)
        ToBus_iri = tp_namespace + uk_topo.EquipmentConnection_EBusKey + branchTopoData[1].zfill(3)       
       
        # Query the FromBus and Tobus GPS location, gpsArray = [FromBus_long,FromBus_lat, Tobus_long, Tobus_lat]
        gpsArray = []
        res_busGPS = list(query_topo.queryBusGPS(graph, FromBus_iri, ToBus_iri))
        for n in str(res_busGPS[0]).split(')),'):
            gps_ = n.split("\'")[1]
            gpsArray.append(gps_)
        # Calculate the Eline length
        Eline_length = DistanceBasedOnGPDLocation(gpsArray)
        
        # PowerFlow_ELine node uri
        branch_context_locator = uk_topo.PowerFlow_ELineKey + str(counter).zfill(3) 
        branch_node = tp_namespace + branch_context_locator
        ELine_namespace = UKDT.namedGraphURIGenerator(3, dt.powerGridModel, 10).split(OWL)[0] + SLASH + uk_eline_model.ModelELineKey + str(counter).zfill(3) + OWL + HASH
        Eline_context_locator = uk_eline_model.ELineKey + str(counter).zfill(3)
        Eline_node = ELine_namespace + Eline_context_locator
        ELine_shape_node = ELine_namespace + uk_eline_model.ShapeKey + Eline_context_locator
        ELine_length_node = ELine_namespace + uk_eline_model.LengthKey + Eline_context_locator
        value_ELine_length_node = ELine_namespace + uk_topo.valueKey + uk_eline_model.LengthKey + Eline_context_locator
        PARALLEL_CONNECTIONS_400kV = ELine_namespace + uk_eline_model.OHL400kVKey + Eline_context_locator
        PARALLEL_CONNECTIONS_275kV = ELine_namespace + uk_eline_model.OHL275kVKey + Eline_context_locator
        num_of_PARALLEL_CONNECTIONS_400kV = ELine_namespace + uk_topo.NumberOfKey + uk_eline_model.OHL400kVKey + Eline_context_locator
        num_of_PARALLEL_CONNECTIONS_275kV = ELine_namespace + uk_topo.NumberOfKey + uk_eline_model.OHL275kVKey + Eline_context_locator
        
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
        # the parallel conection of each branch (400kV and 275kV)
        graph.add((URIRef(Eline_node), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(PARALLEL_CONNECTIONS_400kV)))
        graph.add((URIRef(Eline_node), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(PARALLEL_CONNECTIONS_275kV)))
        graph.add((URIRef(PARALLEL_CONNECTIONS_400kV), RDF.type, URIRef(ontopowsys_PowSysRealization.OverheadLine.iri)))
        graph.add((URIRef(PARALLEL_CONNECTIONS_275kV), RDF.type, URIRef(ontopowsys_PowSysRealization.OverheadLine.iri)))
        
        graph.add((URIRef(PARALLEL_CONNECTIONS_400kV), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(num_of_PARALLEL_CONNECTIONS_400kV)))
        graph.add((URIRef(num_of_PARALLEL_CONNECTIONS_400kV), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
        graph.add((URIRef(num_of_PARALLEL_CONNECTIONS_400kV), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(int(branchTopoData[2]))))
        
        graph.add((URIRef(PARALLEL_CONNECTIONS_275kV), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(num_of_PARALLEL_CONNECTIONS_275kV)))
        graph.add((URIRef(num_of_PARALLEL_CONNECTIONS_275kV), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
        graph.add((URIRef(num_of_PARALLEL_CONNECTIONS_275kV), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(int(branchTopoData[3]))))

        counter += 1
        
    return graph   

#TODO: addEGenNodes func
"""Add nodes represent Branches"""
def addEGenNodes(graph, ConjunctiveGraph):  
    
    res_BusLocatedRegion= list(query_topo.queryBusLocation(ConjunctiveGraph))
    print('#######################')
    for n in res_BusLocatedRegion:
        print(n)
    
    # if branchTopoArray[0] == topo_info.headerBranchTopologicalInformation and branchPropArray[0] == topo_info.headerBranchProperty:
    #     pass
    # else:
    #     print('The branch topoinfo data header is not matched, please check the data file')
    #     return None   
    # counter = 1
    # # Number of the Elines (branches)
    # Num_Eline = len(branchTopoArray) -1    
    # while counter <= 1: # Num_Eline:
    #     branchTopoData = branchTopoArray[counter]
    #     FromBus_iri = tp_namespace + uk_topo.EquipmentConnection_EBusKey + branchTopoData[0].zfill(3)
    #     ToBus_iri = tp_namespace + uk_topo.EquipmentConnection_EBusKey + branchTopoData[1].zfill(3)       
       
    #     # Query the FromBus and Tobus GPS location, gpsArray = [FromBus_long,FromBus_lat, Tobus_long, Tobus_lat]
    #     gpsArray = []
    #     res_busGPS = list(query_topo.queryBusGPS(graph, FromBus_iri, ToBus_iri))
    #     for n in str(res_busGPS[0]).split(')),'):
    #         gps_ = n.split("\'")[1]
    #         gpsArray.append(gps_)
    #     # Calculate the Eline length
    #     Eline_length = DistanceBasedOnGPDLocation(gpsArray)
        
    #     # PowerFlow_ELine node uri
    #     branch_context_locator = uk_topo.PowerFlow_ELineKey + str(counter).zfill(3) 
    #     branch_node = tp_namespace + branch_context_locator
    #     ELine_namespace = UKDT.namedGraphURIGenerator(3, dt.powerGridModel, 10).split(OWL)[0] + SLASH + uk_eline_model.ModelELineKey + str(counter).zfill(3) + OWL + HASH
    #     Eline_context_locator = uk_eline_model.ELineKey + str(counter).zfill(3)
    #     Eline_node = ELine_namespace + Eline_context_locator
    #     ELine_shape_node = ELine_namespace + uk_eline_model.ShapeKey + Eline_context_locator
    #     ELine_length_node = ELine_namespace + uk_eline_model.LengthKey + Eline_context_locator
    #     value_ELine_length_node = ELine_namespace + uk_topo.valueKey + uk_eline_model.LengthKey + Eline_context_locator
    #     PARALLEL_CONNECTIONS_400kV = ELine_namespace + uk_eline_model.OHL400kVKey + Eline_context_locator
    #     PARALLEL_CONNECTIONS_275kV = ELine_namespace + uk_eline_model.OHL275kVKey + Eline_context_locator
    #     num_of_PARALLEL_CONNECTIONS_400kV = ELine_namespace + uk_topo.NumberOfKey + uk_eline_model.OHL400kVKey + Eline_context_locator
    #     num_of_PARALLEL_CONNECTIONS_275kV = ELine_namespace + uk_topo.NumberOfKey + uk_eline_model.OHL275kVKey + Eline_context_locator
        
    #     # Link line node with root node and specify its type
    #     graph.add((URIRef(root_node), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(branch_node)))
    #     graph.add((URIRef(branch_node), RDF.type, URIRef(ontopowsys_PowSysFunction.PowerFlow.iri)))
    #     # link PowerFlow_ELine node with EBus node
    #     graph.add((URIRef(branch_node), URIRef(ontoecape_technical_system.isRealizedBy.iri), URIRef(Eline_node)))
    #     graph.add((URIRef(Eline_node), RDF.type, URIRef(ontopowsys_PowSysRealization.OverheadLine.iri)))
    #     # link with leaves(from) bus and enters(to) bus
    #     graph.add((URIRef(branch_node), URIRef(ontocape_network_system.leaves.iri), URIRef(FromBus_iri)))
    #     graph.add((URIRef(branch_node), URIRef(ontocape_network_system.enters.iri), URIRef(ToBus_iri)))
    #     # link buses with its hasOutput and hasInput
    #     graph.add((URIRef(FromBus_iri), URIRef(ontocape_network_system.hasOutput.iri), URIRef(branch_node)))
    #     graph.add((URIRef(ToBus_iri), URIRef(ontocape_network_system.hasInput.iri), URIRef(branch_node)))
    #     # add ShapeRepresentation (length) of ELine
    #     graph.add((URIRef(Eline_node), URIRef(ontocape_geometry.hasShapeRepresentation.iri), URIRef(ELine_shape_node)))
    #     graph.add((URIRef(ELine_shape_node), RDF.type, URIRef(ontocape_geometry.Cylinder.iri)))
    #     graph.add((URIRef(ELine_shape_node), URIRef(ontocape_geometry.has_length.iri), URIRef(ELine_length_node)))
    #     graph.add((URIRef(ELine_length_node), RDF.type, URIRef(ontocape_geometry.Height.iri)))
    #     # value of branch length
    #     graph.add((URIRef(ELine_length_node), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(value_ELine_length_node)))
    #     graph.add((URIRef(value_ELine_length_node), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
    #     graph.add((URIRef(value_ELine_length_node), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(ontocape_derived_SI_units.km.iri)))
    #     graph.add((URIRef(value_ELine_length_node), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(Eline_length))))
    #     # the parallel conection of each branch (400kV and 275kV)
    #     graph.add((URIRef(Eline_node), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(PARALLEL_CONNECTIONS_400kV)))
    #     graph.add((URIRef(Eline_node), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(PARALLEL_CONNECTIONS_275kV)))
    #     graph.add((URIRef(PARALLEL_CONNECTIONS_400kV), RDF.type, URIRef(ontopowsys_PowSysRealization.OverheadLine.iri)))
    #     graph.add((URIRef(PARALLEL_CONNECTIONS_275kV), RDF.type, URIRef(ontopowsys_PowSysRealization.OverheadLine.iri)))
        
    #     graph.add((URIRef(PARALLEL_CONNECTIONS_400kV), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(num_of_PARALLEL_CONNECTIONS_400kV)))
    #     graph.add((URIRef(num_of_PARALLEL_CONNECTIONS_400kV), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
    #     graph.add((URIRef(num_of_PARALLEL_CONNECTIONS_400kV), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(int(branchTopoData[2]))))
        
    #     graph.add((URIRef(PARALLEL_CONNECTIONS_275kV), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(num_of_PARALLEL_CONNECTIONS_275kV)))
    #     graph.add((URIRef(num_of_PARALLEL_CONNECTIONS_275kV), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
    #     graph.add((URIRef(num_of_PARALLEL_CONNECTIONS_275kV), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(int(branchTopoData[3]))))

    #     counter += 1
        
    return graph   


if __name__ == '__main__':    
    g = createTopologyGraph(store, False)
    
    # g.open("C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Energy_Consumption\\Sleepycat_UKec", create = False)

    # queryStr3 = """
    #         SELECT DISTINCT ?g
    #         WHERE 
    #             {
    #               graph ?g {?s ?p ?o}
    #             }    
    #         """    
    # qres = g.query(queryStr3)
    # num = 0
    # print('The query results are: ')
    # for n in qres:
    #     print(n) 
    #     num += 1
    # print(num)   
    
    
    # """Close the store"""
    # g.close()
    
    # print('Terminated')

    
    