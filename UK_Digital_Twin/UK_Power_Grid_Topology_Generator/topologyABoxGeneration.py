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
from UK_Digital_Twin_Package import UKPowerPlant as UKpp
from UK_Digital_Twin_Package import UKEnergyConsumption as UKec
from UK_Digital_Twin_Package import CO2FactorAndGenCostFactor as ModelFactor
from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs, selectStoragePath, readFile
from UK_Digital_Twin_Package.GraphStore import LocalGraphStore
from UK_Digital_Twin_Package.DistanceCalculator import DistanceBasedOnGPDLocation

import UK_Power_Grid_Topology_Generator.SPARQLQueriesUsedInTopologyABox as query_topo

"""Notation used in URI construction"""
HASH = '#'
SLASH = '/'
UNDERSCORE = '_'
OWL = '.owl'

"""Graph store"""
store = 'default'

"""Create an instance of Class UKDigitalTwin"""
dt = UKDT.UKDigitalTwin()

"""Create an object of Class UKDigitalTwinTBox"""
t_box = T_BOX.UKDigitalTwinTBox()

"""Create an object of Class UKPowerGridTopology"""
uk_topo = UK_Topo.UKPowerGridTopology()

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

"""Sleepycat storage path"""
defaultPath_Sleepycat = uk_topo.SleepycatStoragePath

"""Remote Endpoint lable"""
powerPlant_Endpoint = ukpp.endpoint['lable']
topology_Endpoint = uk_topo.endpoint['lable']
topology_federated_query_ep = uk_topo.endpoint['queryendpoint_iri']
energyConsumption_federated_query_ep = ukec.endpoint['queryendpoint_iri']

"""Root_node and root_uri"""
root_node = UKDT.nodeURIGenerator(3, dt.gridTopology, 10)
root_uri = root_node.split('#')[0]

"""NameSpace"""
tp_namespace = root_uri + HASH

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
modelFactorArrays = readFile(ukmf.CO2EmissionFactor)

"""User specified folder path"""
filepath = None
userSpecified = False

"""Topology Conjunctive graph identifier"""
topo_cg_id = "http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model"

counter = 1 

### Functions ### 

""" Create the TopologicalInformationProperty Instance by specifying its numOfBus and numOfBranch"""
def createTopologicalInformationPropertyInstance(numOfBus, numOfBranch):
    topo_info = TopoInfo.TopologicalInformation(numOfBus, numOfBranch)
    
    busInfoArrays = readFile(topo_info.BusInfo)
    branchTopoInfoArrays = readFile(topo_info.BranchInfo)
    branchPropArrays = readFile(topo_info.BranchProperty)
    
    return topo_info, busInfoArrays, branchTopoInfoArrays, branchPropArrays

"""Main function: create the sub graph represents the Topology"""
def createTopologyGraph(storeType, localQuery, numOfBus, numOfBranch, addEBusNodes, addELineNodes, addEGenNodes, updateLocalOWLFile = True):
    store = LocalGraphStore(storeType)
    topo_info, busInfoArrays, branchTopoInfoArrays, branchPropArrays = createTopologicalInformationPropertyInstance(numOfBus, numOfBranch)
    print('Create the graph for ', topo_info.EBus_num, ' buses and ', topo_info.ELine_num, ' branches topology' )
    global filepath, userSpecified, defaultPath_Sleepycat
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
    g.add((URIRef(root_node), URIRef(ontocape_upper_level_system.isExclusivelySubsystemOf.iri),\
                        URIRef(UKDT.nodeURIGenerator(2, dt.gridTopology, None))))
    g.add((URIRef(root_node), RDF.type, URIRef(ontocape_network_system.NetworkSystem.iri)))
    
    if addEBusNodes != None:
        g = addEBusNodes(g, topo_info.headerBusTopologicalInformation, busInfoArrays) 
    if addELineNodes != None:    
        g = addELineNodes(g, branchTopoInfoArrays, branchPropArrays, topo_info.headerBranchTopologicalInformation, topo_info.headerBranchProperty, localQuery)
    if addEGenNodes != None:
        g = addEGenNodes(g, cg_topo_ukec, modelFactorArrays, localQuery)
    
    # generate/update OWL files
    if updateLocalOWLFile == True:    
        # specify the owl file storage path
        defaultStoredPath = uk_topo.StoreGeneratedOWLs + 'UK_' + topo_info.Name + OWL #default path
    
        # Store/update the generated owl files      
        if os.path.exists(uk_topo.StoreGeneratedOWLs) and not userSpecified:
            print('****Non user specified storage path, will use the default storage path****')
            storeGeneratedOWLs(g, defaultStoredPath)
    
        elif filepath == None:
            print('****Needs user to specify a storage path****')
            filepath = selectStoragePath()
            filepath_ = filepath + '\\' + 'UK_' + topo_info.Name + OWL
            storeGeneratedOWLs(g, filepath_)
        else: 
            filepath_ = filepath + '\\' +'UK_' + topo_info.Name + OWL
            storeGeneratedOWLs(g, filepath_)
    if isinstance(store, Sleepycat):  
        cg_topo_ukec.close()       
    return


"""Add nodes represent Buses"""
def addEBusNodes(graph, header, dataArray): 
    print('Start adding bus node triples in the topology graph')
    if dataArray[0] == header:
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
        Ebus_node = UKDT.nodeURIGenerator(3, dt.powerGridModel, 10).split(OWL)[0] + SLASH + Model_EBus_context_locator + OWL + HASH + Ebus_context_locator
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
def addELineNodes(graph, branchTopoArray, branchPropArray, branchTopoHeader, branchPropHeader, localQuery):  
    if branchTopoArray[0] == branchTopoHeader and branchPropArray[0] == branchPropHeader:
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
        gpsArray = list(query_topo.queryBusGPS(topology_Endpoint, uk_topo.SleepycatStoragePath, FromBus_iri, ToBus_iri, localQuery))
        Eline_length = DistanceBasedOnGPDLocation(gpsArray[0])
              
        # PowerFlow_ELine node uri
        branch_context_locator = uk_topo.PowerFlow_ELineKey + str(counter).zfill(3) 
        branch_node = tp_namespace + branch_context_locator
        ELine_namespace = UKDT.nodeURIGenerator(3, dt.powerGridModel, 10).split(OWL)[0] + SLASH + uk_eline_model.ModelELineKey + str(counter).zfill(3) + OWL + HASH
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

"""Add nodes represent Branches"""
def addEGenNodes(graph, ConjunctiveGraph, modelFactorArrays, localQuery): 
    global counter
    if modelFactorArrays[0] == ukmf.headerModelFactor:
        pass
    else:
        print('The bus model factor data header is not matched, please check the data file')
        return None
    
    res_BusLocatedRegion= list(query_topo.queryBusLocation(ConjunctiveGraph, localQuery, topology_federated_query_ep, energyConsumption_federated_query_ep))
    print('#########START addEGenNodes##############')
    for bus_location in res_BusLocatedRegion: # bus_location[0]: bus node; bus_location[1]: located region
        bus_location[1] = str(bus_location[1]).replace('http:', 'https:')
        print(bus_location[1])     
        res_powerplant= list(query_topo.queryPowerPlantLocatedInSameRegion(powerPlant_Endpoint, ukpp.SleepycatStoragePath, str(bus_location[1]), localQuery))        
        local_counter = 1
        while local_counter <= len(res_powerplant):
            for pg in res_powerplant: # pg[0]: powerGenerator, pg[1]: PrimaryFuel; pg[2]: GenerationTechnology
                # build up iris
                generator_context_locator = uk_topo.PowerGeneration_EGenKey + str(counter).zfill(3) 
                generator_node = tp_namespace + generator_context_locator
                EGen_namespace = UKDT.nodeURIGenerator(3, dt.powerGridModel, 10).split(OWL)[0] + SLASH + uk_egen_model.ModelEGenKey + str(counter).zfill(3) + OWL + HASH
                EGen_context_locator = uk_egen_model.EGenKey + str(counter).zfill(3)
                EGen_node = EGen_namespace + EGen_context_locator
                
                # link the generator_node, EGen_node and their PowerGenerator of the power plant
                graph.add((URIRef(root_node), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(generator_node)))
                graph.add((URIRef(generator_node), URIRef(meta_model_topology.isConnectedTo.iri), URIRef(bus_location[0])))
                graph.add((URIRef(generator_node), RDF.type, URIRef(ontopowsys_PowSysFunction.PowerGeneration.iri)))
                graph.add((URIRef(generator_node), URIRef(ontoecape_technical_system.isRealizedBy.iri), URIRef(EGen_node)))
                graph.add((URIRef(EGen_node), RDF.type, URIRef(ontopowsys_PowSysRealization.PowerGenerator.iri)))
                graph.add((URIRef(EGen_node), URIRef(ontocape_upper_level_system.isExclusivelySubsystemOf.iri), URIRef(pg[0])))
                # Add attributes: FixedOperatingCostandMaintenanceCost, VariableOperatingCostandMaintenanceCost, FuelCost, CarbonFactor
                if pg[1].split('#')[1] in ukmf.Renewable:
                    graph = AddCostAttributes(graph, counter, 1, modelFactorArrays)
                elif pg[1].split('#')[1] == 'Nuclear':
                    graph = AddCostAttributes(graph, counter, 2, modelFactorArrays)
                elif pg[1].split('#')[1] in ukmf.Bio:
                    graph = AddCostAttributes(graph, counter, 3, modelFactorArrays)
                elif pg[1].split('#')[1] == 'Coal':  
                    graph = AddCostAttributes(graph, counter, 4, modelFactorArrays)
                elif pg[2].split('#')[1] in ukmf.CCGT:  
                    graph = AddCostAttributes(graph, counter, 5, modelFactorArrays)
                elif pg[2].split('#')[1] in ukmf.OCGT:  
                    graph = AddCostAttributes(graph, counter, 6, modelFactorArrays)
                else:
                    graph = AddCostAttributes(graph, counter, 7, modelFactorArrays)
                
                counter += 1               
                local_counter += 1
        # print('Counter is ', counter)
        # print('Local counter is ', local_counter)           
    return graph   

def AddCostAttributes(graph, counter, fuelType, modelFactorArrays): # fuelType, 1: Renewable, 2: Nuclear, 3: Bio, 4: Coal, 5: CCGT, 6: OCGT, 7: OtherPeaking
    EGen_namespace = UKDT.nodeURIGenerator(3, dt.powerGridModel, 10).split(OWL)[0] + SLASH + uk_egen_model.ModelEGenKey + str(counter).zfill(3) + OWL + HASH
    EGen_context_locator = uk_egen_model.EGenKey + str(counter).zfill(3)
    EGen_node = EGen_namespace + EGen_context_locator
    EGen_FixedOandMCost_node = EGen_namespace + ukmf.headerModelFactor[1] + UNDERSCORE + EGen_context_locator
    value_EGen_FixedOandMCost_node = EGen_namespace + uk_topo.valueKey  + ukmf.headerModelFactor[1] + UNDERSCORE + EGen_context_locator
    EGen_VarOandMCost_node = EGen_namespace + ukmf.headerModelFactor[2] + UNDERSCORE + EGen_context_locator
    value_EGen_VarOandMCost_node = EGen_namespace + uk_topo.valueKey  + ukmf.headerModelFactor[2] + UNDERSCORE + EGen_context_locator
    EGen_FuelCost_node = EGen_namespace + ukmf.headerModelFactor[3] + UNDERSCORE + EGen_context_locator
    value_EGen_FuelCost_node = EGen_namespace + uk_topo.valueKey  + ukmf.headerModelFactor[3] + UNDERSCORE + EGen_context_locator
    EGen_CarbonFactor_node = EGen_namespace + ukmf.headerModelFactor[4].strip('\n').strip('&')  + UNDERSCORE + modelFactorArrays[fuelType][0] + UNDERSCORE + EGen_context_locator
    value_EGen_CarbonFactor_node = EGen_namespace + uk_topo.valueKey  + ukmf.headerModelFactor[4].strip('\n').strip('&') + UNDERSCORE + modelFactorArrays[fuelType][0] + UNDERSCORE + EGen_context_locator
    
    # add FixedOperatingCostandMaintenanceCost
    graph.add((URIRef(EGen_node), URIRef(ontopowsys_PowSysPerformance.hasFixedMaintenanceCost.iri), URIRef(EGen_FixedOandMCost_node)))
    graph.add((URIRef(EGen_FixedOandMCost_node), RDF.type, URIRef(ontopowsys_PowSysPerformance.FixMaintenanceCosts.iri)))
    graph.add((URIRef(EGen_FixedOandMCost_node), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(value_EGen_FixedOandMCost_node)))
    graph.add((URIRef(value_EGen_FixedOandMCost_node), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
    graph.add((URIRef(value_EGen_FixedOandMCost_node), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(t_box.ontocape_derived_SI_units + 'GBP_per_MWh'))) # undefined unit
    graph.add((URIRef(value_EGen_FixedOandMCost_node), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(modelFactorArrays[fuelType][1]))))
    # add VariableOperatingCostandMaintenanceCost  
    graph.add((URIRef(EGen_node), URIRef(ontopowsys_PowSysPerformance.hasCost.iri), URIRef(EGen_VarOandMCost_node)))
    graph.add((URIRef(EGen_VarOandMCost_node), RDF.type, URIRef(ontopowsys_PowSysPerformance.OperationalExpenditureCosts.iri)))
    graph.add((URIRef(EGen_VarOandMCost_node), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(value_EGen_VarOandMCost_node)))
    graph.add((URIRef(value_EGen_VarOandMCost_node), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
    graph.add((URIRef(value_EGen_VarOandMCost_node), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(t_box.ontocape_derived_SI_units + 'GBP_per_MWh'))) # undefined unit
    graph.add((URIRef(value_EGen_VarOandMCost_node), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(modelFactorArrays[fuelType][2]))))
    # add FuelCost  
    graph.add((URIRef(EGen_node), URIRef(ontopowsys_PowSysPerformance.hasFuelCost.iri), URIRef(EGen_FuelCost_node)))
    graph.add((URIRef(EGen_FuelCost_node), RDF.type, URIRef(ontopowsys_PowSysPerformance.FuelCosts.iri)))
    graph.add((URIRef(EGen_FuelCost_node), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(value_EGen_FuelCost_node)))
    graph.add((URIRef(value_EGen_FuelCost_node), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
    graph.add((URIRef(value_EGen_FuelCost_node), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(t_box.ontocape_derived_SI_units + 'GBP_per_MWh'))) # undefined unit
    graph.add((URIRef(value_EGen_FuelCost_node), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(modelFactorArrays[fuelType][3]))))
    # add CarbonFactor 
    graph.add((URIRef(EGen_node), URIRef(ontoeip_powerplant.hasEmissionFactor.iri), URIRef(EGen_CarbonFactor_node)))
    graph.add((URIRef(EGen_CarbonFactor_node), RDF.type, URIRef(t_box.ontoeip_powerplant + 'CO2EmissionFactor'))) # undefined CO2EmissionFactor
    graph.add((URIRef(EGen_CarbonFactor_node), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(value_EGen_CarbonFactor_node)))
    graph.add((URIRef(value_EGen_CarbonFactor_node), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
    graph.add((URIRef(value_EGen_CarbonFactor_node), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(t_box.ontocape_derived_SI_units + 'GBP_per_MWh'))) # undefined unit
    graph.add((URIRef(value_EGen_CarbonFactor_node), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(modelFactorArrays[fuelType][4].strip('\n')))))
   
    return graph

if __name__ == '__main__':    
    # createTopologyGraph('sleepycat', False, 10, 14, addEBusNodes, None, None, False)
    # createTopologyGraph('default', False, 10, 14, None, addELineNodes, None, True)
    createTopologyGraph('default', False, 10, 14, None, None, addEGenNodes, False)
    print('Terminated')