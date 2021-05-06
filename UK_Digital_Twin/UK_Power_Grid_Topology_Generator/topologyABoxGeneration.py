##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 05 May 2021          #
##########################################

"""This module is designed to generate and update the A-box of UK energy consumption graph."""

import os
import owlready2
import numpy as np
from rdflib.extras.infixowl import OWL_NS
from rdflib import Graph, URIRef, Literal
from rdflib.namespace import RDF
import sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from UK_Digital_Twin_Package import UKDigitalTwinTBox as T_BOX
from UK_Digital_Twin_Package import TopologicalInformationProperty as TopoInfo
from UK_Digital_Twin_Package import UKPowerGridTopology as UK_Topo
from UK_Digital_Twin_Package import UKPowerGridModel as UK_PG
from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs, selectStoragePath, readFile

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

"""Create an object of Class TopologicalInformationProperty"""
uk_ebus_model = UK_PG.UKEbusModel()

"""Root_node and root_uri"""
root_node = UKDT.namedGraphURIGenerator(3, dt.gridTopology, 10)
root_uri = root_node.split('#')[0]

"""T-Box URI"""
ontocape_upper_level_system     = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
# ontocape_derived_SI_units       = owlready2.get_ontology(t_box.ontocape_derived_SI_units).load()
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

"""Data Array"""
busInfoArrays = readFile(topo_info.Topo_10_bus['BusInfo'])

# """Number of owl files to be generated"""
# fileNum = len(elecConDataArrays) # substruct the first header line 

"""User specified folder path"""
filepath = None
userSpecified = False

### Functions ### 

"""create the sub graph represents the Topology"""
def createTopologyGraph():
    print('Create the graph for ', topo_info.Topo_10_bus['Name'])
    # # check the data file header
    # if elecConDataArrays[0] == engconsump.headerElectricityConsumption:
    #     pass
    global filepath, userSpecified
    # counter = 1
    # print('The counter is:') # starts from 1
    # print(counter)
    
    
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
        
    print(g.serialize(format = 'xml').decode('utf-8'))


    # # Add the official region node
    # numOfRegion = len(engconsump.GovernmentOfficeRegions)
    # counter_region = 0   
    # while (counter_region < numOfRegion): # numOfRegion
    #     region = engconsump.GovernmentOfficeRegions[counter_region]
    #     print('The region is: ')
    #     print(region)
    #     # find the index of region in the data file
    #     _elecConDataArrays = np.array(elecConDataArrays)
    #     index_targetRegion = np.argwhere(_elecConDataArrays == region)[0][0]        
    #     regional_base_uri = root_uri + SLASH + region + OWL # the name of the named graph, will be applied as the identifier in method Graph(), without '#' 
        
    #     # Create rdf graph with identifier, regional nodes are named graphs including its local nodes
    #     graph = Graph('default', URIRef(regional_base_uri)) # graph(store='default', identifier)
    #     graph = addUKElectricityConsumptionTriples(graph, index_targetRegion)       
    #     print('Counter and index_targetRegion are:')
    #     print (counter, index_targetRegion)
        
    #     # Add the local nodes under its regional node
    #     while (counter < index_targetRegion):
    #         graph = addUKElectricityConsumptionTriples(graph, counter, index_targetRegion)
    #         counter += 1
            
    #     # specify the owl file storage path
    #     defaultStoredPath = ukec.StoreGeneratedOWLs + 'UK_energy_consumption_' + region + '_UK' + OWL #default path
    
    #     # Store/update the generated owl files      
    #     if os.path.exists(ukec.StoreGeneratedOWLs) and not userSpecified:
    #         print('****Non user specified storage path, will use the default storage path****')
    #         storeGeneratedOWLs(graph, defaultStoredPath)
    
    #     elif filepath == None:
    #         print('****Needs user to specify a storage path****')
    #         filepath = selectStoragePath()
    #         filepath_ = filepath + '\\' + 'UK_energy_consumption_' + region + '_UK' + OWL
    #         storeGeneratedOWLs(graph, filepath_)
    #     else: 
    #         filepath_ = filepath + '\\' +'UK_energy_consumption_' + region + '_UK' + OWL
    #         storeGeneratedOWLs(graph, filepath_)
        
    #     print('counter_region is: ')
    #     print(counter_region)
        
    #     counter_region += 1               
    # return 

"""Add nodes represent EBus"""
def addEBusNodes(graph, dataArray):  
    if dataArray[0] == topo_info.headerTopologicalInformation:
        pass
    else:
        print('The data is not sufficient, please check the data file')
        return None
    counter = 1
    # define namespace
    tp_namespace = root_uri + HASH
    
    while counter <= 1: # len(dataArray):
        busTopoData = dataArray[counter]
        # the EquipmentConnection_EBus node uri
        bus_context_locator = uk_topo.EquipmentConnection_EBusKey + busTopoData[0].zfill(3) 
        bus_node = tp_namespace + bus_context_locator
        Ebus_context_locator = uk_ebus_model.EBusKey + busTopoData[0].zfill(3) + UNDERSCORE + busTopoData[1]
        Ebus_node = UKDT.namedGraphURIGenerator(3, dt.powerGridModel, 10).split(OWL)[0] + SLASH + Ebus_context_locator + OWL + HASH + Ebus_context_locator
        
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
                   URIRef(tp_namespace + uk_topo.LantitudeKey + bus_context_locator)))
        graph.add((URIRef(tp_namespace + uk_topo.CoordinateSystemKey + bus_context_locator), URIRef(ontoecape_space_and_time_extended.hasProjectedCoordinate_x.iri),\
                   URIRef(tp_namespace + uk_topo.LongitudeKey + bus_context_locator)))
        graph.add((URIRef(tp_namespace + uk_topo.LantitudeKey + bus_context_locator), RDF.type, URIRef(ontoecape_space_and_time.StraightCoordinate.iri)))   
        graph.add((URIRef(tp_namespace + uk_topo.LongitudeKey + bus_context_locator), RDF.type, URIRef(ontoecape_space_and_time.StraightCoordinate.iri)))       
        graph.add((URIRef(tp_namespace + uk_topo.LantitudeKey + bus_context_locator), URIRef(ontocape_upper_level_system.hasDimension.iri), URIRef(ontocape_physical_dimension.length.iri)))
        graph.add((URIRef(tp_namespace + uk_topo.LongitudeKey + bus_context_locator), URIRef(ontocape_upper_level_system.hasDimension.iri), URIRef(ontocape_physical_dimension.length.iri)))
        graph.add((URIRef(tp_namespace + uk_topo.LantitudeKey + bus_context_locator), URIRef(ontocape_coordinate_system.refersToAxis.iri), URIRef(t_box.ontoecape_space_and_time + 'y-axis')))
        graph.add((URIRef(tp_namespace + uk_topo.LongitudeKey + bus_context_locator), URIRef(ontocape_coordinate_system.refersToAxis.iri), URIRef(t_box.ontoecape_space_and_time + 'x-axis')))
        
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

if __name__ == '__main__':    
    # print(len(busInfoArrays))
    # print(UKDT.namedGraphURIGenerator(2, dt.gridTopology, None))
    # createTopologyGraph()
    # print(root_uri + HASH)
    # print(busInfoArrays[1][0].zfill(3))
    # print( root_uri + HASH + uk_topo.EquipmentConnection_EBusKey + busInfoArrays[1][0].zfill(3))
    # print( UKDT.namedGraphURIGenerator(3, dt.powerGridModel, 10) )
    # Ebus_locator = uk_ebus_model.EBusKey + busInfoArrays[1][0].zfill(3) + UNDERSCORE + busInfoArrays[1][1]
    # Ebus_root_node = UKDT.namedGraphURIGenerator(3, dt.powerGridModel, 10).split(OWL)[0] + SLASH + Ebus_locator + OWL + HASH + Ebus_locator
    # print(Ebus_root_node)
    createTopologyGraph()       
        
        # # Check node type: reginal or local nodes
        # if elecConData[0] in engconsump.GovernmentOfficeRegions: # regional node
        #     print('Regional node name is: ' + elecConData[0].strip('\n'))
        #     ec_root_node = root_uri + SLASH + elecConData[0].strip('\n') + OWL + HASH + elecConData[0].strip('\n') # top node of the named graph
        #     ec_namespace = root_uri + SLASH + elecConData[0].strip('\n') + OWL + HASH       
        #     # Import T-boxes
        #     graph.add((graph.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_upper_level_system)))
        #     # Add connection between its father node
        #     graph.add((URIRef(ec_root_node), URIRef(ontocape_upper_level_system.isExclusivelySubsystemOf.iri),\
        #                 URIRef(UKDT.namedGraphURIGenerator(3, dt.energyConsumption, engconsump.VERSION))))
        # elif len(counter) > 1: # local node
        #     print('Local node name is: ' + elecConData[0].strip('\n'))
        #     ec_root_node = root_uri + SLASH + elecConDataArrays[counter[1]][0].strip('\n') + OWL + HASH + elecConData[0].strip('\n') # sub node of the graph identifying the local node
        #     ec_namespace = root_uri + SLASH + elecConDataArrays[counter[1]][0].strip('\n') + OWL + HASH       
        #     # Add connection between its father node
        #     graph.add((URIRef(root_uri + SLASH + elecConDataArrays[counter[1]][0].strip('\n') + OWL + HASH + elecConDataArrays[counter[1]][0].strip('\n')),\
        #                URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(ec_root_node)))
        # else:
        #     print ('This is not a regional node. The number of counter arguments is expected to be 2.')
        
        
        # # Add rdf.type
        # graph.add((URIRef(ec_root_node), RDF.type, URIRef(ontocape_upper_level_system.ExclusiveSubsystem.iri)))
        
        # # Add total consumption and value
        # index_total = elecConDataArrays[0].index('Total\n')
        # graph.add((URIRef(ec_root_node), URIRef(ontoeip_system_function.consumes.iri), URIRef(ec_namespace + ukec.TotalConsumptionKey + elecConData[0])))
        # graph.add((URIRef(ec_namespace + ukec.TotalConsumptionKey + elecConData[0]), RDF.type, URIRef(ontocape_upper_level_system.ScalarQuantity.iri)))
        
        # graph.add((URIRef(ec_namespace + ukec.TotalConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.hasValue.iri),\
        #         URIRef(ec_namespace + ukec.valueKey +ukec.TotalConsumptionKey + elecConData[0])))
        # graph.add((URIRef(ec_namespace + ukec.valueKey +ukec.TotalConsumptionKey + elecConData[0]), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
        # graph.add((URIRef(ec_namespace + ukec.valueKey +ukec.TotalConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri),\
        #             URIRef(t_box.ontocape_derived_SI_units + 'GIGAWATT_HOUR'))) # T-box undefined
        # graph.add((URIRef(ec_namespace + ukec.valueKey +ukec.TotalConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.numericalValue.iri),\
        #             Literal(float(elecConData[index_total].strip('\n')))))
        
        # # Add Domestic and IndustrialAndCommercial consumption and value
        # index_domestic = elecConDataArrays[0].index('Domestic')
        # index_industrial = elecConDataArrays[0].index('IndustrialAndCommercial')
        # graph.add((URIRef(ec_namespace + ukec.TotalConsumptionKey + elecConData[0]), URIRef(t_box.ontocape_mathematical_relation + 'ConsistsOf'),\
        #             URIRef(ec_namespace + ukec.DomesticConsumptionKey + elecConData[0]))) # T-box undefined
        # graph.add((URIRef(ec_namespace + ukec.TotalConsumptionKey + elecConData[0]), URIRef(t_box.ontocape_mathematical_relation + 'ConsistsOf'),\
        #             URIRef(ec_namespace + ukec.IndustrialAndCommercialConsumptionKey + elecConData[0])))
        # graph.add((URIRef(ec_namespace + ukec.DomesticConsumptionKey + elecConData[0]), RDF.type, URIRef(ontocape_upper_level_system.ScalarQuantity.iri)))
        # graph.add((URIRef(ec_namespace + ukec.IndustrialAndCommercialConsumptionKey + elecConData[0]), RDF.type, URIRef(ontocape_upper_level_system.ScalarQuantity.iri)))
        
        # graph.add((URIRef(ec_namespace + ukec.DomesticConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.hasValue.iri),\
        #         URIRef(ec_namespace + ukec.valueKey +ukec.DomesticConsumptionKey + elecConData[0])))
        # graph.add((URIRef(ec_namespace + ukec.valueKey +ukec.DomesticConsumptionKey + elecConData[0]), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
        # graph.add((URIRef(ec_namespace + ukec.valueKey +ukec.DomesticConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri),\
        #             URIRef(t_box.ontocape_derived_SI_units + 'GIGAWATT_HOUR'))) # T-box undefined
        # graph.add((URIRef(ec_namespace + ukec.valueKey +ukec.DomesticConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.numericalValue.iri),\
        #             Literal(float(elecConData[index_domestic].strip('\n')))))
            
        # graph.add((URIRef(ec_namespace + ukec.IndustrialAndCommercialConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.hasValue.iri),\
        #         URIRef(ec_namespace + ukec.valueKey +ukec.IndustrialAndCommercialConsumptionKey + elecConData[0])))
        # graph.add((URIRef(ec_namespace + ukec.valueKey +ukec.IndustrialAndCommercialConsumptionKey + elecConData[0]), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
        # graph.add((URIRef(ec_namespace + ukec.valueKey +ukec.IndustrialAndCommercialConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri),\
        #             URIRef(t_box.ontocape_derived_SI_units + 'GIGAWATT_HOUR'))) # T-box undefined
        # graph.add((URIRef(ec_namespace + ukec.valueKey +ukec.IndustrialAndCommercialConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.numericalValue.iri),\
        #             Literal(float(elecConData[index_industrial].strip('\n')))))
            
        # # Add hasAddress and LACODE    
        # index_LACode = elecConDataArrays[0].index('LACode')
        # graph.add((URIRef(ec_root_node), URIRef(ontocape_upper_level_system.hasAddress.iri), URIRef(t_box.dbr + elecConData[0])))
        # if elecConData[0] in engconsump.GovernmentOfficeRegions: # regional node
        #     graph.add((URIRef(t_box.dbr + elecConData[0]), RDF.type, URIRef(t_box.dbo + 'Region')))
        # elif len(counter) > 1: # local node
        #     graph.add((URIRef(t_box.dbr + elecConData[0]), RDF.type, URIRef(ontoecape_space_and_time_extended.AddressArea.iri)))
        #     # Add subdivision relationship between local areas and its region
        #     graph.add((URIRef(t_box.dbr + elecConDataArrays[counter[1]][0].strip('\n')), URIRef(t_box.dbo + 'subdivision'), URIRef(t_box.dbr + elecConData[0])))
        # graph.add((URIRef(t_box.dbr + elecConData[0]), URIRef(t_box.dbo + 'areaCode'), Literal(str(elecConData[index_LACode]))))
                    
        # return graph





# if __name__ == '__main__':
#     addRegionalandLocalNodes()
#     print('terminated')