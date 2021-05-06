##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 26 April 2021        #
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
from UK_Digital_Twin_Package import EnergyConsumptionDataProperty as EngConsump
from UK_Digital_Twin_Package import UKEnergyConsumption as UKec
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

"""Create an object of Class UKEnergyConsumption"""
ukec = UKec.UKEnergyConsumption()

"""Create an object of Class EnergyConsumptionDataProperty"""
engconsump = EngConsump.EnergyConsumptionData()

"""Root_uri"""
uriSplit = UKDT.namedGraphURIGenerator(3, dt.energyConsumption, engconsump.VERSION).split('.owl') 
root_uri = uriSplit[0]

"""T-Box URI"""
ontocape_upper_level_system     = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
ontocape_derived_SI_units       = owlready2.get_ontology(t_box.ontocape_derived_SI_units).load()
ontoecape_space_and_time_extended = owlready2.get_ontology(t_box.ontoecape_space_and_time_extended).load()
ontoeip_system_function         = owlready2.get_ontology(t_box.ontoeip_system_function).load()

"""Data Array"""
elecConDataArrays = readFile(engconsump.ElectricityConsumptionData)

"""Number of owl files to be generated"""
fileNum = len(elecConDataArrays) # substruct the first header line 

"""User specified folder path"""
filepath = None
userSpecified = False

### Functions ### 
"""Add Triples to the target nodes"""
def addUKElectricityConsumptionTriples(graph, *counter):  
    if len(elecConDataArrays[counter[0]]) != len(engconsump.headerElectricityConsumption):
        print('The data is not sufficient, please check the data file')
        return None
    else:
        elecConData = elecConDataArrays[counter[0]]
        # Check node type: reginal or local nodes
        if elecConData[0] in engconsump.GovernmentOfficeRegions: # regional node
            print('Regional node name is: ' + elecConData[0].strip('\n'))
            ec_root_node = root_uri + SLASH + elecConData[0].strip('\n') + OWL + HASH + elecConData[0].strip('\n') # top node of the named graph
            ec_namespace = root_uri + SLASH + elecConData[0].strip('\n') + OWL + HASH       
            # Import T-boxes
            graph.add((graph.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_upper_level_system)))
            # Add connection between its father node
            graph.add((URIRef(ec_root_node), URIRef(ontocape_upper_level_system.isExclusivelySubsystemOf.iri),\
                        URIRef(UKDT.namedGraphURIGenerator(3, dt.energyConsumption, engconsump.VERSION))))
        elif len(counter) > 1: # local node
            print('Local node name is: ' + elecConData[0].strip('\n'))
            ec_root_node = root_uri + SLASH + elecConDataArrays[counter[1]][0].strip('\n') + OWL + HASH + elecConData[0].strip('\n') # sub node of the graph identifying the local node
            ec_namespace = root_uri + SLASH + elecConDataArrays[counter[1]][0].strip('\n') + OWL + HASH       
            # Add connection between its father node
            graph.add((URIRef(root_uri + SLASH + elecConDataArrays[counter[1]][0].strip('\n') + OWL + HASH + elecConDataArrays[counter[1]][0].strip('\n')),\
                       URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(ec_root_node)))
        else:
            print ('This is not a regional node. The number of counter arguments is expected to be 2.')
        
        
        # Add rdf.type
        graph.add((URIRef(ec_root_node), RDF.type, URIRef(ontocape_upper_level_system.ExclusiveSubsystem.iri)))
        
        # Add total consumption and value
        index_total = elecConDataArrays[0].index('Total\n')
        graph.add((URIRef(ec_root_node), URIRef(ontoeip_system_function.consumes.iri), URIRef(ec_namespace + ukec.TotalConsumptionKey + elecConData[0])))
        graph.add((URIRef(ec_namespace + ukec.TotalConsumptionKey + elecConData[0]), RDF.type, URIRef(ontocape_upper_level_system.ScalarQuantity.iri)))
        
        graph.add((URIRef(ec_namespace + ukec.TotalConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.hasValue.iri),\
                URIRef(ec_namespace + ukec.valueKey +ukec.TotalConsumptionKey + elecConData[0])))
        graph.add((URIRef(ec_namespace + ukec.valueKey +ukec.TotalConsumptionKey + elecConData[0]), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
        graph.add((URIRef(ec_namespace + ukec.valueKey +ukec.TotalConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri),\
                    URIRef(t_box.ontocape_derived_SI_units + 'GIGAWATT_HOUR'))) # T-box undefined
        graph.add((URIRef(ec_namespace + ukec.valueKey +ukec.TotalConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.numericalValue.iri),\
                    Literal(float(elecConData[index_total].strip('\n')))))
        
        # Add Domestic and IndustrialAndCommercial consumption and value
        index_domestic = elecConDataArrays[0].index('Domestic')
        index_industrial = elecConDataArrays[0].index('IndustrialAndCommercial')
        graph.add((URIRef(ec_namespace + ukec.TotalConsumptionKey + elecConData[0]), URIRef(t_box.ontocape_mathematical_relation + 'ConsistsOf'),\
                    URIRef(ec_namespace + ukec.DomesticConsumptionKey + elecConData[0]))) # T-box undefined
        graph.add((URIRef(ec_namespace + ukec.TotalConsumptionKey + elecConData[0]), URIRef(t_box.ontocape_mathematical_relation + 'ConsistsOf'),\
                    URIRef(ec_namespace + ukec.IndustrialAndCommercialConsumptionKey + elecConData[0])))
        graph.add((URIRef(ec_namespace + ukec.DomesticConsumptionKey + elecConData[0]), RDF.type, URIRef(ontocape_upper_level_system.ScalarQuantity.iri)))
        graph.add((URIRef(ec_namespace + ukec.IndustrialAndCommercialConsumptionKey + elecConData[0]), RDF.type, URIRef(ontocape_upper_level_system.ScalarQuantity.iri)))
        
        graph.add((URIRef(ec_namespace + ukec.DomesticConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.hasValue.iri),\
                URIRef(ec_namespace + ukec.valueKey +ukec.DomesticConsumptionKey + elecConData[0])))
        graph.add((URIRef(ec_namespace + ukec.valueKey +ukec.DomesticConsumptionKey + elecConData[0]), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
        graph.add((URIRef(ec_namespace + ukec.valueKey +ukec.DomesticConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri),\
                    URIRef(t_box.ontocape_derived_SI_units + 'GIGAWATT_HOUR'))) # T-box undefined
        graph.add((URIRef(ec_namespace + ukec.valueKey +ukec.DomesticConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.numericalValue.iri),\
                    Literal(float(elecConData[index_domestic].strip('\n')))))
            
        graph.add((URIRef(ec_namespace + ukec.IndustrialAndCommercialConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.hasValue.iri),\
                URIRef(ec_namespace + ukec.valueKey +ukec.IndustrialAndCommercialConsumptionKey + elecConData[0])))
        graph.add((URIRef(ec_namespace + ukec.valueKey +ukec.IndustrialAndCommercialConsumptionKey + elecConData[0]), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
        graph.add((URIRef(ec_namespace + ukec.valueKey +ukec.IndustrialAndCommercialConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri),\
                    URIRef(t_box.ontocape_derived_SI_units + 'GIGAWATT_HOUR'))) # T-box undefined
        graph.add((URIRef(ec_namespace + ukec.valueKey +ukec.IndustrialAndCommercialConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.numericalValue.iri),\
                    Literal(float(elecConData[index_industrial].strip('\n')))))
            
        # Add hasAddress and LACODE    
        index_LACode = elecConDataArrays[0].index('LACode')
        graph.add((URIRef(ec_root_node), URIRef(ontocape_upper_level_system.hasAddress.iri), URIRef(t_box.dbr + elecConData[0])))
        if elecConData[0] in engconsump.GovernmentOfficeRegions: # regional node
            graph.add((URIRef(t_box.dbr + elecConData[0]), RDF.type, URIRef(t_box.dbo + 'Region')))
        elif len(counter) > 1: # local node
            graph.add((URIRef(t_box.dbr + elecConData[0]), RDF.type, URIRef(ontoecape_space_and_time_extended.AddressArea.iri)))
            # Add subdivision relationship between local areas and its region
            graph.add((URIRef(t_box.dbr + elecConDataArrays[counter[1]][0].strip('\n')), URIRef(t_box.dbo + 'subdivision'), URIRef(t_box.dbr + elecConData[0])))
        graph.add((URIRef(t_box.dbr + elecConData[0]), URIRef(t_box.dbo + 'areaCode'), Literal(str(elecConData[index_LACode]))))
                    
        return graph



"""Add Triples to the regional and local nodes"""
def addRegionalandLocalNodes():
    print('Starts adding regional node.')
    # check the data file header
    if elecConDataArrays[0] == engconsump.headerElectricityConsumption:
        pass
    global fileNum, filepath, userSpecified
    counter = 1
    print('The counter is:') # starts from 1
    print(counter)
    
    # Add the official region node
    numOfRegion = len(engconsump.GovernmentOfficeRegions)
    counter_region = 0   
    while (counter_region < numOfRegion): # numOfRegion
        region = engconsump.GovernmentOfficeRegions[counter_region]
        print('The region is: ')
        print(region)
        # find the index of region in the data file
        _elecConDataArrays = np.array(elecConDataArrays)
        index_targetRegion = np.argwhere(_elecConDataArrays == region)[0][0]        
        regional_base_uri = root_uri + SLASH + region + OWL # the name of the named graph, will be applied as the identifier in method Graph(), without '#' 
        
        # Create rdf graph with identifier, regional nodes are named graphs including its local nodes
        graph = Graph(store = store, identifier = URIRef(regional_base_uri)) # graph(store='default', identifier)
        graph = addUKElectricityConsumptionTriples(graph, index_targetRegion)       
        print('Counter and index_targetRegion are:')
        print (counter, index_targetRegion)
        
        # Add the local nodes under its regional node
        while (counter < index_targetRegion):
            graph = addUKElectricityConsumptionTriples(graph, counter, index_targetRegion)
            counter += 1
            
        # specify the owl file storage path
        defaultStoredPath = ukec.StoreGeneratedOWLs + 'UK_energy_consumption_' + region + '_UK' + OWL #default path
    
        # Store/update the generated owl files      
        if os.path.exists(ukec.StoreGeneratedOWLs) and not userSpecified:
            print('****Non user specified strorage path, will use the default storage path****')
            storeGeneratedOWLs(graph, defaultStoredPath)
    
        elif filepath == None:
            print('****Needs user to specify a strorage path****')
            filepath = selectStoragePath()
            filepath_ = filepath + '\\' + 'UK_energy_consumption_' + region + '_UK' + OWL
            storeGeneratedOWLs(graph, filepath_)
        else: 
            filepath_ = filepath + '\\' +'UK_energy_consumption_' + region + '_UK' + OWL
            storeGeneratedOWLs(graph, filepath_)
        
        print('counter_region is: ')
        print(counter_region)
        
        counter_region += 1               
    return 

if __name__ == '__main__':
    addRegionalandLocalNodes()
    print('terminated')