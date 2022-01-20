##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 19 Jan 2022          #
##########################################

"""This module is designed to generate and update the A-box of UK energy consumption graph."""

import os
import owlready2
import numpy as np
from rdflib.extras.infixowl import OWL_NS
from rdflib import Graph, URIRef, Literal, ConjunctiveGraph
from rdflib.namespace import RDF, XSD, RDFS
from rdflib.plugins.sleepycat import Sleepycat
from rdflib.store import NO_STORE, VALID_STORE
import sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from UK_Digital_Twin_Package import UKDigitalTwinTBox as T_BOX
from UK_Digital_Twin_Package import EnergyConsumptionDataProperty as EngConsump
from UK_Digital_Twin_Package import UKEnergyConsumption as UKec
from UK_Digital_Twin_Package.GraphStore import LocalGraphStore
from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs, selectStoragePath, readFile, specifyValidFilePath
from UK_Digital_Twin_Package.LACodeOfOfficialRegion import LACodeOfOfficialRegion as LACode

"""Notation used in URI construction"""
HASH = '#'
SLASH = '/'
UNDERSCORE = '_'
OWL = '.owl'

"""Create an instance of Class UKDigitalTwin"""
dt = UKDT.UKDigitalTwin()

"""Create an object of Class UKDigitalTwinTBox"""
t_box = T_BOX.UKDigitalTwinTBox()

"""T-Box URI"""
ontocape_upper_level_system     = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
ontocape_derived_SI_units       = owlready2.get_ontology(t_box.ontocape_derived_SI_units).load()
ontoecape_space_and_time_extended = owlready2.get_ontology(t_box.ontoecape_space_and_time_extended).load()
ontoecape_space_and_time        = owlready2.get_ontology(t_box.ontoecape_space_and_time).load()
ontocape_coordinate_system      = owlready2.get_ontology(t_box.ontocape_coordinate_system).load()
ontoeip_system_function         = owlready2.get_ontology(t_box.ontoeip_system_function).load()
# bibtex         = owlready2.get_ontology(t_box.bibtex).load()
# owl         = owlready2.get_ontology(t_box.owl).load()

"""User specified folder path"""
filepath = None
userSpecified = False

"""Energy Consumption Conjunctive graph identifier"""
ukec_cg_id = "http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_energy_consumption/energyConsumptionIn2017"

"""The UK digital twin URL and UK enelectricity system URL"""
# UKDigitalTwinURL = UKDT.nodeURIGenerator(1, dt.topNode, None)
UKElectricitySystem = UKDT.nodeURIGenerator(2, dt.electricitySystem, None)

### Functions ### 
""" The Energy Consumption DataProperty Instance constructor"""
def createEnergyConsumptionDataPropertyInstance(version):
    engconsump = EngConsump.EnergyConsumptionData(version)   
    elecConDataArrays = readFile(engconsump.ElectricityConsumptionData)      
    ukElectricityConsumption = UKDT.nodeURIGenerator(3, dt.energyConsumption, engconsump.VERSION)
    root_node = ukElectricityConsumption
    fileNum = len(elecConDataArrays)  # substruct the first header line 
    
    return engconsump, elecConDataArrays, root_node, ukElectricityConsumption, fileNum

"""Main function: Add Triples to the regional and local nodes"""
def addUKElectricityConsumptionTriples(storeType, version, OWLFileStoragePath, updateLocalOWLFile = True):
    print('Starts adding regional and local nodes.')
    ukec = UKec.UKEnergyConsumption(version)
    defaultStoredPath = ukec.StoreGeneratedOWLs
    defaultPath_Sleepycat = ukec.SleepycatStoragePath 
    filepath = specifyValidFilePath(defaultStoredPath, OWLFileStoragePath, updateLocalOWLFile)
    if filepath == None:
        return
    store = LocalGraphStore(storeType)
    
    if isinstance(store, Sleepycat):    
        # Create Conjunctive graph maintain all power plant graphs
        eleConConjunctiveGraph = ConjunctiveGraph(store=store, identifier = ukec_cg_id)
        sl = eleConConjunctiveGraph.open(defaultPath_Sleepycat, create = False)
        if sl == NO_STORE:
            print('Cannot find the specified sleepycat store')
    else:
        eleConConjunctiveGraph = None

    engconsump, elecConDataArrays, root_node, ukElectricityConsumption, fileNum = createEnergyConsumptionDataPropertyInstance(version)  
    
    # check the data file header
    if elecConDataArrays[0] == engconsump.headerElectricityConsumption:
        header = elecConDataArrays[0]
        elecConDataArrays.remove(elecConDataArrays[0])
    else:
        raise Exception('The raw data header does not match, please check the raw data file.')
    
    # IRIs
    ontologyIRI = root_node + dt.GB
    GBElectricitySystemIRI = UKElectricitySystem + dt.GB
    # Create rdf graph with identifier, regional nodes are named graphs including its local nodes
    graph = Graph(store = store, identifier = URIRef(ontologyIRI)) # graph(store='default', identifier)
    
    # Import T-boxes
    graph.set((graph.identifier, RDF.type, OWL_NS['Ontology']))
    graph.add((graph.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_upper_level_system)))
    graph.set((graph.identifier, RDFS.comment, Literal('This ontology represents the local energy consumption of Great Britain.')))
    graph.set((graph.identifier, RDFS.label, Literal('UK Digital Twin - Energy Consumption - Electricity Consumption - ' + dt.GB)))
   
    # Add connection between its father node                               
    graph.add((URIRef(GBElectricitySystemIRI), RDF.type, URIRef(t_box.ontoenergysystem + 'ElectricPowerSystem'))) 
    
    for elecConData in elecConDataArrays:
        print('*********************************************************')         
        print('The place is:', elecConData[0].strip('\n').replace('|',','))
        if len(elecConData) != len(engconsump.headerElectricityConsumption):
            raise Exception('The data is not sufficient, please check the data file')
       
        # Define the URL of the nodes
        ec_place_name = elecConData[0].strip('\n').replace('|',',')
        ec_root_node = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukec.TotalConsumptionKey + ec_place_name # top node of the named graph
        timeperiod_uri = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukec.TimePeriodKey + ec_place_name        
        value_timeperiod_uri = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukec.valueKey + ukec.TimePeriodKey + ec_place_name 
        starttime_uri = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukec.StartTimeKey + ec_place_name 
        value_totalconsumption_uri = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukec.valueKey +ukec.TotalConsumptionKey + ec_place_name
        domesticconsumption_uri = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukec.DomesticConsumptionKey + ec_place_name
        non_domesticconsumption_uri = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukec.IndustrialAndCommercialConsumptionKey + ec_place_name
        value_domesticconsumption_uri = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukec.valueKey +ukec.DomesticConsumptionKey + ec_place_name
        value_non_domesticconsumption_uri = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukec.valueKey + ukec.IndustrialAndCommercialConsumptionKey + ec_place_name
        observed_place_uri = t_box.dbr + ec_place_name
               
        # type of the root node                              
        graph.add((URIRef(ec_root_node), RDF.type, URIRef(t_box.ontoenergysystem + 'TotalElectricityConsumption')))
        graph.add((URIRef(GBElectricitySystemIRI), URIRef(t_box.ontoenergysystem + 'enablesElectricityConsumptionOf'), URIRef(ec_root_node)))    
        
        # Specify the time period of the current data and its start time 
        graph.add((URIRef(ec_root_node), URIRef(t_box.ontocape_derived_SI_units + 'hasTimePeriod'), URIRef(timeperiod_uri))) # T-box undefined
        graph.add((URIRef(timeperiod_uri), RDF.type, URIRef(ontoecape_space_and_time.TimePeriod.iri))) 
        graph.add((URIRef(timeperiod_uri), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(value_timeperiod_uri)))
        graph.add((URIRef(value_timeperiod_uri), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
        graph.add((URIRef(value_timeperiod_uri), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(ontocape_derived_SI_units.YEAR.iri)))
        graph.add((URIRef(value_timeperiod_uri), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(1)))  
        graph.add((URIRef(value_timeperiod_uri), URIRef(ontoecape_space_and_time.hasStartingTime.iri), URIRef(starttime_uri)))   
        graph.add((URIRef(starttime_uri), RDF.type, URIRef(ontocape_coordinate_system.CoordinateValue.iri)))
        graph.add((URIRef(starttime_uri), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(t_box.ontocape_derived_SI_units+ 'UTC')))
        graph.add((URIRef(starttime_uri), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(engconsump.startTime_NHH, datatype = XSD.dateTime)))  
        
        # Add total consumption value
        index_total = header.index('Total\n')
        graph.add((URIRef(ec_root_node), RDF.type, URIRef(ontocape_upper_level_system.ScalarQuantity.iri)))
        graph.add((URIRef(ec_root_node), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(value_totalconsumption_uri)))
        graph.add((URIRef(value_totalconsumption_uri), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
        graph.add((URIRef(value_totalconsumption_uri), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(t_box.ontocape_derived_SI_units + 'GIGAWATT_HOUR'))) # T-box undefined
        graph.add((URIRef(value_totalconsumption_uri), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(elecConData[index_total].strip('\n')))))
    
        # Add Domestic and IndustrialAndCommercial(Non-domestic) consumption and value
        index_domestic = header.index('Domestic')
        index_industrial = header.index('IndustrialAndCommercial')
        graph.add((URIRef(ec_root_node), URIRef(t_box.ontoenergysystem + 'includesConsumption'), URIRef(domesticconsumption_uri)))
        graph.add((URIRef(ec_root_node), URIRef(t_box.ontoenergysystem + 'includesConsumption'), URIRef(non_domesticconsumption_uri)))
        graph.add((URIRef(domesticconsumption_uri), RDF.type, URIRef(t_box.ontoenergysystem + 'DomesticElectricityConsumption')))
        graph.add((URIRef(domesticconsumption_uri), RDF.type, URIRef(ontocape_upper_level_system.ScalarQuantity.iri)))
        graph.add((URIRef(non_domesticconsumption_uri), RDF.type, URIRef(t_box.ontoenergysystem + 'Non-DomesticElectricityConsumption')))
        graph.add((URIRef(non_domesticconsumption_uri), RDF.type, URIRef(ontocape_upper_level_system.ScalarQuantity.iri)))
            
        graph.add((URIRef(domesticconsumption_uri), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(value_domesticconsumption_uri)))
        graph.add((URIRef(value_domesticconsumption_uri), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
        graph.add((URIRef(value_domesticconsumption_uri), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(t_box.ontocape_derived_SI_units + 'GIGAWATT_HOUR'))) # T-box undefined
        graph.add((URIRef(value_domesticconsumption_uri), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(elecConData[index_domestic].strip('\n')))))
            
        graph.add((URIRef(non_domesticconsumption_uri), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(value_non_domesticconsumption_uri)))
        graph.add((URIRef(value_non_domesticconsumption_uri), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
        graph.add((URIRef(value_non_domesticconsumption_uri), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(t_box.ontocape_derived_SI_units + 'GIGAWATT_HOUR'))) # T-box undefined
        graph.add((URIRef(value_non_domesticconsumption_uri), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(elecConData[index_industrial].strip('\n')))))
              
        # Add hasAddress and LACODE    
        index_LACode = header.index('LACode')            
        graph.add((URIRef(ec_root_node), URIRef(t_box.ontoenergysystem + 'isObservedIn'), URIRef(observed_place_uri)))
        graph.add((URIRef(observed_place_uri), URIRef(t_box.ontoenergysystem + 'hasLocalAuthorityCode'), Literal(str(elecConData[index_LACode]))))
        graph.add((URIRef(observed_place_uri), RDF.type, URIRef(t_box.ontoenergysystem + 'AdministrativeDivision')))
                    
    # generate/update OWL files
    if updateLocalOWLFile == True:
         # Store/update the generated owl files      
        if filepath[-2:] != "\\": 
            filepath_ = filepath + '\\' + 'UK_energy_consumption_UK_' + str(version) + OWL
        else:
            filepath_ = filepath + 'UK_energy_consumption_UK_' + str(version) + OWL
        storeGeneratedOWLs(graph, filepath_)
    
    if isinstance(store, Sleepycat):  
        eleConConjunctiveGraph.close()               
    return  

if __name__ == '__main__':
    path = "C:\\Users\\wx243\\Desktop\\test\\new_elec_consump\\"
    addUKElectricityConsumptionTriples('default', 2017, None, True)
    print('terminated')