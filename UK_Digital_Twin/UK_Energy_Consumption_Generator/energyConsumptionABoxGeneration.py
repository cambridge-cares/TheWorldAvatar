##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 18 Nov 2021          #
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

"""Create an object of Class UKEnergyConsumption"""
ukec = UKec.UKEnergyConsumption()

"""Sleepycat storage path"""
userSpecifiePath_Sleepycat = None # user specified path
userSpecified_Sleepycat = False # storage mode: False: default, True: user specified
defaultPath_Sleepycat = ukec.SleepycatStoragePath

"""T-Box URI"""
ontocape_upper_level_system     = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
ontocape_derived_SI_units       = owlready2.get_ontology(t_box.ontocape_derived_SI_units).load()
ontoecape_space_and_time_extended = owlready2.get_ontology(t_box.ontoecape_space_and_time_extended).load()
ontoecape_space_and_time        = owlready2.get_ontology(t_box.ontoecape_space_and_time).load()
ontocape_coordinate_system      = owlready2.get_ontology(t_box.ontocape_coordinate_system).load()
ontoeip_system_function         = owlready2.get_ontology(t_box.ontoeip_system_function).load()
# bibtex         = owlready2.get_ontology(t_box.bibtex).load()
# owl         = owlready2.get_ontology(t_box.owl).load()

"""OWL file storage path"""
defaultStoredPath = ukec.StoreGeneratedOWLs # default path

"""User specified folder path"""
filepath = None
userSpecified = False

"""Energy Consumption Conjunctive graph identifier"""
ukec_cg_id = "http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_energy_consumption/energyConsumptionIn2017"

"""The UK digital twin URL and UK enelectricity system URL"""
UKDigitalTwinURL = UKDT.nodeURIGenerator(1, dt.topNode, None)
UKElectricitySystem = UKDT.nodeURIGenerator(2, dt.electricitySystem, None)

### Functions ### 
""" The Energy Consumption DataProperty Instance constructor"""
def createEnergyConsumptionDataPropertyInstance(version):
    engconsump = EngConsump.EnergyConsumptionData(version)   
    elecConDataArrays = readFile(engconsump.ElectricityConsumptionData)      
    ukElectricityConsumption = UKDT.nodeURIGenerator(3, dt.energyConsumption, engconsump.VERSION)
    root_namespace = ukElectricityConsumption.split('.owl')[0]
    fileNum = len(elecConDataArrays)  # substruct the first header line 
    
    return engconsump, elecConDataArrays, root_namespace, ukElectricityConsumption, fileNum

"""Main function: Add Triples to the regional and local nodes"""
def addUKElectricityConsumptionTriples(storeType, version, OWLFileStoragePath, updateLocalOWLFile = True):
    print('Starts adding regional and local nodes.')
    filepath = specifyValidFilePath(defaultStoredPath, OWLFileStoragePath, updateLocalOWLFile)
    if filepath == None:
        return
    store = LocalGraphStore(storeType)
    global userSpecifiePath_Sleepycat, userSpecified_Sleepycat, defaultPath_Sleepycat
    if isinstance(store, Sleepycat):    
        # Create Conjunctive graph maintain all power plant graphs
        eleConConjunctiveGraph = ConjunctiveGraph(store=store, identifier = ukec_cg_id)
        if userSpecifiePath_Sleepycat == None and userSpecified_Sleepycat:
            print('****Needs user to specify a Sleepycat storage path****')
            userSpecifiePath_Sleepycat = selectStoragePath()
            userSpecifiePath_Sleepycat_ = userSpecifiePath_Sleepycat + '\\' + 'ConjunctiveGraph_UKElectricityConsumption'
            sl = eleConConjunctiveGraph.open(userSpecifiePath_Sleepycat_, create = False) 
            
        elif os.path.exists(defaultPath_Sleepycat) and not userSpecified_Sleepycat:
            print('****Non user specified Sleepycat storage path, will use the default storage path****')
            sl = eleConConjunctiveGraph.open(defaultPath_Sleepycat, create = False)        
        else:
            sl = eleConConjunctiveGraph.open(defaultPath_Sleepycat, create = True)   
        
        if sl == NO_STORE:
        # There is no underlying Sleepycat infrastructure, so create it
            eleConConjunctiveGraph.open(defaultPath_Sleepycat, create=True)
        else:
            assert sl == VALID_STORE, "The underlying sleepycat store is corrupt"
    
    engconsump, elecConDataArrays, root_namespace, ukElectricityConsumption, fileNum = createEnergyConsumptionDataPropertyInstance(version)  
    
    # check the data file header
    if elecConDataArrays[0] == engconsump.headerElectricityConsumption:
        header = elecConDataArrays[0]
        elecConDataArrays.remove(elecConDataArrays[0])
    else:
        raise Exception('The raw data header does not match, please check the raw data file.')
 
    # Create rdf graph with identifier, regional nodes are named graphs including its local nodes
    graph = Graph(store = store, identifier = URIRef(ukElectricityConsumption)) # graph(store='default', identifier)
    # Import T-boxes
    graph.set((graph.identifier, RDF.type, OWL_NS['Ontology']))
    graph.add((graph.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_upper_level_system)))
    # Add connection between its father node                               
    graph.add((URIRef(UKElectricitySystem), URIRef(ontocape_upper_level_system.isExclusivelySubsystemOf.iri), URIRef(UKDigitalTwinURL)))
    graph.add((URIRef(UKElectricitySystem), RDF.type, URIRef(t_box.ontoenergysystem + 'ElectricPowerSystem'))) 
    graph.add((URIRef(UKDigitalTwinURL), RDF.type, URIRef(ontocape_upper_level_system.TopLevelSystem.iri)))  
    # Add label denoting the version of the data being published
    graph.add((URIRef(ukElectricityConsumption), RDFS.label, Literal("UK_Energy_Consumption_" + str(engconsump.VERSION))))
   
    for elecConData in elecConDataArrays:
        print('*********************************************************')         
        print('The place is:', elecConData[0].strip('\n').replace('|',','))
        if len(elecConData) != len(engconsump.headerElectricityConsumption):
            raise Exception('The data is not sufficient, please check the data file')
       
        # Define the URL of the nodes
        ec_place_name = elecConData[0].strip('\n').replace('|',',')
        ec_namespace = root_namespace + SLASH + ec_place_name+ OWL + HASH 
        ec_root_node = ec_namespace + ukec.TotalConsumptionKey + ec_place_name # top node of the named graph
        timeperiod_uri = ec_namespace + ukec.TimePeriodKey + ukec.TotalConsumptionKey + ec_place_name
        value_timeperiod_uri = ec_namespace + ukec.valueKey + ukec.TimePeriodKey + ukec.TotalConsumptionKey + ec_place_name
        starttime_uri = ec_namespace + ukec.StartTimeKey + ukec.TotalConsumptionKey + ec_place_name
        value_totalconsumption_uri = ec_namespace + ukec.valueKey +ukec.TotalConsumptionKey + ec_place_name
        domesticconsumption_uri = ec_namespace + ukec.DomesticConsumptionKey + ec_place_name
        non_domesticconsumption_uri = ec_namespace + ukec.IndustrialAndCommercialConsumptionKey + ec_place_name
        value_domesticconsumption_uri = ec_namespace + ukec.valueKey +ukec.DomesticConsumptionKey + ec_place_name
        value_non_domesticconsumption_uri = ec_namespace + ukec.valueKey + ukec.IndustrialAndCommercialConsumptionKey + ec_place_name
        observed_place_uri = t_box.dbr + ec_place_name
               
        # Add connection between its father node                               
        graph.add((URIRef(ec_root_node), RDF.type, URIRef(t_box.ontoenergysystem + 'TotalElectricityConsumption')))
        graph.add((URIRef(UKElectricitySystem), URIRef(t_box.ontoenergysystem + 'enablesElectricityConsumptionOf'), URIRef(ec_root_node)))    
        
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
    # res = test_returnLACode(2019)
    print('terminated')