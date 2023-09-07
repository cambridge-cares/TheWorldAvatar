##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 09 Sept 2023         #
##########################################

"""This module is designed to generate and update the A-box of UK energy consumption graph."""

import os
import owlready2
from rdflib.extras.infixowl import OWL_NS
from rdflib import Graph, URIRef, Literal
from rdflib.namespace import RDF, XSD, RDFS
from rdflib.plugins.sleepycat import Sleepycat
import sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from UK_Digital_Twin_Package import UKDigitalTwinTBox as T_BOX
from UK_Digital_Twin_Package import EnergyConsumptionDataProperty as EngConsump
from UK_Digital_Twin_Package import UKEnergyConsumption as UKec
from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs, readFile
from UK_Digital_Twin_Package.LACodeOfOfficialRegion import LACodeOfOfficialRegion as LACode
import uuid
from pyderivationagent.kg_operations.sparql_client import PySparqlClient

"""Notation used in URI construction"""
HASH = '#'
SLASH = '/'
UNDERSCORE = '_'
OWL = '.owl'
TTL = '.ttl'

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
ontoenergysystem                = owlready2.get_ontology(t_box.ontoenergysystem).load()

### Functions ### 
""" The Energy Consumption DataProperty Instance constructor"""
def createEnergyConsumptionDataPropertyInstance(version):
    engconsump = EngConsump.EnergyConsumptionData(version)   
    elecConDataArrays = readFile(engconsump.ElectricityConsumptionData)      
    return engconsump, elecConDataArrays

"""Main function: Add Triples to the regional and local nodes"""
def addUKElectricityConsumptionTriples(ElectricitySystemIRI, version, updateEndpointIRI, KGFileStoragePath, updateLocalOWLFile = True):
    ## Validate the file path
    folder = os.path.exists(KGFileStoragePath)
    if not folder:                
        os.makedirs(KGFileStoragePath)           
        print("---  New folder %s...  ---" % KGFileStoragePath)

    print('Starts adding regional and local nodes.')
    ukec = UKec.UKEnergyConsumption(version)
    engconsump, elecConDataArrays = createEnergyConsumptionDataPropertyInstance(version)  
    
    # check the data file header
    if elecConDataArrays[0] == engconsump.headerElectricityConsumption:
        header = elecConDataArrays[0]
        elecConDataArrays.remove(elecConDataArrays[0])
    else:
        raise Exception('The raw data header does not match, please check the raw data file.')
    
    # IRIs
    ontologyIRI = dt.baseURL + SLASH + dt.topNode + SLASH + str(uuid.uuid4())  # root_node + dt.GB
    
    ## Create rdf graph with identifier, regional nodes are named graphs including its local nodes
    graph = Graph(store = 'default', identifier = URIRef(ontologyIRI))
    
    # Import T-boxes
    graph.set((graph.identifier, RDF.type, OWL_NS['Ontology']))
    graph.add((graph.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_upper_level_system)))
    graph.set((graph.identifier, RDFS.comment, Literal('This ontology represents the local energy consumption of Great Britain.')))
    graph.set((graph.identifier, RDFS.label, Literal('UK Digital Twin - Energy Consumption - Electricity Consumption - ' + dt.GB)))
   
    for elecConData in elecConDataArrays:
        print('*********************************************************')         
        print('The place is:', elecConData[0].strip('\n').replace('|',','))
        if len(elecConData) != len(engconsump.headerElectricityConsumption):
            raise Exception('The data is not sufficient, please check the data file')
       
        # Define the URL of the nodes
        ec_place_name = elecConData[0].strip('\n').replace('|',',')
        ec_root_node = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukec.TotalConsumptionKey + str(uuid.uuid4())
        timeperiod_uri = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukec.TimePeriodKey + str(uuid.uuid4())      
        value_timeperiod_uri = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukec.valueKey + str(uuid.uuid4())
        starttime_uri = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukec.StartTimeKey + str(uuid.uuid4())
        value_totalconsumption_uri = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukec.valueKey + str(uuid.uuid4())
        domesticconsumption_uri = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukec.DomesticConsumptionKey + str(uuid.uuid4())
        non_domesticconsumption_uri = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukec.IndustrialAndCommercialConsumptionKey + str(uuid.uuid4())
        value_domesticconsumption_uri = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukec.valueKey + str(uuid.uuid4())
        value_non_domesticconsumption_uri = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukec.valueKey + str(uuid.uuid4())
        observed_AdministrativeDivision_uri = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukec.AdministrativeDivisionKey + str(uuid.uuid4())    
        observed_place_uri = t_box.dbr + ec_place_name # dbpedia
               
        # type of the root node                              
        graph.add((URIRef(ec_root_node), RDF.type, URIRef(ontoenergysystem.TotalElectricityConsumption.iri)))
        graph.add((URIRef(ElectricitySystemIRI), URIRef(ontoenergysystem.enablesElectricityConsumptionOf.iri), URIRef(ec_root_node)))    
        
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
        graph.add((URIRef(ec_root_node), URIRef(ontoenergysystem.includesConsumption.iri), URIRef(domesticconsumption_uri)))
        graph.add((URIRef(ec_root_node), URIRef(ontoenergysystem.includesConsumption.iri), URIRef(non_domesticconsumption_uri)))
        graph.add((URIRef(domesticconsumption_uri), RDF.type, URIRef(ontoenergysystem.DomesticElectricityConsumption.iri)))
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
        graph.add((URIRef(ec_root_node), URIRef(ontoenergysystem.isObservedIn.iri), URIRef(observed_AdministrativeDivision_uri)))
        graph.add((URIRef(observed_AdministrativeDivision_uri), URIRef(ontoenergysystem.hasLocalAuthorityCode.iri), Literal(str(elecConData[index_LACode]))))
        graph.add((URIRef(observed_AdministrativeDivision_uri), RDF.type, URIRef(ontoenergysystem.AdministrativeDivision.iri)))
        graph.add((URIRef(observed_AdministrativeDivision_uri), OWL_NS['sameAs'], URIRef(observed_place_uri)))
        
        # print(graph.serialize(format="turtle").decode("utf-8"))
            
    # generate/update OWL files
    if updateLocalOWLFile == True:
         # Store/update the generated owl files      
        if KGFileStoragePath[-2:] != "/": 
            filepath_ = KGFileStoragePath + '/' + 'UK_energy_consumption_UK_' + str(version) + TTL
        else:
            filepath_ = KGFileStoragePath + 'UK_energy_consumption_UK_' + str(version) + TTL
        storeGeneratedOWLs(graph, filepath_)   

    sparql_client = PySparqlClient(updateEndpointIRI, updateEndpointIRI)
    sparql_client.uploadOntology(filepath_)  
    return  