##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 20 April 2021        #
##########################################

"""This module is designed to generate and update the A-box of UK power plant graph."""

import os
import owlready2
from rdflib.extras.infixowl import OWL_NS
from rdflib import Graph, URIRef, BNode, Literal, Namespace
from rdflib.namespace import RDF, RDFS, XSD
import sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from UK_Digital_Twin_Package import UKDigitalTwinTBox as T_BOX
from UK_Digital_Twin_Package import DUKESDataProperty as DUKES
from UK_Digital_Twin_Package import UKPowerPlant as UKpp
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

"""Create an object of Class DUKESDataProperty"""
dukes = DUKES.DUKESData()

"""Create an object of Class UKPowerPlantDataProperty"""
ukpp = UKpp.UKPowerPlant()

"""Root_uri"""
uriSplit = UKDT.namedGraphURIGenerator(3, dt.powerPlant, dukes.VERSION).split('.owl') 
root_uri = uriSplit[0]

"""T-Box URI"""
ontocape_upper_level_system     = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
ontoecape_technical_system      = owlready2.get_ontology(t_box.ontoecape_technical_system).load()
ontoeip_powerplant              = owlready2.get_ontology(t_box.ontoeip_powerplant).load()
ontopowsys_PowSysRealization    = owlready2.get_ontology(t_box.ontopowsys_PowSysRealization).load()
ontocape_derived_SI_units       = owlready2.get_ontology(t_box.ontocape_derived_SI_units).load()
ontoeip_upper_level_system_v1   = owlready2.get_ontology(t_box.ontoeip_upper_level_system_v1).load()
ontoecape_space_and_time_extended = owlready2.get_ontology(t_box.ontoecape_space_and_time_extended).load()
ontoecape_space_and_time        = owlready2.get_ontology(t_box.ontoecape_space_and_time).load()
ontocape_physical_dimension     = owlready2.get_ontology(t_box.ontocape_physical_dimension).load()
ontocape_coordinate_system      = owlready2.get_ontology(t_box.ontocape_coordinate_system).load()
ontocape_SI_units       = owlready2.get_ontology(t_box.ontocape_SI_units).load()

"""Data Array"""
plantnameArrays      = readFile(dukes.PlantName)
planttypeArrays      = readFile(dukes.PlantType)
energygenArrays      = readFile(dukes.EnergyGen)
gentechArrays        = readFile(dukes.GenTech)
primaryfuelArrays    = readFile(dukes.PrimaryFuel)
designcapacityArrays = readFile(dukes.DesignCapacity)
builtYearArrays      = readFile(dukes.BuiltYear)
ownerArrays          = readFile(dukes.Owner)
gpslocationArrays    = readFile(dukes.GPSLocation)
regionArrays         = readFile(dukes.Region)

# """Counter"""
# counter = 0

"""Number of owl files to be generated"""
fileNum = len(plantnameArrays)

"""User specified folder path"""
filepath = None # user specified path
userSpecified = False # storage mode: False: default, True: user specified

### Functions ### 
"""Add Triples to the named graph"""
def addUKPowerPlantTriples():  
    counter = 0
    while(counter < fileNum):
        print('The counter is:')
        print(counter)
        plantname = ''.join(plantnameArrays[counter]).strip('\n')
        planttype = ''.join(planttypeArrays[counter]).strip('\n')
        energygen = ''.join(energygenArrays[counter]).strip('\n')
        gentech = ''.join(gentechArrays[counter]).strip('\n')
        primaryfuel = ''.join(primaryfuelArrays[counter]).strip('\n')
        designcapacity = ''.join(designcapacityArrays[counter]).strip('\n')
        builtyear = ''.join(builtYearArrays[counter]).strip('\n')
        owner = ''.join(ownerArrays[counter]).strip('\n')
        gpslocation = gpslocationArrays[counter]
        region = ''.join(regionArrays[counter]).strip('\n')
        
        if  len(planttypeArrays) != fileNum or len(energygenArrays) != fileNum or len(gentechArrays) != fileNum or len(primaryfuelArrays) != fileNum or\
            len(designcapacityArrays) != fileNum or len(builtYearArrays) != fileNum or len(ownerArrays) != fileNum or len(gpslocationArrays) != fileNum:
            print('The list length of each data files does not match')
            return
        else:
            pp_base_uri  = root_uri + SLASH + plantname + OWL # the name of the named graph, will be applied as the identifier in method Graph(), without '#'
            pp_root_node = root_uri + SLASH + plantname + OWL + HASH + plantname # the top node of the named graph, '#XXX'
            pp_namespace = root_uri + SLASH + plantname + OWL + HASH
            
            # Create rdf graph with identifier
            graph = Graph(store = store, identifier = URIRef(pp_base_uri)) # graph(store='default', identifier)
            
            # Import T-boxes
            graph.add((graph.identifier, OWL_NS['imports'], URIRef(t_box.ontoecape_technical_system)))
            graph.add((graph.identifier, OWL_NS['imports'], URIRef(t_box.ontoeip_powerplant)))
            
            # Add rdf.type
            graph.add((URIRef(pp_root_node), RDF.type, URIRef(ontoeip_powerplant.PowerPlant.iri)))
            graph.add((URIRef(pp_root_node), RDF.type, URIRef(ontoecape_technical_system.TechnicalSystem.iri)))
            graph.add((URIRef(pp_root_node), RDF.type, URIRef(t_box.ontopowsys_PowSysRealization + planttype)))
            
            # Add connection between its father node
            graph.add((URIRef(pp_root_node), URIRef(ontocape_upper_level_system.isExclusivelySubsystemOf.iri),\
                        URIRef(UKDT.namedGraphURIGenerator(3, dt.powerPlant, dukes.VERSION))))
            
            # Add Realization Aspect  
            graph.add((URIRef(pp_root_node), URIRef(ontoecape_technical_system.hasRealizationAspect.iri), URIRef(pp_namespace + ukpp.RealizationAspectKey + plantname)))
            graph.add((URIRef(pp_namespace + ukpp.RealizationAspectKey + plantname), RDF.type, URIRef(ontoeip_powerplant.PowerGenerator.iri)))
            graph.add((URIRef(pp_namespace + ukpp.RealizationAspectKey + plantname), URIRef(ontoecape_technical_system.realizes.iri),\
                        URIRef(pp_namespace + energygen + UNDERSCORE + plantname)))
            graph.add((URIRef(pp_namespace + energygen + UNDERSCORE + plantname), RDF.type, URIRef(ontoeip_powerplant.usesGenerationTechnology.iri)))
            graph.add((URIRef(pp_namespace + energygen + UNDERSCORE + plantname), URIRef(ontoeip_powerplant.PowerGenerator.iri),\
                        URIRef(t_box.ontoeip_powerplant + gentech)))
            graph.add((URIRef(t_box.ontoeip_powerplant + gentech), RDF.type, URIRef(ontoeip_powerplant.PlantGenerationTechnology.iri)))
            graph.add((URIRef(pp_namespace + energygen + UNDERSCORE + plantname), URIRef(ontoeip_powerplant.consumesPrimaryFuel.iri),\
                        URIRef(t_box.ontoeip_powerplant + primaryfuel)))
            graph.add((URIRef(t_box.ontoeip_powerplant + primaryfuel), RDF.type, URIRef(ontoeip_powerplant.PrimaryFuel.iri)))
            
            # Add Functional Aspect  
            graph.add((URIRef(pp_root_node), URIRef(ontoecape_technical_system.hasFunctionalAspect.iri), URIRef(pp_namespace + energygen + UNDERSCORE + plantname)))
            
            # Add Requirements Aspect
            graph.add((URIRef(pp_root_node), URIRef(ontoecape_technical_system.hasRequirementsAspect.iri), URIRef(pp_namespace + ukpp.RequirementsAspectKey + plantname)))
            graph.add((URIRef(pp_namespace + ukpp.RequirementsAspectKey + plantname), RDF.type, URIRef(t_box.ontoeip_system_requirement + 'DesignCapacity'))) # T-box undefined
            graph.add((URIRef(pp_namespace + ukpp.RequirementsAspectKey + plantname), URIRef(ontoecape_technical_system.isAchievedThrough.iri),\
                        URIRef(pp_namespace + energygen + UNDERSCORE + plantname)))
            # # add values to attributes
            graph.add((URIRef(pp_namespace + ukpp.RequirementsAspectKey + plantname), URIRef(ontocape_upper_level_system.hasValue.iri),\
                    URIRef(pp_namespace + ukpp.valueKey + ukpp.RequirementsAspectKey + plantname)))
            graph.add((URIRef(pp_namespace + ukpp.valueKey + ukpp.RequirementsAspectKey + plantname), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
            graph.add((URIRef(pp_namespace + ukpp.valueKey + ukpp.RequirementsAspectKey + plantname), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri),\
                        URIRef(ontocape_derived_SI_units.MW.iri)))
            graph.add((URIRef(pp_namespace + ukpp.valueKey + ukpp.RequirementsAspectKey + plantname), URIRef(ontocape_upper_level_system.numericalValue.iri),\
                        Literal(float(designcapacity))))
            # Add other attributes
            graph.add((URIRef(pp_root_node), URIRef(ontoeip_powerplant.hasYearOfBuilt.iri), URIRef(pp_namespace + ukpp.BuiltYearKey + plantname)))
            graph.add((URIRef(pp_namespace + ukpp.BuiltYearKey + plantname), RDF.type, URIRef(ontoeip_powerplant.YearOfBuilt.iri)))
            graph.add((URIRef(pp_namespace + ukpp.BuiltYearKey + plantname), URIRef(ontocape_upper_level_system.hasValue.iri),\
                         URIRef(pp_namespace + ukpp.valueKey + ukpp.BuiltYearKey + plantname)))
            graph.add((URIRef(pp_namespace + ukpp.valueKey + ukpp.BuiltYearKey + plantname), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
            graph.add((URIRef(pp_namespace + ukpp.valueKey + ukpp.BuiltYearKey + plantname), URIRef(ontocape_upper_level_system.numericalValue.iri),\
                         Literal(int(builtyear))))
            
            graph.add((URIRef(pp_root_node), URIRef(ontoeip_upper_level_system_v1.isOwnedBy.iri), URIRef(pp_namespace + ukpp.OwnerKey + plantname)))
            graph.add((URIRef(pp_namespace + ukpp.OwnerKey + plantname), RDF.type, URIRef(ontoeip_upper_level_system_v1.Organization.iri)))
            graph.add((URIRef(pp_namespace + ukpp.OwnerKey + plantname), URIRef(ontoeip_upper_level_system_v1.hasName.iri), Literal(owner)))
    
            graph.add((URIRef(pp_root_node), URIRef(ontocape_upper_level_system.hasAddress.iri), URIRef(t_box.dbr + region)))
            graph.add((URIRef(pp_root_node), URIRef(ontoecape_space_and_time_extended.hasGISCoordinateSystem.iri), URIRef(pp_namespace + ukpp.CoordinateSystemKey + plantname)))
            graph.add((URIRef(pp_namespace + ukpp.CoordinateSystemKey + plantname), RDF.type, URIRef(ontoecape_space_and_time_extended.ProjectedCoordinateSystem.iri)))
            graph.add((URIRef(pp_namespace + ukpp.CoordinateSystemKey + plantname), URIRef(ontoecape_space_and_time_extended.hasProjectedCoordinate_y.iri),\
                       URIRef(pp_namespace + ukpp.LantitudeKey + plantname)))
            graph.add((URIRef(pp_namespace + ukpp.CoordinateSystemKey + plantname), URIRef(ontoecape_space_and_time_extended.hasProjectedCoordinate_x.iri),\
                       URIRef(pp_namespace + ukpp.LongitudeKey + plantname)))
            graph.add((URIRef(pp_namespace + ukpp.LantitudeKey + plantname), RDF.type, URIRef(ontoecape_space_and_time.StraightCoordinate.iri)))   
            graph.add((URIRef(pp_namespace + ukpp.LongitudeKey + plantname), RDF.type, URIRef(ontoecape_space_and_time.StraightCoordinate.iri)))  
            graph.add((URIRef(pp_namespace + ukpp.LantitudeKey + plantname), URIRef(ontocape_upper_level_system.hasDimension.iri), URIRef(ontocape_physical_dimension.length.iri)))
            graph.add((URIRef(pp_namespace + ukpp.LongitudeKey + plantname), URIRef(ontocape_upper_level_system.hasDimension.iri), URIRef(ontocape_physical_dimension.length.iri)))
            graph.add((URIRef(pp_namespace + ukpp.LantitudeKey + plantname), URIRef(ontocape_coordinate_system.refersToAxis.iri), URIRef(t_box.ontoecape_space_and_time + 'y-axis')))
            graph.add((URIRef(pp_namespace + ukpp.LongitudeKey + plantname), URIRef(ontocape_coordinate_system.refersToAxis.iri), URIRef(t_box.ontoecape_space_and_time + 'x-axis')))
            graph.add((URIRef(pp_namespace + ukpp.LantitudeKey + plantname), URIRef(ontocape_upper_level_system.hasValue.iri),\
                       URIRef(pp_namespace + ukpp.valueKey + ukpp.LantitudeKey + plantname)))
            graph.add((URIRef(pp_namespace + ukpp.valueKey + ukpp.LantitudeKey + plantname), RDF.type, URIRef(ontocape_coordinate_system.CoordinateValue.iri)))
            graph.add((URIRef(pp_namespace + ukpp.valueKey + ukpp.LantitudeKey + plantname), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri),\
                       URIRef(ontocape_SI_units.m.iri)))
            graph.add((URIRef(pp_namespace + ukpp.valueKey + ukpp.LantitudeKey + plantname), URIRef(ontocape_upper_level_system.numericalValue.iri),\
                       Literal(float(gpslocation[0].strip('\n')))))
            graph.add((URIRef(pp_namespace + ukpp.LongitudeKey + plantname), URIRef(ontocape_upper_level_system.hasValue.iri),\
                       URIRef(pp_namespace + ukpp.valueKey + ukpp.LongitudeKey + plantname)))
            graph.add((URIRef(pp_namespace + ukpp.valueKey + ukpp.LongitudeKey + plantname), RDF.type, URIRef(ontocape_coordinate_system.CoordinateValue.iri)))
            graph.add((URIRef(pp_namespace + ukpp.valueKey + ukpp.LongitudeKey + plantname), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri),\
                       URIRef(ontocape_SI_units.m.iri)))
            graph.add((URIRef(pp_namespace + ukpp.valueKey + ukpp.LongitudeKey + plantname), URIRef(ontocape_upper_level_system.numericalValue.iri),\
                       Literal(float(gpslocation[1].strip('\n')))))
            
            # specify the owl file storage path
            defaultStoredPath = ukpp.StoreGeneratedOWLs + str(counter) + UNDERSCORE + plantname + '_UK' + OWL #default path
            global filepath, userSpecified
        
            # Store/update the generated owl files      
            if os.path.exists(ukpp.StoreGeneratedOWLs) and not userSpecified:
                print('****Non user specified storage path, will use the default storage path****')
                storeGeneratedOWLs(graph, defaultStoredPath)
        
            elif filepath == None:
                print('****Needs user to specify a storage path****')
                filepath = selectStoragePath()
                filepath_ = filepath + '\\' + str(counter) + UNDERSCORE + plantname + '_UK' + OWL
                storeGeneratedOWLs(graph, filepath_)
            else: 
                filepath_ = filepath + '\\' + str(counter) + UNDERSCORE + plantname + '_UK' + OWL
                storeGeneratedOWLs(graph, filepath_)
            
        counter += 1        
    return 

if __name__ == '__main__':
   addUKPowerPlantTriples()
   print('terminated')