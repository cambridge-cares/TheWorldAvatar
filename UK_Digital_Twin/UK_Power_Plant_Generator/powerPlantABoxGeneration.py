##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 15 Dec 2021          #
##########################################

"""This module is designed to generate and update the A-box of UK power plant graph."""

import os
import owlready2
from rdflib.extras.infixowl import OWL_NS
from rdflib import Graph, URIRef, Literal, ConjunctiveGraph
from rdflib.namespace import RDF, RDFS
from rdflib.plugins.sleepycat import Sleepycat
from rdflib.store import NO_STORE, VALID_STORE
import sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from UK_Digital_Twin_Package import UKDigitalTwinTBox as T_BOX
from UK_Digital_Twin_Package import DUKESDataProperty as DUKES
from UK_Digital_Twin_Package import UKPowerPlant as UKpp
from UK_Digital_Twin_Package.GraphStore import LocalGraphStore
from UK_Digital_Twin_Package.LACodeOfOfficialRegion import LACodeOfOfficialRegion as LACode
from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs, selectStoragePath, readFile, specifyValidFilePath

"""Notation used in URI construction"""
HASH = '#'
SLASH = '/'
UNDERSCORE = '_'
OWL = '.owl'

"""Create an instance of Class UKDigitalTwin"""
dt = UKDT.UKDigitalTwin()

"""Create an object of Class UKDigitalTwinTBox"""
t_box = T_BOX.UKDigitalTwinTBox()

"""Create an object of Class UKPowerPlant"""
ukpp = UKpp.UKPowerPlant()

"""OWL file storage path"""
defaultStoredPath = ukpp.StoreGeneratedOWLs

"""Sleepycat storage path"""
userSpecifiePath_Sleepycat = None # user specified path
userSpecified_Sleepycat = False # storage mode: False: default, True: user specified
defaultPath_Sleepycat = ukpp.SleepycatStoragePath

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
ontocape_SI_units               = owlready2.get_ontology(t_box.ontocape_SI_units).load()

"""User specified folder path"""
filepath = None # user specified path
userSpecified = False # storage mode: False: default, True: user specified

"""UK electricity system URL"""
UKDigitalTwinURL = UKDT.nodeURIGenerator(1, dt.topNode, None)
UKElectricitySystem = UKDT.nodeURIGenerator(2, dt.electricitySystem, None)

### Functions ### 
""" Create the DUKESDataProperty Instance by specifying its version """
def createDUKESDataPropertyInstance(version):
    dukes = DUKES.DUKESData(version)
    
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
    
    # uriSplit = UKDT.nodeURIGenerator(3, dt.powerPlant, dukes.VERSION).split('.owl') 
    # root_uri = uriSplit[0]
    
    root_uri = UKDT.nodeURIGenerator(2, dt.powerPlant, None)
    
    fileNum = len(plantnameArrays)
    
    return dukes, plantnameArrays, planttypeArrays, energygenArrays, gentechArrays, primaryfuelArrays, \
        designcapacityArrays, builtYearArrays, ownerArrays, gpslocationArrays, regionArrays, root_uri, fileNum

"""Main Function: Add Triples to the named graph"""
def addUKPowerPlantTriples(storeType, version, OWLFileStoragePath, updateLocalOWLFile = True):  
    global userSpecifiePath_Sleepycat, userSpecified_Sleepycat, defaultPath_Sleepycat
    filepath = specifyValidFilePath(defaultStoredPath, OWLFileStoragePath, updateLocalOWLFile)
    if filepath == None:
        return
    store = LocalGraphStore(storeType)   
    if isinstance(store, Sleepycat):    
        # Create Conjunctive graph maintain all power plant graphs
        powerPlantConjunctiveGraph = ConjunctiveGraph(store=store)
        
        if os.path.exists(defaultPath_Sleepycat) and not userSpecified_Sleepycat:
            print('****Non user specified Sleepycat storage path, will use the default storage path****')
            sl = powerPlantConjunctiveGraph.open(defaultPath_Sleepycat, create = False)
        
        elif userSpecifiePath_Sleepycat == None:
            print('****Needs user to specify a Sleepycat storage path****')
            userSpecifiePath_Sleepycat = selectStoragePath()
            userSpecifiePath_Sleepycat_ = userSpecifiePath_Sleepycat + '\\' + 'ConjunctiveGraph_UKPowerPlant'
            sl = powerPlantConjunctiveGraph.open(userSpecifiePath_Sleepycat_, create = False)   
        
        if sl == NO_STORE:
        # There is no underlying Sleepycat infrastructure, so create it
            powerPlantConjunctiveGraph.open(defaultPath_Sleepycat, create=True)
        else:
            assert sl == VALID_STORE, "The underlying sleepycat store is corrupt"
            
    dukes, plantnameArrays, planttypeArrays, energygenArrays, gentechArrays, primaryfuelArrays, designcapacityArrays, builtYearArrays, ownerArrays, \
        gpslocationArrays, regionArrays, root_uri, fileNum = createDUKESDataPropertyInstance(version)     
        
    counter = 0
    while(counter < fileNum): 
        plantname = ''.join(plantnameArrays[counter]).strip('\n').strip(' ')
        planttype = ''.join(planttypeArrays[counter]).strip('\n').strip(' ')
        # energygen = ''.join(energygenArrays[counter]).strip('\n').strip(' ')
        energygen = "PowerGeneration"
        gentech = ''.join(gentechArrays[counter]).strip('\n').strip(' ')
        primaryfueltype = ''.join(primaryfuelArrays[counter][0]).strip('\n').strip(' ')
        primaryfuellabel = ''.join(primaryfuelArrays[counter][1]).strip('\n').strip(' ')
        designcapacity = ''.join(designcapacityArrays[counter]).strip('\n').strip(' ')
        builtyear = ''.join(builtYearArrays[counter]).strip('\n').strip(' ')
        owner = ''.join(ownerArrays[counter]).strip('\n').strip(' ')
        gpslocation = gpslocationArrays[counter]
        region = ''.join(regionArrays[counter]).strip('\n').strip(' ')
        
        if  len(planttypeArrays) != fileNum or len(energygenArrays) != fileNum or len(gentechArrays) != fileNum or len(primaryfuelArrays) != fileNum or\
            len(designcapacityArrays) != fileNum or len(builtYearArrays) != fileNum or len(ownerArrays) != fileNum or len(gpslocationArrays) != fileNum:
            raise Exception('The list length of each data files does not match')
        else:
            pp_root_node = root_uri + plantname # the top node of the named graph
            # pp_namespace = root_uri + plantname + SLASH
            
            #attribute IRRs
            ontologyIRI = dt.baseURL + SLASH + dt.topNode + SLASH + plantname
            UKElectricitySystemIRI = UKElectricitySystem + dt.UK
            LocalElectricitySystemIRI = UKElectricitySystem + plantname
            RealizationAspectIRI = dt.baseURL + SLASH + t_box.ontoeipName +  SLASH + ukpp.RealizationAspectKey + plantname
            EnergyGenerationIRI = dt.baseURL + SLASH + t_box.ontoeipName +  SLASH + energygen + UNDERSCORE + plantname
            GenerationTechnologyIRI = dt.baseURL + SLASH + t_box.ontoeipName +  SLASH + ukpp.GenerationTechnologyKey + gentech
            PrimaryFuelTypeIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + primaryfueltype + UNDERSCORE + plantname
            RequirementsAspectIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + ukpp.RequirementsAspectKey + plantname
            valueOfRequirementsAspectIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + ukpp.valueKey + ukpp.RequirementsAspectKey + plantname
            BuiltYearIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + ukpp.BuiltYearKey + plantname
            valueOfBuiltYearIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + ukpp.valueKey + ukpp.BuiltYearKey + plantname
            OwnerIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + ukpp.OwnerKey + plantname
            
            latlon = str(gpslocation[0].strip('\n').strip(' ').replace(' ', '') + '#' + gpslocation[1].strip('\n').strip(' ').replace(' ', '')).replace('\xa0', '')
           
            # Create rdf graph with identifier
            graph = Graph(store = store, identifier = URIRef(ontologyIRI))
            
            # Import T-boxes
            graph.set((graph.identifier, RDF.type, OWL_NS['Ontology']))
            graph.add((graph.identifier, OWL_NS['imports'], URIRef(t_box.ontoecape_technical_system)))
            graph.add((graph.identifier, OWL_NS['imports'], URIRef(t_box.ontoeip_powerplant)))
            graph.set((graph.identifier, RDFS.comment, Literal('This ontology represents the energy system of the UK')))
            graph.set((graph.identifier, RDFS.label, Literal('UK Digital Twin - Energy System')))
            
            # Add rdf.type
            graph.add((URIRef(pp_root_node), RDF.type, URIRef(ontoeip_powerplant.PowerPlant.iri)))
            graph.add((URIRef(pp_root_node), RDF.type, URIRef(ontoecape_technical_system.TechnicalSystem.iri)))
            graph.add((URIRef(pp_root_node), RDF.type, URIRef(t_box.ontopowsys_PowSysRealization + planttype)))
            graph.add((URIRef(pp_root_node), RDF.type, URIRef(t_box.ontoenergysystem + 'Asset'))) # The power plant is specifically declared as an asset  
            graph.add((URIRef(pp_root_node), RDFS.label, Literal(str(plantname)))) 
            graph.add((URIRef(pp_root_node), RDFS.label, Literal("DUKES_Data_Version_" + str(dukes.VERSION))))
            
            # Link the power plant with the UK electricity system
            graph.add((URIRef(LocalElectricitySystemIRI), URIRef(ontocape_upper_level_system.contains.iri), URIRef(pp_root_node)))
            graph.add((URIRef(LocalElectricitySystemIRI), RDF.type, URIRef(t_box.ontoenergysystem + 'ElectricPowerSystem')))
            graph.add((URIRef(UKElectricitySystemIRI), RDF.type, URIRef(t_box.ontoenergysystem + 'ElectricPowerSystem')))
            graph.add((URIRef(LocalElectricitySystemIRI), URIRef(ontocape_upper_level_system.isDirectSubsystemOf.iri), URIRef(UKElectricitySystemIRI)))
            
            # Add Realization Aspect  
            graph.add((URIRef(pp_root_node), URIRef(ontoecape_technical_system.hasRealizationAspect.iri), URIRef(RealizationAspectIRI)))
            graph.add((URIRef(RealizationAspectIRI), RDF.type, URIRef(ontoeip_powerplant.PowerGenerator.iri)))
            graph.add((URIRef(RealizationAspectIRI), URIRef(ontoecape_technical_system.realizes.iri), URIRef(EnergyGenerationIRI)))
            
            graph.add((URIRef(EnergyGenerationIRI), RDF.type, URIRef(ontoeip_powerplant.PowerGeneration.iri)))                
            graph.add((URIRef(EnergyGenerationIRI), URIRef(ontoeip_powerplant.usesGenerationTechnology.iri), URIRef(GenerationTechnologyIRI)))
                
            graph.add((URIRef(GenerationTechnologyIRI), RDF.type, URIRef(ontoeip_powerplant.PlantGenerationTechnology.iri)))
            graph.add((URIRef(EnergyGenerationIRI), URIRef(ontoeip_powerplant.consumesPrimaryFuel.iri), URIRef(PrimaryFuelTypeIRI)))
            graph.add((URIRef(PrimaryFuelTypeIRI), RDF.type, URIRef(t_box.ontoeip_powerplant + primaryfueltype)))
            graph.add((URIRef(PrimaryFuelTypeIRI), RDFS.label, Literal(str(primaryfuellabel))))
            
            # Add Functional Aspect  
            graph.add((URIRef(pp_root_node), URIRef(ontoecape_technical_system.hasFunctionalAspect.iri), URIRef(EnergyGenerationIRI)))
            
            # Add Requirements Aspect
            graph.add((URIRef(pp_root_node), URIRef(ontoecape_technical_system.hasRequirementsAspect.iri), URIRef(RequirementsAspectIRI)))
            graph.add((URIRef(RequirementsAspectIRI), RDF.type, URIRef(t_box.ontoeip_system_requirement + 'DesignCapacity'))) # T-box undefined
            graph.add((URIRef(RequirementsAspectIRI), URIRef(ontoecape_technical_system.isAchievedThrough.iri), URIRef(EnergyGenerationIRI)))
            # # add values to attributes
            graph.add((URIRef(RequirementsAspectIRI), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(valueOfRequirementsAspectIRI)))
            graph.add((URIRef(valueOfRequirementsAspectIRI), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
            graph.add((URIRef(valueOfRequirementsAspectIRI), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(ontocape_derived_SI_units.MW.iri)))
            graph.add((URIRef(valueOfRequirementsAspectIRI), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(designcapacity))))
            # Add other attributes
            graph.add((URIRef(pp_root_node), URIRef(ontoeip_powerplant.hasYearOfBuilt.iri), URIRef(BuiltYearIRI)))
            graph.add((URIRef(BuiltYearIRI), RDF.type, URIRef(ontoeip_powerplant.YearOfBuilt.iri)))
            graph.add((URIRef(BuiltYearIRI), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(valueOfBuiltYearIRI)))
            graph.add((URIRef(valueOfBuiltYearIRI), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
            graph.add((URIRef(valueOfBuiltYearIRI), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(int(builtyear))))
            
            graph.add((URIRef(pp_root_node), URIRef(ontoeip_upper_level_system_v1.isOwnedBy.iri), URIRef(OwnerIRI)))
            graph.add((URIRef(OwnerIRI), RDF.type, URIRef(ontoeip_upper_level_system_v1.Organization.iri)))
            graph.add((URIRef(OwnerIRI), URIRef(ontoeip_upper_level_system_v1.hasName.iri), Literal(owner)))
    
            # Apply the OntoEnergySystem for representing the asset with LA code and its lat-lon
            graph.add((URIRef(pp_root_node), URIRef(t_box.ontoenergysystem + 'hasRelevantPlace'), URIRef(t_box.dbr + region)))
            graph.add((URIRef(t_box.dbr + region), RDF.type, URIRef(t_box.ontoenergysystem + 'AdministrativeDivision')))
            graph.add((URIRef(t_box.dbr + region), URIRef(t_box.ontoenergysystem + 'hasLocalAuthorityCode'), Literal(str(LACode[region]))))
            graph.add((URIRef(pp_root_node), URIRef(t_box.ontoenergysystem + 'hasWGS84LatitudeLongitude'), \
                       Literal(latlon, datatype = 'http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon')))
            
            # generate/update OWL files
            if updateLocalOWLFile == True:
                # Store/update the generated owl files      
                if filepath[-2:] != '\\': 
                    filepath_ = filepath + '\\' + str(counter) + UNDERSCORE + plantname + '_UK' + OWL
                else:
                    filepath_ = filepath + str(counter) + UNDERSCORE + plantname + '_UK' + OWL
                storeGeneratedOWLs(graph, filepath_)
                  
        counter += 1 
    
    if isinstance(store, Sleepycat):  
        powerPlantConjunctiveGraph.close()
    return

if __name__ == '__main__':
    addUKPowerPlantTriples('default', 2019, None, True)
    print('terminated')