##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 06 Sept 2023         #
##########################################

"""This module is designed to generate and update the A-box of UK power plant graph."""

import os
import owlready2
from rdflib.extras.infixowl import OWL_NS
from rdflib import Graph, URIRef, Literal
from rdflib.namespace import RDF, RDFS
import sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from UK_Digital_Twin_Package import UKDigitalTwinTBox as T_BOX
from UK_Digital_Twin_Package import DUKESDataProperty as DUKES
from UK_Digital_Twin_Package import UKPowerPlant as UKpp
from UK_Digital_Twin_Package.LACodeOfOfficialRegion import LACodeOfOfficialRegion as LACode
from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs, readFile
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

"""Create an object of Class UKPowerPlant"""
ukpp = UKpp.UKPowerPlant()

"""T-Box URI"""
ontocape_upper_level_system     = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
ontocape_technical_system      = owlready2.get_ontology(t_box.ontoecape_technical_system).load()
ontoeip_powerplant              = owlready2.get_ontology(t_box.ontoeip_powerplant).load()
ontopowsys_PowSysRealization    = owlready2.get_ontology(t_box.ontopowsys_PowSysRealization).load()
ontocape_derived_SI_units       = owlready2.get_ontology(t_box.ontocape_derived_SI_units).load()
ontoeip_upper_level_system_v1   = owlready2.get_ontology(t_box.ontoeip_upper_level_system_v1).load()
ontoecape_space_and_time_extended = owlready2.get_ontology(t_box.ontoecape_space_and_time_extended).load()
ontoecape_space_and_time        = owlready2.get_ontology(t_box.ontoecape_space_and_time).load()
ontocape_physical_dimension     = owlready2.get_ontology(t_box.ontocape_physical_dimension).load()
ontocape_coordinate_system      = owlready2.get_ontology(t_box.ontocape_coordinate_system).load()
ontocape_SI_units               = owlready2.get_ontology(t_box.ontocape_SI_units).load()
ontoenergysystem                = owlready2.get_ontology(t_box.ontoenergysystem).load()

"""UK electricity system URL"""
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
    
    root_uri = UKDT.nodeURIGenerator(2, dt.powerPlant, None)
    
    fileNum = len(plantnameArrays)
    
    return dukes, plantnameArrays, planttypeArrays, energygenArrays, gentechArrays, primaryfuelArrays, \
        designcapacityArrays, builtYearArrays, ownerArrays, gpslocationArrays, regionArrays, root_uri, fileNum

"""Main Function: Add Triples to the named graph"""
def addUKPowerPlantTriples(version, updateEndpointIRI, KGFileStoragePath, updateEndpoint = True):  
    
    ## Validate the file path
    folder = os.path.exists(KGFileStoragePath)
    if not folder:                
        os.makedirs(KGFileStoragePath)           
        print("---  New folder %s...  ---" % KGFileStoragePath)

    ## Specify the DUKES data properties      
    dukes, plantnameArrays, planttypeArrays, energygenArrays, gentechArrays, primaryfuelArrays, designcapacityArrays, builtYearArrays, ownerArrays, \
        gpslocationArrays, regionArrays, root_uri, fileNum = createDUKESDataPropertyInstance(version)     
     
    ## Define the power system IRIs
    UKElectricitySystemIRI = UKElectricitySystem + str(uuid.uuid4())            
    GBElectricitySystemIRI = UKElectricitySystem + str(uuid.uuid4())
    NIElectricitySystemIRI = UKElectricitySystem + str(uuid.uuid4())
    
    UKAdministrativeDivisionIRI = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukpp.AdministrativeDivisionKey + str(uuid.uuid4())
    GBAdministrativeDivisionIRI = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukpp.AdministrativeDivisionKey + str(uuid.uuid4())
    NIAdministrativeDivisionIRI = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukpp.AdministrativeDivisionKey + str(uuid.uuid4())    

    ## Create the power plant entities  
    counter = 0
    while(counter < fileNum): 
        plantname = ''.join(plantnameArrays[counter]).strip('\n').strip(' ')
        planttype = ''.join(planttypeArrays[counter]).strip('\n').strip(' ')
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
            raise Exception('The list length of each data files does not match.')
        else:
            
            ## root iri and ontology iri
            pp_root_node = root_uri + str(uuid.uuid4()) # plantname ## the top node of the named graph
            ontologyIRI = dt.baseURL + SLASH + dt.topNode + SLASH + str(uuid.uuid4())       
            ## attribute IRIs
            GeneratorIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + ukpp.RealizationAspectKey + str(uuid.uuid4()) 
            EnergyGenerationIRI = dt.baseURL + SLASH + t_box.ontoeipName +  SLASH + ukpp.PowerGenerationKey + str(uuid.uuid4()) 
            GenerationTechnologyClassIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + gentech 
            GenerationTechnologyIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + gentech + UNDERSCORE + str(uuid.uuid4())
            PrimaryFuelTypeIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + primaryfueltype + UNDERSCORE + str(uuid.uuid4()) 
            RequirementsAspectIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + ukpp.RequirementsAspectKey + str(uuid.uuid4()) 
            valueOfRequirementsAspectIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + ukpp.valueKey + str(uuid.uuid4())
            BuiltYearIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + ukpp.BuiltYearKey + str(uuid.uuid4())
            valueOfBuiltYearIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + ukpp.valueKey + str(uuid.uuid4())
            OwnerIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + ukpp.OwnerKey + str(uuid.uuid4())
            powerPlantRelevantPlaceIRI = dt.baseURL + SLASH + t_box.ontoenergysystemName + SLASH + ukpp.AdministrativeDivisionKey + str(uuid.uuid4())
            
            latlon = str(gpslocation[0].strip('\n').strip(' ').replace(' ', '') + '#' + gpslocation[1].strip('\n').strip(' ').replace(' ', '')).replace('\xa0', '')
           
            ## Create rdf graph with identifier
            graph = Graph(store = 'default', identifier = URIRef(ontologyIRI))
            
            ## Import T-boxes and add attributes of the ontology
            graph.set((graph.identifier, RDF.type, OWL_NS['Ontology']))
            graph.add((graph.identifier, OWL_NS['imports'], URIRef(t_box.ontoecape_technical_system)))
            graph.add((graph.identifier, OWL_NS['imports'], URIRef(t_box.ontoeip_powerplant)))
            graph.set((graph.identifier, RDFS.comment, Literal('This ontology represents power plant entities of the UK energy system.')))
            graph.set((graph.identifier, RDFS.label, Literal('UK Digital Twin - Energy System - Power Plant - ' + plantname)))
            
            ## Add rdf.type and label to power plant
            graph.add((URIRef(pp_root_node), RDF.type, URIRef(ontoeip_powerplant.PowerPlant.iri)))
            graph.add((URIRef(pp_root_node), RDF.type, URIRef(ontocape_technical_system.TechnicalSystem.iri)))
            graph.add((URIRef(pp_root_node), RDF.type, URIRef(t_box.ontopowsys_PowSysRealization + planttype)))
            graph.add((URIRef(pp_root_node), RDF.type, URIRef(ontoenergysystem.Asset.iri))) # The power plant is specifically declared as an asset  
            graph.add((URIRef(pp_root_node), RDFS.label, Literal(str(plantname)))) 
            graph.add((URIRef(pp_root_node), RDFS.comment, Literal("The DUKES Data Version is " + str(dukes.VERSION))))
            
            ## Create the enetity of the UK electricity system
            graph.add((URIRef(UKElectricitySystemIRI), URIRef(ontoenergysystem.hasRelevantPlace.iri), URIRef(UKAdministrativeDivisionIRI)))
            graph.add((URIRef(UKAdministrativeDivisionIRI), OWL_NS['sameAs'], URIRef(t_box.dbr + dt.UK)))
            graph.add((URIRef(UKAdministrativeDivisionIRI), RDF.type, URIRef(ontoenergysystem.AdministrativeDivision.iri)))
            graph.add((URIRef(UKAdministrativeDivisionIRI), URIRef(ontoenergysystem.hasLocalAuthorityCode.iri), Literal(LACode['United_Kingdom'])))
            
            ## Link the power plant with the UK electricity system and GB or NI electricity system
            if region == dt.NI:
                graph.add((URIRef(NIElectricitySystemIRI), URIRef(ontocape_upper_level_system.contains.iri), URIRef(pp_root_node)))
                graph.add((URIRef(NIElectricitySystemIRI), RDF.type, URIRef(ontoenergysystem.ElectricPowerSystem.iri)))
                graph.add((URIRef(UKElectricitySystemIRI), RDF.type, URIRef(ontoenergysystem.ElectricPowerSystem.iri)))
                graph.add((URIRef(NIElectricitySystemIRI), URIRef(ontocape_upper_level_system.isDirectSubsystemOf.iri), URIRef(UKElectricitySystemIRI)))
                
                graph.add((URIRef(NIElectricitySystemIRI), URIRef(ontoenergysystem.hasRelevantPlace.iri), URIRef(NIAdministrativeDivisionIRI)))
                graph.add((URIRef(NIAdministrativeDivisionIRI), OWL_NS['sameAs'], URIRef(t_box.dbr + dt.NI)))
                graph.add((URIRef(NIAdministrativeDivisionIRI), RDF.type, URIRef(ontoenergysystem.AdministrativeDivision.iri)))
                graph.add((URIRef(NIAdministrativeDivisionIRI), URIRef(ontoenergysystem.hasLocalAuthorityCode.iri), Literal(LACode['Northern_Ireland'])))                           
            else:
                graph.add((URIRef(GBElectricitySystemIRI), URIRef(ontocape_upper_level_system.contains.iri), URIRef(pp_root_node)))
                graph.add((URIRef(GBElectricitySystemIRI), RDF.type, URIRef(ontoenergysystem.ElectricPowerSystem.iri)))
                graph.add((URIRef(UKElectricitySystemIRI), RDF.type, URIRef(ontoenergysystem.ElectricPowerSystem.iri)))
                graph.add((URIRef(GBElectricitySystemIRI), URIRef(ontocape_upper_level_system.isDirectSubsystemOf.iri), URIRef(UKElectricitySystemIRI)))
                
                graph.add((URIRef(GBElectricitySystemIRI), URIRef(ontoenergysystem.hasRelevantPlace.iri), URIRef(GBAdministrativeDivisionIRI)))
                graph.add((URIRef(GBAdministrativeDivisionIRI), OWL_NS['sameAs'], URIRef(t_box.dbr + dt.GB)))
                graph.add((URIRef(GBAdministrativeDivisionIRI), RDF.type, URIRef(ontoenergysystem.AdministrativeDivision.iri)))
                graph.add((URIRef(GBAdministrativeDivisionIRI), URIRef(ontoenergysystem.hasLocalAuthorityCode.iri), Literal(LACode['Great_Britain'])))
            
            ## Add Realization Aspect (PowerGenerator)
            graph.add((URIRef(pp_root_node), URIRef(ontocape_technical_system.hasRealizationAspect.iri), URIRef(GeneratorIRI)))
            graph.add((URIRef(GeneratorIRI), RDF.type, URIRef(ontoeip_powerplant.PowerGenerator.iri)))
            graph.add((URIRef(GeneratorIRI), URIRef(ontocape_technical_system.realizes.iri), URIRef(EnergyGenerationIRI)))
            
            graph.add((URIRef(EnergyGenerationIRI), RDF.type, URIRef(ontoeip_powerplant.PowerGeneration.iri)))                
            graph.add((URIRef(EnergyGenerationIRI), URIRef(ontoeip_powerplant.usesGenerationTechnology.iri), URIRef(GenerationTechnologyIRI)))
            
            graph.add((URIRef(GenerationTechnologyIRI), RDF.type, URIRef(GenerationTechnologyClassIRI)))
            graph.add((URIRef(GenerationTechnologyClassIRI), RDFS.subClassOf, URIRef(ontoeip_powerplant.PlantGenerationTechnology.iri)))
            graph.add((URIRef(EnergyGenerationIRI), URIRef(ontoeip_powerplant.consumesPrimaryFuel.iri), URIRef(PrimaryFuelTypeIRI)))
            graph.add((URIRef(PrimaryFuelTypeIRI), RDF.type, URIRef(t_box.ontoeip_powerplant + primaryfueltype)))
            graph.add((URIRef(PrimaryFuelTypeIRI), RDFS.label, Literal(str(primaryfuellabel))))
            
            ## Add Functional Aspect  
            graph.add((URIRef(pp_root_node), URIRef(ontocape_technical_system.hasFunctionalAspect.iri), URIRef(EnergyGenerationIRI)))
            
            ## Add Requirements Aspect
            graph.add((URIRef(pp_root_node), URIRef(ontocape_technical_system.hasRequirementsAspect.iri), URIRef(RequirementsAspectIRI)))
            graph.add((URIRef(RequirementsAspectIRI), RDF.type, URIRef(t_box.ontoeip_system_requirement + 'DesignCapacity'))) # T-box undefined
            graph.add((URIRef(RequirementsAspectIRI), URIRef(ontocape_technical_system.isAchievedThrough.iri), URIRef(EnergyGenerationIRI)))
            ## Add values to attributes
            graph.add((URIRef(RequirementsAspectIRI), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(valueOfRequirementsAspectIRI)))
            graph.add((URIRef(valueOfRequirementsAspectIRI), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
            graph.add((URIRef(valueOfRequirementsAspectIRI), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(ontocape_derived_SI_units.MW.iri)))
            graph.add((URIRef(valueOfRequirementsAspectIRI), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(designcapacity))))
            ## Add other attributes
            graph.add((URIRef(pp_root_node), URIRef(ontoeip_powerplant.hasYearOfBuilt.iri), URIRef(BuiltYearIRI)))
            graph.add((URIRef(BuiltYearIRI), RDF.type, URIRef(ontoeip_powerplant.YearOfBuilt.iri)))
            graph.add((URIRef(BuiltYearIRI), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(valueOfBuiltYearIRI)))
            graph.add((URIRef(valueOfBuiltYearIRI), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
            graph.add((URIRef(valueOfBuiltYearIRI), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(int(builtyear))))
            
            graph.add((URIRef(pp_root_node), URIRef(ontoeip_upper_level_system_v1.isOwnedBy.iri), URIRef(OwnerIRI)))
            graph.add((URIRef(OwnerIRI), RDF.type, URIRef(ontoeip_upper_level_system_v1.Organization.iri)))
            graph.add((URIRef(OwnerIRI), URIRef(ontoeip_upper_level_system_v1.hasName.iri), Literal(owner)))
            
            graph.add((URIRef(pp_root_node), URIRef(ontoenergysystem.hasRelevantPlace.iri), URIRef(powerPlantRelevantPlaceIRI)))
            graph.add((URIRef(powerPlantRelevantPlaceIRI), OWL_NS['sameAs'], URIRef(t_box.dbr + region)))
            graph.add((URIRef(powerPlantRelevantPlaceIRI), RDF.type, URIRef(ontoenergysystem.AdministrativeDivision.iri)))
            graph.add((URIRef(powerPlantRelevantPlaceIRI), URIRef(ontoenergysystem.hasLocalAuthorityCode.iri), Literal(str(LACode[region]))))
            graph.add((URIRef(pp_root_node), URIRef(ontoenergysystem.hasWGS84LatitudeLongitude.iri), \
                       Literal(latlon, datatype = 'http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon')))
                
            # print(graph.serialize(format="turtle").decode("utf-8"))
            
            ## generate/update OWL files
            if updateEndpoint == True:
                # Store/update the generated owl files      
                if KGFileStoragePath[-1:] != '/': 
                    filepath_ = KGFileStoragePath + '/' + str(counter) + UNDERSCORE + plantname + '_UK' + TTL
                else:
                    filepath_ = KGFileStoragePath + str(counter) + UNDERSCORE + plantname + '_UK' + TTL
                storeGeneratedOWLs(graph, filepath_)
            
            sparql_client = PySparqlClient(updateEndpointIRI, updateEndpointIRI)
            sparql_client.uploadOntology(filepath_)                 
        counter += 1 

    return UKElectricitySystemIRI, GBElectricitySystemIRI, NIElectricitySystemIRI