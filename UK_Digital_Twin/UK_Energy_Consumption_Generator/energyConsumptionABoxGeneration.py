##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 13 Oct 2021          #
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
from UK_Digital_Twin_Package import ONSNamespace

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

### Functions ### 
""" The Energy Consumption DataProperty Instance constructor"""
def createEnergyConsumptionDataPropertyInstance(version):
    engconsump = EngConsump.EnergyConsumptionData(version)   
    elecConDataArrays = readFile(engconsump.ElectricityConsumptionData)      
    uriSplit = UKDT.nodeURIGenerator(3, dt.energyConsumption, engconsump.VERSION).split('.owl') 
    root_uri = uriSplit[0] #namespace   
    fileNum = len(elecConDataArrays)  # substruct the first header line 
    
    return engconsump, elecConDataArrays, root_uri, fileNum

def test_returnLACode(version):
    engconsump, elecConDataArrays, root_uri, fileNum = createEnergyConsumptionDataPropertyInstance(version)
    n = 1
    LACode = []
    while n < len(elecConDataArrays):
        LACode.append(str(elecConDataArrays[n][1]))        
        n+=1
    return LACode

"""Add Triples to the target nodes""" 
def addRegionalAndLocalNodes(graph, engconsump, elecConDataArrays, root_uri, indexOfLocalArea, indexOfRegion):
    while indexOfLocalArea <= indexOfRegion:       
        if len(elecConDataArrays[indexOfLocalArea]) != len(engconsump.headerElectricityConsumption):
            print('The data is not sufficient, please check the data file')
            return None
        else:
            elecConData = elecConDataArrays[indexOfLocalArea]
            # Check node type: regional or local nodes
            if indexOfLocalArea == indexOfRegion: #elecConData[0] in engconsump.GovernmentOfficeRegions: # regional node
                print('Regional node name is: ' + elecConData[0].strip('\n'))
                ec_root_node = root_uri + SLASH + elecConData[0].strip('\n') + OWL + HASH + elecConData[0].strip('\n') # top node of the named graph
                ec_namespace = root_uri + SLASH + elecConData[0].strip('\n') + OWL + HASH       
                # Import T-boxes
                graph.set((graph.identifier, RDF.type, OWL_NS['Ontology']))
                graph.add((graph.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_upper_level_system)))
                # Add connection between its father node
                graph.add((URIRef(ec_root_node), URIRef(ontocape_upper_level_system.isExclusivelySubsystemOf.iri),\
                            URIRef(UKDT.nodeURIGenerator(3, dt.energyConsumption, engconsump.VERSION))))
                graph.add((URIRef(UKDT.nodeURIGenerator(3, dt.energyConsumption, engconsump.VERSION)), RDFS.label, Literal("UK_Energy_Consumption_" + str(engconsump.VERSION))))
            else: # local node
                print('Local node name is: ' + elecConData[0].strip('\n'))
                ec_root_node = root_uri + SLASH + elecConDataArrays[indexOfRegion][0].strip('\n') + OWL + HASH + elecConData[0].strip('\n') # sub node of the graph identifying the local node
                ec_namespace = root_uri + SLASH + elecConDataArrays[indexOfRegion][0].strip('\n') + OWL + HASH       
                # Add connection between its father node
                graph.add((URIRef(root_uri + SLASH + elecConDataArrays[indexOfRegion][0].strip('\n') + OWL + HASH + elecConDataArrays[indexOfRegion][0].strip('\n')),\
                           URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(ec_root_node)))
            
            # Add rdf.type
            graph.add((URIRef(ec_root_node), RDF.type, URIRef(ontocape_upper_level_system.ExclusiveSubsystem.iri)))
            
            # Add total consumption nodes
            index_total = elecConDataArrays[0].index('Total\n')
            graph.add((URIRef(ec_root_node), URIRef(ontoeip_system_function.consumes.iri), URIRef(ec_namespace + ukec.TotalConsumptionKey + elecConData[0])))
            graph.add((URIRef(ec_namespace + ukec.TotalConsumptionKey + elecConData[0]), RDF.type, URIRef(ontocape_upper_level_system.ScalarQuantity.iri)))
            
            # Specify the time period of the current data and its start time 
            graph.add((URIRef(ec_namespace + ukec.TotalConsumptionKey + elecConData[0]), URIRef(t_box.ontocape_derived_SI_units + 'hasTimePeriod'), \
                       URIRef(ec_namespace + ukec.TimePeriodKey + ukec.IndustrialAndCommercialConsumptionKey + elecConData[0]))) # T-box undefined
            graph.add((URIRef(ec_namespace + ukec.TimePeriodKey + ukec.IndustrialAndCommercialConsumptionKey + elecConData[0]), RDF.type, \
                       URIRef(ontoecape_space_and_time.TimePeriod.iri))) 
            graph.add((URIRef(ec_namespace + ukec.TimePeriodKey + ukec.IndustrialAndCommercialConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.hasValue.iri), \
                       URIRef(ec_namespace + ukec.valueKey + ukec.TimePeriodKey + ukec.IndustrialAndCommercialConsumptionKey + elecConData[0])))
            graph.add((URIRef(ec_namespace + ukec.valueKey + ukec.TimePeriodKey + ukec.IndustrialAndCommercialConsumptionKey + elecConData[0]), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
            graph.add((URIRef(ec_namespace + ukec.valueKey + ukec.TimePeriodKey + ukec.IndustrialAndCommercialConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri),\
                        URIRef(ontocape_derived_SI_units.YEAR.iri)))
            graph.add((URIRef(ec_namespace + ukec.valueKey + ukec.TimePeriodKey + ukec.IndustrialAndCommercialConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.numericalValue.iri),\
                        Literal(1)))  
            graph.add((URIRef(ec_namespace + ukec.valueKey + ukec.TimePeriodKey + ukec.IndustrialAndCommercialConsumptionKey + elecConData[0]), URIRef(ontoecape_space_and_time.hasStartingTime.iri),\
                        URIRef(ec_namespace + ukec.StartTimeKey + ukec.IndustrialAndCommercialConsumptionKey + elecConData[0])))   
            graph.add((URIRef(ec_namespace + ukec.StartTimeKey + ukec.IndustrialAndCommercialConsumptionKey + elecConData[0]), RDF.type, \
                       URIRef(ontocape_coordinate_system.CoordinateValue.iri)))
            graph.add((URIRef(ec_namespace + ukec.StartTimeKey + ukec.IndustrialAndCommercialConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri),\
                    URIRef(t_box.ontocape_derived_SI_units+ 'UTC')))
            graph.add((URIRef(ec_namespace + ukec.StartTimeKey + ukec.IndustrialAndCommercialConsumptionKey + elecConData[0]), URIRef(ontocape_upper_level_system.numericalValue.iri),\
                        Literal(engconsump.startTime_NHH, datatype = XSD.dateTime)))  
                
            # Add total consumption value
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
            graph.add((URIRef(ec_namespace + ukec.TotalConsumptionKey + elecConData[0]), URIRef(t_box.ontocape_mathematical_relation + 'ConsistsOfDemesticElectricityConsumption'),\
                        URIRef(ec_namespace + ukec.DomesticConsumptionKey + elecConData[0]))) # T-box undefined
            graph.add((URIRef(ec_namespace + ukec.TotalConsumptionKey + elecConData[0]), URIRef(t_box.ontocape_mathematical_relation + 'ConsistsOfIndustrialAndCommercialConsumption'),\
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
                    URIRef(ec_namespace + ukec.valueKey + ukec.IndustrialAndCommercialConsumptionKey + elecConData[0])))
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
            else: # local node
                graph.add((URIRef(t_box.dbr + elecConData[0]), RDF.type, URIRef(ontoecape_space_and_time_extended.AddressArea.iri)))
                # Add subdivision relationship between local areas and its region
                graph.add((URIRef(t_box.dbr + elecConDataArrays[indexOfRegion][0].strip('\n')), URIRef(t_box.dbo + 'subdivision'), URIRef(t_box.dbr + elecConData[0])))
            graph.add((URIRef(t_box.dbr + elecConData[0]), URIRef(t_box.dbo + 'areaCode'), Literal(str(elecConData[index_LACode]))))
            graph.add((URIRef(t_box.dbr + elecConData[0]), URIRef('https://zeitkunst.org/bibtex/0.2/bibtex.owl#hasURL'), URIRef(ONSNamespace.ons_id + str(elecConData[index_LACode]))))  
        indexOfLocalArea += 1
        
    return graph, indexOfLocalArea

# Take the first element of the tuple (employed in addUKElectricityConsumptionTriples)
def takeFirst(elem):
    return elem[0]

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
    
    engconsump, elecConDataArrays, root_uri, fileNum = createEnergyConsumptionDataPropertyInstance(version)  
    
    # check the data file header
    if elecConDataArrays[0] == engconsump.headerElectricityConsumption:
        pass
    else:
        raise Exception('The raw data header does not match, please check the raw data file.')
    
    # find the index of the region in the raw data file 
    counter_region = 0     
    numOfRegion = len(engconsump.GovernmentOfficeRegions) 
    # the countainer of the region and ist index pair for navigating the iterative loop
    regionAndIndexPairList = []   
    while (counter_region < numOfRegion): # numOfRegion
        region = engconsump.GovernmentOfficeRegions[counter_region]
        # find the index of region in the data file
        _elecConDataArrays = np.array(elecConDataArrays, dtype = object)
        index_targetRegion = np.argwhere(_elecConDataArrays == region)[0][0] 
        regionAndIndexPair = (int(index_targetRegion), str(region)) 
        regionAndIndexPairList.append(regionAndIndexPair)
        counter_region += 1 
    regionAndIndexPairList.sort(key=takeFirst)
    
    # Add the official region node    
    counter = 1 
    for region in regionAndIndexPairList:       
        #while (counter <= region[1]):
            print('*********************************************************') 
            print('The counter is:', counter) # starts from 1          
            print('The region is:', region[1])
            # the name of the named graph, will be applied as the identifier in method Graph(), without '#' 
            regional_base_uri = root_uri + SLASH + str(region[1]) + OWL 
            
            # Create rdf graph with identifier, regional nodes are named graphs including its local nodes
            graph = Graph(store = store, identifier = URIRef(regional_base_uri)) # graph(store='default', identifier)
            graph, counter = addRegionalAndLocalNodes(graph, engconsump, elecConDataArrays, root_uri, counter, region[0]) # The first counter is the index of local area while the second one is the region       
            
            # generate/update OWL files
            if updateLocalOWLFile == True:
                 # Store/update the generated owl files      
                if filepath[-2:] != "\\": 
                    filepath_ = filepath + '\\' 'UK_energy_consumption_' + region[1] + '_UK_' + str(version) + OWL
                else:
                    filepath_ = filepath + 'UK_energy_consumption_' + region[1] + '_UK_' + str(version) + OWL
                storeGeneratedOWLs(graph, filepath_)
    
    print('####################################The total counter of the area is: ', counter)
    if isinstance(store, Sleepycat):  
        eleConConjunctiveGraph.close()               
    return 

if __name__ == '__main__':
    path = "C:\\Users\\wx243\\Desktop\\test\\new_elec_consump\\"
    addUKElectricityConsumptionTriples('default', 2017, None, True)
    # res = test_returnLACode(2019)
    print('terminated')