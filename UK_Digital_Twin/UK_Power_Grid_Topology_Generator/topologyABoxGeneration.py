##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 07 June 2022         #
##########################################

"""This module is designed to generate and update the A-box of UK power grid topology graph."""
import csv
import owlready2
# import numpy as np
from rdflib.extras.infixowl import OWL_NS
from rdflib import Graph, URIRef, Literal, ConjunctiveGraph
from rdflib.namespace import RDF, RDFS
from rdflib.plugins.sleepycat import Sleepycat
from rdflib.store import NO_STORE
import sys, os
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
from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs, selectStoragePath, readFile, specifyValidFilePath
from UK_Digital_Twin_Package.GraphStore import LocalGraphStore
from UK_Digital_Twin_Package.DistanceCalculator import DistanceBasedOnGPSLocation
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel as endpointList
from UK_Digital_Twin_Package import generatorCluster as genCluster
import uuid

import UK_Power_Grid_Topology_Generator.SPARQLQueriesUsedInTopologyABox as query_topo

"""Notation used in URI construction"""
HASH = '#'
SLASH = '/'
UNDERSCORE = '_'
OWL = '.owl'
KV = 'kV'

"""Graph store"""
store = 'default'

"""Create an instance of Class UKDigitalTwin"""
dt = UKDT.UKDigitalTwin()

"""Create an object of Class UKDigitalTwinTBox"""
t_box = T_BOX.UKDigitalTwinTBox()

# """Decommissioned: Create an object of Class UKPowerGridTopology"""
# uk_topo = UK_Topo.UKPowerGridTopology()

"""Create an object of Class UKPowerPlant"""
ukpp = UKpp.UKPowerPlant()

"""Create an object of Class CO2FactorAndGenCostFactor"""
ukmf = ModelFactor.ModelFactor()

"""Create an object of Class UKEnergyConsumption"""
ukec = UKec.UKEnergyConsumption()

# """Create an object of Class UKPowerGridModel"""
# uk_ebus_model = UK_PG.UKEbusModel()
# uk_eline_model = UK_PG.UKElineModel()
# uk_egen_model = UK_PG.UKEGenModel()

"""Blazegraph UK digital tiwn"""
endpoint_label = endpointList.ukdigitaltwin['label']
endpoint_url = endpointList.ukdigitaltwin['queryendpoint_iri']

def loadTboxs(): 
    """T-Box URI"""
    global ontocape_upper_level_system, ontocape_derived_SI_units, ontoecape_space_and_time_extended, ontocape_network_system, \
        ontopowsys_PowSysFunction, ontoecape_technical_system, ontopowsys_PowSysRealization, ontocape_coordinate_system, ontoecape_space_and_time, \
        ontocape_physical_dimension, ontocape_SI_units, ontocape_geometry, meta_model_topology, ontopowsys_PowSysPerformance,ontoeip_powerplant, ontoenergysystem
 
    ontocape_upper_level_system = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
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
    ontoenergysystem                = owlready2.get_ontology(t_box.ontoenergysystem).load()
    return

"""Data Array"""
modelFactorArrays = readFile(ukmf.CO2EmissionFactorAndCostFactor)

"""User specified folder path"""
filepath = None
userSpecified = False

"""Topology Conjunctive graph identifier"""
topo_cg_id = "http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model"

### Functions ### 
"""Root_node, root_uri (top node) and namespace creator based on different bus number"""
def rootNodeAndNameSpace(numOfBus, numOfBranch, endpoint_label, ElectricitySystemName): 
    root_node = UKDT.nodeURIGenerator(3, dt.gridTopology, numOfBus)
    # gridModelNodeSegment = UKDT.nodeURIGenerator(3, dt.powerGridModel, numOfBus).split(OWL)[0] 
    ontopowsys_namespace = dt.baseURL + SLASH + t_box.ontopowsysName + SLASH
    topology_root_node = root_node + str(uuid.uuid4())
    ElectricitySystemIRI = query_topo.queryElectricitySystemIRI(endpoint_label, ElectricitySystemName)
    topoConfig = {'ontopowsys_namespace': ontopowsys_namespace,
                  'topology_root_node': topology_root_node,
                  'numOfBus': numOfBus,
                  'numOfBranch': numOfBranch,
                  'ElectricitySystemIRI':ElectricitySystemIRI
        }
    return topoConfig

""" Create the TopologicalInformationProperty Instance by specifying its numOfBus and voltageLevel"""
def createTopologicalInformationPropertyInstance(numOfBus, voltageLevel):
    topo_info = TopoInfo.TopologicalInformation(numOfBus, voltageLevel)   
    busInfoArrays = readFile(topo_info.BusInfo)
    branchTopoInfoArrays = readFile(topo_info.BranchInfo)
    return topo_info, busInfoArrays, branchTopoInfoArrays, topo_info.BranchInfo

"""Main function: create the sub graph represents the Topology"""
def createTopologyGraph(topoConfig:dir, generatorClusterFunctionName, voltageLevel, OWLFileStoragePath, updateLocalOWLFile = True, storeType = 'default'):
    ## Loading the T-boxes
    loadTboxs()
    
    ## The configration of the topology
    numOfBus = topoConfig['numOfBus']
    numOfBranch = topoConfig['numOfBranch']
    ontopowsys_namespace = topoConfig['ontopowsys_namespace']
    topology_root_node = topoConfig['topology_root_node']
    ElectricitySystemIRI = topoConfig['ElectricitySystemIRI']
    
    ## initialise the topology instance
    uk_topo = UK_Topo.UKPowerGridTopology(numOfBus)
    
    ## initialise the storage
    defaultStoredPath = uk_topo.StoreGeneratedOWLs
    defaultPath_Sleepycat = uk_topo.SleepycatStoragePath
    filepath = specifyValidFilePath(defaultStoredPath, OWLFileStoragePath, updateLocalOWLFile)
    if filepath == None:
        return
    store = LocalGraphStore(storeType) 
    ## specify the topology properties according to the model type
    topo_info, busInfoArrays, branchTopoInfoArrays, filePathofBranchTopoInfo = createTopologicalInformationPropertyInstance(numOfBus, voltageLevel)

    ## initialise the Sleepycat
    if isinstance(store, Sleepycat): 
        print('Create the graph for ', topo_info.EBus_num, ' buses and ', topo_info.ELine_num, ' branches topology.')
        cg_topo_ukec = ConjunctiveGraph(store=store, identifier = topo_cg_id)
        sl = cg_topo_ukec.open(defaultPath_Sleepycat, create = False)
        if sl == NO_STORE:
            print('Cannot find the specified sleepycat store')
    else:
        cg_topo_ukec = None
    
    ## create a named graph
    ontologyIRI = dt.baseURL + SLASH + dt.topNode + SLASH + str(uuid.uuid4())
    graph = Graph(store = store, identifier = URIRef(ontologyIRI))
       
    ## Import T-boxes
    graph.set((graph.identifier, RDF.type, OWL_NS['Ontology']))
    graph.add((graph.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_network_system)))
    graph.add((graph.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_upper_level_system)))
    graph.set((graph.identifier, RDFS.comment, Literal('This ontology represents topological information of the interconnection of buses and branches of the UK energy system.')))
    graph.set((graph.identifier, RDFS.label, Literal("UK Digital Twin - Energy System - Grid Topology - " + str(numOfBus) + "_Bus_" + str(numOfBranch) + "_Branch")))
    
    ## Add attributes to the topology node
    graph.add((URIRef(topology_root_node), RDF.type, URIRef(t_box.ontoenergysystem + "PowerGridTopology")))
    graph.add((URIRef(t_box.ontoenergysystem + "PowerGridTopology"), RDFS.subClassOf, URIRef(ontocape_network_system.NetworkSystem.iri)))
    graph.add((URIRef(topology_root_node), RDFS.label, Literal("UK_Topology_" + str(numOfBus) + "_Bus_" + str(numOfBranch) + "_Branch")))  
    graph.add((URIRef(topology_root_node), URIRef(t_box.ontoenergysystem + "isTopologyOf"), URIRef(ElectricitySystemIRI))) 
    
    
    ## check the aggregatedBus and return aggregatedBusList
    aggregatedBusList = checkaggregatedBus(numOfBus)
    
    ## create the bus nodes
    graph, orderedBusList, orderedLatlon = addBusTopologyNodes(graph, topo_info.headerBusTopologicalInformation, busInfoArrays, ontopowsys_namespace, topology_root_node, uk_topo)
    
    ## create branch nodes
    graph = addBranchTopologyNodes(graph, numOfBranch, topo_info.headerBranchTopologicalInformation, filePathofBranchTopoInfo, branchTopoInfoArrays, orderedBusList, orderedLatlon, ontopowsys_namespace, topology_root_node, uk_topo)
    
    ## create tehe generator nodes
    graph = addGeneratorTopologyNodes(graph, orderedBusList, orderedLatlon, generatorClusterFunctionName, aggregatedBusList, ontopowsys_namespace, topology_root_node, uk_topo)
    
    # print(graph.serialize(format="turtle").decode("utf-8"))
    
    ## generate/update OWL files
    if updateLocalOWLFile == True:  
        if filepath[-2:] != "\\": 
            filepath_ = filepath + '\\' + 'UK_' + topo_info.Name + OWL
        else:
            filepath_ = filepath + 'UK_' + topo_info.Name + OWL
        storeGeneratedOWLs(graph, filepath_)
        
    if isinstance(store, Sleepycat):  
        cg_topo_ukec.close()       
    return


def addBusTopologyNodes(graph, busTopoheader, busDataArray, ontopowsys_namespace, topology_root_node, uk_topo): 
    print('****************Adding the triples of BusNode****************')
    ## Check the bus data header
    if busDataArray[0] != busTopoheader:
        raise Exception('The bus topoinfo data header is not matched, please check the data file.') 
    
    ## Add attributes to the bus node
    counter = 1
    orderedBusList = []
    orderedLatlon = []
    while counter < len(busDataArray):   
        # print('Counter is ', counter)
        busTopoData = busDataArray[counter]
        busTopoData[2] = str(busTopoData[2]).replace("|", ",")
        print("##Bus number ", counter, " located at ", busTopoData[2])
        ## the bus node uri
        bus_node = ontopowsys_namespace + uk_topo.BusNodeKey + str(uuid.uuid4()) # uk_topo.EquipmentConnection_EBusKey + busTopoData[0].zfill(3) 
        latlon = (str(busTopoData[4].strip('\n')) + '#' + str(busTopoData[3].strip('\n'))).strip(' ').replace(' ', '').replace('\xa0', '')
        ## Link bus node with root node and specify its type
        graph.add((URIRef(topology_root_node), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(bus_node)))
        graph.add((URIRef(bus_node), RDF.type, URIRef(ontopowsys_PowSysRealization.BusNode.iri)))
        ## Add GPS attribute tp BusNode
        graph.add((URIRef(bus_node), URIRef(ontoenergysystem.hasWGS84LatitudeLongitude.iri), Literal(latlon, datatype = 'http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon')))
        orderedBusList.append(bus_node)
        orderedLatlon.append(latlon)
        counter += 1     
    return graph, orderedBusList, orderedLatlon

def addBranchTopologyNodes(graph, numOfBranch, branchTopoHeader, filePathofBranchTopoInfo, branchTopoArray, orderedBusList, orderedLatlon, ontopowsys_namespace, topology_root_node, uk_topo): 
    print("****************Adding the triples of ELineNode****************") 
    ## check the branch topology data header

    for bt in branchTopoArray:
        bt[-1] = str(bt[-1]).replace("\n", "")
    # print(repr(branchTopoArray))
    
    for header in branchTopoArray[0]:
        if not header.strip('\n').strip('\"') in branchTopoHeader:   
            raise Exception('The branch topoinfo data header is not matched, please check the data file.')    
    
    if len(branchTopoHeader) != len(branchTopoArray[0]):
        raise Exception('The length of the branch topoinfo data header does not match, please check the data file.')   
    ## Add the attributes to branches
    counter = 1
    ## Number of the Elines (branches)
    Num_Eline = len(branchTopoArray) - 1    
    if numOfBranch != Num_Eline:
        print("WARNING: the specified number of the ELine does not equal to the one providede from the branch topoplogy information.")
        print("The generation of the ELine topology will continue but please check the number of the ELine in the runing model.")
    while counter <= Num_Eline:
        branchTopoData = branchTopoArray[counter]
        FromBusIndex = int(branchTopoData[0].strip("\n").strip("\"").strip(" ").strip("")) - 1
        ToBusIndex = int(branchTopoData[1].strip('\n').strip("\"").strip(" ").strip("")) - 1
        FromBus_iri = orderedBusList[FromBusIndex]
        ToBus_iri = orderedBusList[ToBusIndex]
        
        ## PowerFlow_ELine node uri
        branch_node = ontopowsys_namespace + uk_topo.OverheadLineKey + str(uuid.uuid4())

        ## Map the branch_node with the rank of the branchTopoArray
        branchTopoArray[counter][2] = branch_node

        ## Link line node with root node and specify its type
        graph.add((URIRef(topology_root_node), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(branch_node)))
        graph.add((URIRef(branch_node), RDF.type, URIRef(ontopowsys_PowSysRealization.ElectricalLine.iri)))
        
        ## link with leaves(from) bus and enters(to) bus
        graph.add((URIRef(branch_node), URIRef(ontocape_network_system.leaves.iri), URIRef(FromBus_iri)))
        graph.add((URIRef(branch_node), URIRef(ontocape_network_system.enters.iri), URIRef(ToBus_iri)))
        ## link buses with its hasOutput and hasInput
        graph.add((URIRef(FromBus_iri), URIRef(ontocape_network_system.hasOutput.iri), URIRef(branch_node)))
        graph.add((URIRef(ToBus_iri), URIRef(ontocape_network_system.hasInput.iri), URIRef(branch_node)))
              
        ## the lat-lon of the branches connected buses
        busLatLon = [float(orderedLatlon[FromBusIndex].split('#')[0]), float(orderedLatlon[FromBusIndex].split('#')[1]), \
                     float(orderedLatlon[ToBusIndex].split('#')[0]), float(orderedLatlon[ToBusIndex].split('#')[1])]
        ## calculate the length of the branches 
        Eline_length = DistanceBasedOnGPSLocation(busLatLon)
        
        ## ELine ShapeRepresentation node uri
        ELine_shape_node = ontopowsys_namespace + uk_topo.ShapeKey + str(uuid.uuid4())
        ELine_length_node = ontopowsys_namespace + uk_topo.LengthKey + str(uuid.uuid4())
        value_ELine_length_node = ontopowsys_namespace + uk_topo.valueKey + str(uuid.uuid4())
        
        ## add ShapeRepresentation (length) of ELine
        graph.add((URIRef(branch_node), URIRef(ontocape_geometry.hasShapeRepresentation.iri), URIRef(ELine_shape_node)))
        graph.add((URIRef(ELine_shape_node), RDF.type, URIRef(ontocape_geometry.Cylinder.iri)))
        graph.add((URIRef(ELine_shape_node), URIRef(ontocape_geometry.has_length.iri), URIRef(ELine_length_node)))
        graph.add((URIRef(ELine_length_node), RDF.type, URIRef(ontocape_geometry.Height.iri)))
        ## value of branch length
        graph.add((URIRef(ELine_length_node), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(value_ELine_length_node)))
        graph.add((URIRef(value_ELine_length_node), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
        graph.add((URIRef(value_ELine_length_node), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(ontocape_derived_SI_units.km.iri)))
        graph.add((URIRef(value_ELine_length_node), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(Eline_length))))
       
        ## add the parallel branches of different voltage levels
        if len(branchTopoArray[0]) > 3: # indicates that there are PARALLEL_CONNECTIONS of the branches topology            
            headerIndex = 3
            while headerIndex < len(branchTopoArray[0]):
                ## parallel connections node uri   
                PARALLEL_CONNECTIONS = ontopowsys_namespace + uk_topo.OHLKey + str(uuid.uuid4())
                ##the parallel conection of each branch (400kV and 275kV)
                graph.add((URIRef(branch_node), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(PARALLEL_CONNECTIONS)))
                graph.add((URIRef(PARALLEL_CONNECTIONS), RDF.type, URIRef(ontopowsys_PowSysRealization.OverheadLine.iri)))
                graph.add((URIRef(PARALLEL_CONNECTIONS), URIRef(t_box.ontopowsys_PowSysRealization + 'hasVoltageLevel'), Literal(str(int(branchTopoArray[0][headerIndex].strip('\n').strip('\"'))) + KV)))
                graph.add((URIRef(PARALLEL_CONNECTIONS), URIRef(t_box.ontopowsys_PowSysRealization + 'hasNumberOfParallelLine'), Literal(int(branchTopoData[headerIndex].strip('\n').strip("\"").strip(" ").strip("")))))
                
                headerIndex +=1     
        counter += 1        

    for bt in branchTopoArray:
        for i in bt:
            i.strip("\n").strip("\"").strip("\'").strip(" ").replace("\n", "").replace("\"", "")           
    ## update BranchTopoInfo with the BranchNodeIRIs 
    with open(filePathofBranchTopoInfo, 'w', newline='') as BranchTopoInfo:
        mywriter = csv.writer(BranchTopoInfo, delimiter=',')
        mywriter.writerows(branchTopoArray)
    print("***The BranchTopoInfo has been updated in " + filePathofBranchTopoInfo) 
    return graph
    
def addGeneratorTopologyNodes(graph, orderedBusList, orderedLatlon, generatorClusterFunctionName, aggregatedBusList, ontopowsys_namespace, topology_root_node, uk_topo):    
    print('****************Adding the triples of GeneratorNode****************')
    # counter = 1 
    if modelFactorArrays[0] != ukmf.headerModelFactor:
        raise Exception('The bus model factor data header is not matched, please check the data file')
    
    busInfoList = []
    i = 0
    while i < len(orderedBusList):
        bus = {"Bus_node": orderedBusList[i],
               "Bus_lat_lon": [float(orderedLatlon[i].split('#')[0]), float(orderedLatlon[i].split('#')[1])]
               }
        busInfoList.append(bus)
        i += 1
    
    res_queryPowerPlantAttributes = list(query_topo.queryPowerPlantAttributes(endpoint_label))
    ## create an instance of class generatorCluster
    gc = genCluster.generatorCluster()
    ## get the cluster method via getattr function 
    genClusterMethod = getattr(gc, generatorClusterFunctionName)
    ## pass the arrguments to the cluster method
    bus_generator_assignment_list = genClusterMethod(busInfoList, res_queryPowerPlantAttributes, aggregatedBusList)
    
    ## check if the allocator method is applicable
    while bus_generator_assignment_list == None:
        generatorClusterFunctionName = str(input('The current generator cluster is not applicable. Please choose another cluster: '))
        ## get the load allocation method via getattr function 
        genClusterMethod = getattr(gc, generatorClusterFunctionName)
        ## pass the arrguments to the cluster method
        bus_generator_assignment_list = genClusterMethod(busInfoList, res_queryPowerPlantAttributes, aggregatedBusList)    

    for busGen in bus_generator_assignment_list: ## Bus_node, EBus, Bus_lat_lon[], Bus_LACode; PowerGenerator, LACode_PP, PP_lat_lon, PrimaryFuel, GenerationTechnology
        ## Link the generator_node, EGen_node and their PowerGenerator of the power plant
        graph.add((URIRef(topology_root_node), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(busGen['PowerGenerator'])))
        graph.add((URIRef(busGen['PowerGenerator']), URIRef(meta_model_topology.hasOutput.iri), URIRef(busGen['Bus_node'])))
       
        ## Add attributes: FixedOperatingCostandMaintenanceCost, VariableOperatingCostandMaintenanceCost, FuelCost, CarbonFactor
        graph = AddCostAttributes(graph, busGen['PrimaryFuel'], busGen['GenerationTechnology'], busGen['PowerGenerator'], modelFactorArrays, ontopowsys_namespace, uk_topo)
        # counter += 1               
     
    # print(graph.serialize(format="turtle").decode("utf-8"))
    return graph

"""This function is designed for added the attributes to the generator cost function"""
def AddCostAttributes(graph, fuelType, genTech, generatorNodeIRI, modelFactorArrays, ontopowsys_namespace, uk_topo): 
    fuelType = str(fuelType.split('#')[1])
    genTech = str(genTech)
    if genTech in ukmf.SMR:
        fuelTypeIndex = 1     
    elif fuelType in ukmf.Nuclear:
        fuelTypeIndex = 2
    elif fuelType in ukmf.Bio:
        fuelTypeIndex = 3
    elif fuelType in ukmf.Coal: 
        fuelTypeIndex = 4
    elif genTech in ukmf.CCGT: 
        fuelTypeIndex = 5
    elif genTech in ukmf.OCGT:  
        fuelTypeIndex = 6
    elif fuelType in ukmf.Solar:  
        fuelTypeIndex = 7
    elif fuelType in ukmf.Hydro:  
        fuelTypeIndex = 8
    elif fuelType in ukmf.PumpHydro:  
        fuelTypeIndex = 9
    elif genTech in ukmf.WindOnshore:  
        fuelTypeIndex = 10    
    elif genTech in ukmf.WindOffshore:  
        fuelTypeIndex = 11    
    elif fuelType in ukmf.Waste:  
        fuelTypeIndex = 12    
    else:
        fuelTypeIndex = 13
    
    EGen_FixedOandMCost_node = ontopowsys_namespace + ukmf.FixMaintenanceCostsKey + str(uuid.uuid4())
    value_EGen_FixedOandMCost_node = ontopowsys_namespace + uk_topo.valueKey + str(uuid.uuid4())
    EGen_VarOandMCost_node = ontopowsys_namespace + ukmf.OperationalExpenditureCostsKey + str(uuid.uuid4())
    value_EGen_VarOandMCost_node = ontopowsys_namespace + uk_topo.valueKey + str(uuid.uuid4())
    EGen_FuelCost_node = ontopowsys_namespace + ukmf.FuelCostsKey + str(uuid.uuid4())
    value_EGen_FuelCost_node = ontopowsys_namespace + uk_topo.valueKey + str(uuid.uuid4())
    EGen_CarbonFactor_node = ontopowsys_namespace + ukmf.CO2EmissionFactorkey + str(uuid.uuid4())
    value_EGen_CarbonFactor_node = ontopowsys_namespace + uk_topo.valueKey + str(uuid.uuid4())
    
    # add FixedOperatingCostandMaintenanceCost
    graph.add((URIRef(generatorNodeIRI), URIRef(ontopowsys_PowSysPerformance.hasFixedMaintenanceCost.iri), URIRef(EGen_FixedOandMCost_node)))
    graph.add((URIRef(EGen_FixedOandMCost_node), RDF.type, URIRef(ontopowsys_PowSysPerformance.FixMaintenanceCosts.iri)))
    graph.add((URIRef(EGen_FixedOandMCost_node), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(value_EGen_FixedOandMCost_node)))
    graph.add((URIRef(value_EGen_FixedOandMCost_node), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
    graph.add((URIRef(value_EGen_FixedOandMCost_node), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(t_box.ontocape_derived_SI_units + 'GBP_per_MWh'))) # undefined unit
    graph.add((URIRef(value_EGen_FixedOandMCost_node), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(modelFactorArrays[fuelTypeIndex][1]))))
    # add VariableOperatingCostandMaintenanceCost  
    graph.add((URIRef(generatorNodeIRI), URIRef(ontopowsys_PowSysPerformance.hasCost.iri), URIRef(EGen_VarOandMCost_node)))
    graph.add((URIRef(EGen_VarOandMCost_node), RDF.type, URIRef(ontopowsys_PowSysPerformance.OperationalExpenditureCosts.iri)))
    graph.add((URIRef(EGen_VarOandMCost_node), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(value_EGen_VarOandMCost_node)))
    graph.add((URIRef(value_EGen_VarOandMCost_node), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
    graph.add((URIRef(value_EGen_VarOandMCost_node), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(t_box.ontocape_derived_SI_units + 'GBP_per_MWh'))) # undefined unit
    graph.add((URIRef(value_EGen_VarOandMCost_node), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(modelFactorArrays[fuelTypeIndex][2]))))
    # add FuelCost  
    graph.add((URIRef(generatorNodeIRI), URIRef(ontopowsys_PowSysPerformance.hasFuelCost.iri), URIRef(EGen_FuelCost_node)))
    graph.add((URIRef(EGen_FuelCost_node), RDF.type, URIRef(ontopowsys_PowSysPerformance.FuelCosts.iri)))
    graph.add((URIRef(EGen_FuelCost_node), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(value_EGen_FuelCost_node)))
    graph.add((URIRef(value_EGen_FuelCost_node), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
    graph.add((URIRef(value_EGen_FuelCost_node), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(t_box.ontocape_derived_SI_units + 'GBP_per_MWh'))) # undefined unit
    graph.add((URIRef(value_EGen_FuelCost_node), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(modelFactorArrays[fuelTypeIndex][3]))))
    # add CarbonFactor 
    graph.add((URIRef(generatorNodeIRI), URIRef(ontoeip_powerplant.hasEmissionFactor.iri), URIRef(EGen_CarbonFactor_node)))
    graph.add((URIRef(EGen_CarbonFactor_node), RDF.type, URIRef(t_box.ontoeip_powerplant + 'CO2EmissionFactor'))) # undefined CO2EmissionFactor
    graph.add((URIRef(EGen_CarbonFactor_node), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(value_EGen_CarbonFactor_node)))
    graph.add((URIRef(value_EGen_CarbonFactor_node), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
    graph.add((URIRef(value_EGen_CarbonFactor_node), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(t_box.ontocape_derived_SI_units + 'GBP_per_MWh'))) # undefined unit
    graph.add((URIRef(value_EGen_CarbonFactor_node), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(modelFactorArrays[fuelTypeIndex][4].strip('\n')))))
    return graph

"""Check the aggregated bus"""
def checkaggregatedBus(numOfBus):
    topo_info = TopoInfo.TopologicalInformation(numOfBus)  
    busInfoArrays = readFile(topo_info.BusInfo)
    aggregatedBusList = []
    for busInfo in busInfoArrays:
        if str(busInfo[5]).strip('\n') == str(topo_info.headerBusTopologicalInformation[5].strip('\n')):
            continue
        elif str(busInfo[5].strip('\n') ) != str(None):  
            aggregatedBus = [str(busInfo[0]).strip('\n'), str(busInfo[5]).strip('\n'), float(busInfo[4].strip('\n')), float(busInfo[3].strip('\n'))] # busNumber, aggregated LA code, lat, lon
            aggregatedBusList.append(aggregatedBus)
    return aggregatedBusList

# if __name__ == '__main__': 
#     # topoConfig = rootNodeAndNameSpace(10, 14, 'ukdigitaltwin_test2', 'Great_Britain')
#     # print(topoConfig)
#     # createTopologyGraph(topoConfig, 'sameRegionWithBus', ["275", "400"], None, True, 'default')
    
#     topoConfig = rootNodeAndNameSpace(29, 99, 'ukdigitaltwin_test2', 'Great_Britain')
#     print(topoConfig)
#     createTopologyGraph(topoConfig, 'closestBus', [], None, True, 'default')
    
#     print('**************Terminated**************')