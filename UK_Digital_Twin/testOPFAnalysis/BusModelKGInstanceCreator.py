##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 29 June 2022         #
##########################################a

"""This module is designed to generate and update the A-box of UK power grid model_EBus."""

import os
import owlready2
from rdflib.extras.infixowl import OWL_NS
from rdflib import Graph, URIRef, Literal
from rdflib.namespace import RDF, RDFS, XSD
import sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from UK_Digital_Twin_Package import UKDigitalTwinTBox as T_BOX
from UK_Digital_Twin_Package import UKPowerGridModel as UK_PG
from UK_Digital_Twin_Package import UKPowerPlant as UKpp
from UK_Digital_Twin_Package import UKEnergyConsumption as UKec
from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs, specifyValidFilePath
from UK_Power_Grid_Model_Generator.AddModelVariables import AddModelVariable
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel as endpointList
from UK_Digital_Twin_Package.GraphStore import LocalGraphStore
from UK_Digital_Twin_Package import demandLoadAllocator as DLA
# from UK_Digital_Twin_Package.derivationInterface import createMarkUpDerivation
from pyderivationagent.kg_operations.sparql_client import PySparqlClient # the import of this agent will need a parckage name werkzeug, install `pip install Werkzeug==2.0.2`, otherwise it will report the error message
import uuid
from py4jps.resources import JpsBaseLib
import urllib.request

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

"""Create an object of Class UKPowerPlantDataProperty"""
ukpp = UKpp.UKPowerPlant()

"""Create an object of Class UKEnergyConsumption"""
ukec = UKec.UKEnergyConsumption()

## TODO: check if the endpoint is the right endpoint
"""Blazegraph UK digital tiwn"""
endpoint_label = endpointList.ukdigitaltwin['label'] # remote query
endpoint_iri = endpointList.ukdigitaltwin['queryendpoint_iri'] # federated query
ONS_JSON =  endpointList.ONS['endpoint_iri']

"""T-Box URI"""
if urllib.request.urlopen("http://www.theworldavatar.com/ontology/").getcode() == 200:
    ontocape_upper_level_system     = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
    ontocape_derived_SI_units       = owlready2.get_ontology(t_box.ontocape_derived_SI_units).load()
    ontocape_mathematical_model     = owlready2.get_ontology(t_box.ontocape_mathematical_model).load()
    ontopowsys_PowerSystemModel     = owlready2.get_ontology(t_box.ontopowsys_PowerSystemModel).load()
    ontoecape_space_and_time_extended = owlready2.get_ontology(t_box.ontoecape_space_and_time_extended).load()
else:
    print('---THE WORLD AVATAR NOT FOUND---')

"""User specified folder path"""
filepath = None
userSpecified = False

### Functions ### 
"""Main function: create the named graph Model_EBus and their sub graphs each EBus"""

def BusModelKGInstanceCreator(ObjectSet, BusObjectNameList, numOfBus, topologyNodeIRI, powerSystemModelIRI, powerSystemNodetimeStamp, \
    AgentIRI, derivationClient, updateEndpointIRI, OWLFileStoragePath, updateLocalOWLFile = True, storeType = "default"):
    """T-Box URI"""
    if urllib.request.urlopen("http://www.theworldavatar.com/ontology/").getcode() == 200:
        ontocape_upper_level_system     = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
        ontocape_derived_SI_units       = owlready2.get_ontology(t_box.ontocape_derived_SI_units).load()
        ontocape_mathematical_model     = owlready2.get_ontology(t_box.ontocape_mathematical_model).load()
        ontopowsys_PowerSystemModel     = owlready2.get_ontology(t_box.ontopowsys_PowerSystemModel).load()
        ontoecape_space_and_time_extended = owlready2.get_ontology(t_box.ontoecape_space_and_time_extended).load()
    else:
        print('---THE WORLD AVATAR NOT FOUND---')
    ## Set up the default storage path
    store = LocalGraphStore(storeType)
    print('################START BusModelKGInstanceCreator#################')
    ontologyIRI = dt.baseURL + SLASH + dt.topNode + SLASH + str(uuid.uuid4())
    namespace = UK_PG.ontopowsys_namespace  
    ## ElectricalBusModel node IRI 
    ElectricalBusModelIRI = namespace + UK_PG.UKEbusModel.ModelEBusKey + str(uuid.uuid4()) # root node
    ## create a named graph
    g = Graph(store = store, identifier = URIRef(ontologyIRI))
    ## Import T-boxes
    g.set((g.identifier, RDF.type, OWL_NS['Ontology']))
    g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_mathematical_model)))
    g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_upper_level_system)))  
    g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontopowsys_PowerSystemModel))) 
    g.set((g.identifier, RDFS.comment, Literal('This ontology represents mathematical model of the electricity bus of the UK energy system.'))) 
    ## Link topologyNodeIRI with PowerSystemModel and ElectricalBusModelIRI
    g.add((URIRef(powerSystemModelIRI), URIRef(ontopowsys_PowerSystemModel.hasModelingPrinciple.iri), URIRef(topologyNodeIRI)))
    g.add((URIRef(powerSystemModelIRI), URIRef(ontoecape_space_and_time_extended.hasTimestamp.iri), Literal(powerSystemNodetimeStamp, \
    datatype = XSD.dateTimeStamp)))
    g.add((URIRef(ElectricalBusModelIRI), URIRef(ontocape_upper_level_system.isExclusivelySubsystemOf.iri), URIRef(powerSystemModelIRI)))

    containerOfModelVariableIRIList = []
    containerOfAgentIRIList = []
    containerOfBusNodeIRIList = []

    for busName in BusObjectNameList:          
        ebus = ObjectSet[busName]
        BusNodeIRI = ebus.BusNodeIRI # topology bus node IRI
        
        ## link the ElectricalBusModelIRI with topology BusNodeIRI
        g.add((URIRef(BusNodeIRI), URIRef(ontocape_upper_level_system.isModeledBy.iri), URIRef(ElectricalBusModelIRI)))
        
        ## Add model input variables attributes 
        ModelVariableIRIList = [] 
                
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, ebus.BUSNUMKey, int(ebus.BUS), None, ontopowsys_PowerSystemModel.BusNumber.iri, True)
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, ebus.BUSTYPEKey, int(ebus.TYPE), None, ontopowsys_PowerSystemModel.BusType.iri, True)
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, ebus.PD_INPUTKey, float(ebus.PD_INPUT), ontocape_derived_SI_units.MW.iri, ontopowsys_PowerSystemModel.PdBus.iri, True)    
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, ebus.GD_INPUTKey, float(ebus.GD_INPUT), ontocape_derived_SI_units.Mvar.iri, ontopowsys_PowerSystemModel.GdBus.iri, True) 
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, ebus.GSKey, int(ebus.GS), None, ontopowsys_PowerSystemModel.Gs.iri, True)
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, ebus.BSKey, int(ebus.BS), None, ontopowsys_PowerSystemModel.Bs.iri, True)   
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, ebus.AREAKey, int(ebus.AREA), None, ontopowsys_PowerSystemModel.Area.iri, True)
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, ebus.VM_INPUTKey, float(ebus.VM_INPUT), ontocape_derived_SI_units.kV.iri, ontopowsys_PowerSystemModel.Vm.iri, True) 
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, ebus.VA_INPUTKey, float(ebus.VA_INPUT), ontocape_derived_SI_units.degree.iri, ontopowsys_PowerSystemModel.Va.iri, True)  
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, ebus.BASEKVKey, int(ebus.BASEKV), ontocape_derived_SI_units.kV.iri, ontopowsys_PowerSystemModel.baseKV.iri, True)    
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, ebus.ZONEKey, int(ebus.ZONE), None, ontopowsys_PowerSystemModel.Zone.iri, True)
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, ebus.VMAXKey, float(ebus.VMAX), ontocape_derived_SI_units.kV.iri, ontopowsys_PowerSystemModel.VmMax.iri, True) 
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, ebus.VMINKey, float(ebus.VMIN), ontocape_derived_SI_units.kV.iri, ontopowsys_PowerSystemModel.VmMin.iri, True)
        ModelVariableIRIList.append(varNode)

        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, ebus.VMINKey, float(ebus.VMIN), ontocape_derived_SI_units.kV.iri, ontopowsys_PowerSystemModel.VmMin.iri, True)
        ModelVariableIRIList.append(varNode)
        
        ## Add model output variables attributes 
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, ebus.VM_OUTPUTKey, float(ebus.VM_OUTPUT), ontocape_derived_SI_units.kV.iri, ontopowsys_PowerSystemModel.Vm.iri, False)
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, ebus.VA_OUTPUTKey, float(ebus.VA_OUTPUT), ontocape_derived_SI_units.degree.iri, ontopowsys_PowerSystemModel.Va.iri, False)
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, ebus.PG_OUTPUTKey, float(ebus.P_GEN), ontocape_derived_SI_units.MW.iri, ontopowsys_PowerSystemModel.PdGen.iri, False)    
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, ebus.GG_OUTPUTKey, float(ebus.G_GEN), ontocape_derived_SI_units.Mvar.iri, ontopowsys_PowerSystemModel.GdGen.iri, False) 
        ModelVariableIRIList.append(varNode)

        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, ebus.PD_OUTPUTKey, float(ebus.PD_OUTPUT), ontocape_derived_SI_units.MW.iri, ontopowsys_PowerSystemModel.PdBus.iri, False)    
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalBusModelIRI, namespace, ebus.GD_OUTPUTKey, float(ebus.GD_OUTPUT), ontocape_derived_SI_units.Mvar.iri, ontopowsys_PowerSystemModel.GdBus.iri, False) 
        ModelVariableIRIList.append(varNode)
        
        # print(g.serialize(format="pretty-xml").decode("utf-8"))

        containerOfModelVariableIRIList.append(ModelVariableIRIList)
        containerOfAgentIRIList.append(AgentIRI)
        containerOfBusNodeIRIList.append([BusNodeIRI])
        
    ## add derviation 
    ## derivationClient.createAsyncDerivation(list(ModelVariableIRIList), AgentIRI, [BusNodeIRI], False)
    derivationClient.bulkCreateAsyncDerivations(containerOfModelVariableIRIList, containerOfAgentIRIList, containerOfBusNodeIRIList, [False for iri in containerOfAgentIRIList])

    ## generate/update OWL files
    
    if updateLocalOWLFile == True:  
        defaultStoredPath = ObjectSet[BusObjectNameList[0]].StoreGeneratedOWLs
        filepath = specifyValidFilePath(defaultStoredPath, OWLFileStoragePath, updateLocalOWLFile)
        # Store/update the generated owl files      
        if filepath[-1:] != "\\": 
            filepath_ = filepath + '\\' + 'BusModel_' + str(numOfBus) + '_Bus_Grid' + TTL
        else:
            filepath_ = filepath + 'BusModel_' + str(numOfBus) + '_Bus_Grid' + TTL
        storeGeneratedOWLs(g, filepath_)

        ## update the graph to endpoint
        sparql_client = PySparqlClient(updateEndpointIRI, updateEndpointIRI)
        sparql_client.uploadOntology(filepath_)         
    return 

# The demanding of an AggregatedBus is the sum of their regional consumption (elec demanding)
def addUpConsumptionForAggregatedBus(EBus_Load_List):
    bus_node  = []
    for ebus in EBus_Load_List:
        if ebus['BusNodeIRI'] in bus_node:
            counter_1 = bus_node.index(ebus['BusNodeIRI'])
            counter_2 = EBus_Load_List.index(ebus) 
            if counter_2 > counter_1:
                EBus_Load_List[counter_1]['v_TotalELecConsumption'] += EBus_Load_List[counter_2]['v_TotalELecConsumption']
                EBus_Load_List[counter_1]['v_TotalELecConsumption'] = round(float(EBus_Load_List[counter_1]['v_TotalELecConsumption']), 2)
                del EBus_Load_List[counter_2]
            else:
                print('counter_2 should be larger than counter_1')
                return None
        else:
            bus_node.append(ebus['BusNodeIRI'])
    return EBus_Load_List

if __name__ == '__main__':  
    jpsBaseLibGW = JpsBaseLib()
    jpsBaseLibGW.launchGateway()

    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")
    jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.derivation.*")

    endPointURL = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test1/sparql"
    storeClient = jpsBaseLib_view.RemoteStoreClient(endPointURL, endPointURL)

    topologyNodeIRI_10Bus = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerGridTopology_b22aaffa-fd51-4643-98a3-ff72ee04e21e" 
    powerSystemModelIRI = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerSystemModel_22fe8504-f3bb-403c-9363-34b258d59712"
    AgentIRI = "http://www.example.com/triplestore/agents/Service__XXXAgent#Service"
    slackBusNodeIRI = "http://www.theworldavatar.com/kb/ontopowsys/BusNode_1f3c4462-3472-4949-bffb-eae7d3135591"
    
    # ModelInputVariableIRIList = ['http://www.theworldavatar.com/kb/ontopowsys/BusNumber_4a832bb7-a9f6-40c0-bb49-3a036a382dae', 'http://www.theworldavatar.com/kb/ontopowsys/BusType_bf8f7115-94d2-4b30-98d2-f0932df918f3', 'http://www.theworldavatar.com/kb/ontopowsys/PdBus_c4fa810b-131e-4805-b14a-10674e9f8769', 'http://www.theworldavatar.com/kb/ontopowsys/GdBus_138f59ae-ca18-4f47-bbb6-edf260e8e77c', 'http://www.theworldavatar.com/kb/ontopowsys/Gs_06a10ba8-397a-4693-a981-30a0575a68f8', 'http://www.theworldavatar.com/kb/ontopowsys/Bs_dadf103f-d40a-4782-b8f1-fcc24d0e1a76', 'http://www.theworldavatar.com/kb/ontopowsys/Area_c3c0cde4-9718-4147-888f-b001861d7557', 'http://www.theworldavatar.com/kb/ontopowsys/Vm_351d012b-14f3-4f3b-834b-c0a358863507', 'http://www.theworldavatar.com/kb/ontopowsys/Va_cfe7d283-da92-4dc7-8017-70979b4f1e1c', 'http://www.theworldavatar.com/kb/ontopowsys/baseKV_fc33f334-aa88-4927-9b5f-2b1a30ddd502', 'http://www.theworldavatar.com/kb/ontopowsys/Zone_3720a687-5e53-4ffd-8cfc-1dd2c39c743e', 'http://www.theworldavatar.com/kb/ontopowsys/VmMax_725c736d-eb00-4c0a-aa9f-84f84a631e33', 'http://www.theworldavatar.com/kb/ontopowsys/VmMin_34de8974-29ea-4a8c-99a3-f38a99b0a921']
    # BusNodeIRI = "http://www.theworldavatar.com/kb/ontopowsys/BusNode_9fee5b07-8a08-4a60-93e2-bc118bce3965"

    ## set up the derivationInstanceBaseURL
    derivationInstanceBaseURL = dt.baseURL + '/' + dt.topNode + '/'
    ## initialise the derivationClient
    derivationClient = jpsBaseLib_view.DerivationClient(storeClient, derivationInstanceBaseURL)

    OrderedBusNodeIRIList = BusModelKGInstanceCreator(10, topologyNodeIRI_10Bus, powerSystemModelIRI, "2022-06-15T16:24:29.371941+00:00", AgentIRI, slackBusNodeIRI, derivationClient, endPointURL, "2017-01-31", "regionalDemandLoad", "defaultInitialisation", ' ', None, True, 'default')  
    
    print(OrderedBusNodeIRIList)
    print('*****************Terminated*****************')