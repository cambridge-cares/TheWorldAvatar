##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 29 June 2022         #
##########################################

"""This module is designed to generate and update the A-box of UK power grid model_ELine."""

import os
import owlready2
from rdflib.extras.infixowl import OWL_NS
from rdflib import Graph, URIRef, Literal, ConjunctiveGraph
from rdflib.namespace import RDF, RDFS, XSD
from rdflib.plugins.sleepycat import Sleepycat
from rdflib.store import NO_STORE, VALID_STORE
import sys
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import UKDigitalTwin as UKDT
from UK_Digital_Twin_Package import UKDigitalTwinTBox as T_BOX
from UK_Digital_Twin_Package import UKPowerGridModel as UK_PG
from UK_Digital_Twin_Package import UKPowerPlant as UKpp
from UK_Digital_Twin_Package import UKPowerGridTopology as UK_Topo
from UK_Digital_Twin_Package import TopologicalInformationProperty as TopoInfo
from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs, selectStoragePath, readFile, specifyValidFilePath
import UK_Power_Grid_Model_Generator.SPARQLQueryUsedInModel as query_model
from UK_Power_Grid_Model_Generator.AddModelVariables import AddModelVariable
from UK_Power_Grid_Topology_Generator.topologyABoxGeneration import createTopologicalInformationPropertyInstance
from UK_Digital_Twin_Package.GraphStore import LocalGraphStore
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel as endpointList
from UK_Digital_Twin_Package import BranchPropertyInitialisation as BPI

from UK_Digital_Twin_Package.derivationInterface import createMarkUpDerivation
from pyderivationagent.kg_operations.sparql_client import PySparqlClient # the import of this agent will need a parckage name werkzeug, install `pip install Werkzeug==2.0.2`, otherwise it will report the error message
import uuid
from py4jps.resources import JpsBaseLib


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

"""Blazegraph UK digital tiwn"""
endpoint_label = endpointList.ukdigitaltwin['label']
endpoint_iri = endpointList.ukdigitaltwin['queryendpoint_iri']

"""T-Box URI"""
ontocape_upper_level_system     = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
ontocape_derived_SI_units       = owlready2.get_ontology(t_box.ontocape_derived_SI_units).load()
ontocape_mathematical_model     = owlready2.get_ontology(t_box.ontocape_mathematical_model).load()
ontopowsys_PowerSystemModel     = owlready2.get_ontology(t_box.ontopowsys_PowerSystemModel).load()
ontoecape_space_and_time_extended = owlready2.get_ontology(t_box.ontoecape_space_and_time_extended).load()

"""User specified folder path"""
filepath = None
userSpecified = False

### Functions ###
"""Main function: create the named graph Model_EBus and their sub graphs each ELine"""
def BranchModelKGInstanceCreator(ObjectSet, BranchObjectNameList, numOfBus, topologyNodeIRI, powerSystemModelIRI, powerSystemNodetimeStamp, \
    AgentIRI, derivationClient, updateEndpointIRI, OWLFileStoragePath, updateLocalOWLFile = True, storeType = "default"): 
    store = LocalGraphStore(storeType)
    print('################START BranchModelKGInstanceCreator#################')
    ontologyIRI = dt.baseURL + SLASH + dt.topNode + SLASH + str(uuid.uuid4())
    namespace = UK_PG.ontopowsys_namespace  
    ## ElectricalBusModel node IRI 
    ElectricalELineModelIRI = namespace + UK_PG.UKElineModel.ModelELineKey + str(uuid.uuid4()) # root node
    ## create a named graph
    g = Graph(store = store, identifier = URIRef(ontologyIRI))
    ## Import T-boxes
    g.set((g.identifier, RDF.type, OWL_NS['Ontology']))
    g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_mathematical_model)))
    g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_upper_level_system)))  
    g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontopowsys_PowerSystemModel))) 
    g.set((g.identifier, RDFS.comment, Literal('This ontology represents mathematical model of the electricity branch of the UK energy system.'))) 
    ## Link topologyNodeIRI with PowerSystemModel and ElectricalBusModelIRI
    g.add((URIRef(powerSystemModelIRI), URIRef(ontopowsys_PowerSystemModel.hasModelingPrinciple.iri), URIRef(topologyNodeIRI)))
    g.add((URIRef(powerSystemModelIRI), URIRef(ontoecape_space_and_time_extended.hasTimestamp.iri), Literal(powerSystemNodetimeStamp, \
            datatype = XSD.dateTimeStamp)))
    g.add((URIRef(ElectricalELineModelIRI), URIRef(ontocape_upper_level_system.isExclusivelySubsystemOf.iri), URIRef(powerSystemModelIRI)))
    g.add((URIRef(ElectricalELineModelIRI), RDF.type, URIRef(t_box.ontopowsys_PowerSystemModel + 'ElectricalBranchModel')))

    containerOfModelVariableIRIList = []
    containerOfAgentIRIList = []
    containerOfELineNodeIRI = []

    for elineName in BranchObjectNameList:         
        eline = ObjectSet[elineName]
        ELineNodeIRI = eline.BranchNodeIRI # topology branch node IRI

        ## link the ElectricalELineModelIRI with topology ELineNodeIRI
        g.add((URIRef(ELineNodeIRI), URIRef(ontocape_upper_level_system.isModeledBy.iri), URIRef(ElectricalELineModelIRI)))
        
        ModelVariableIRIList = []
        ## Add Model input Variable to Eline entity
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, eline.FROMBUSKey, int(eline.FROMBUS), None, ontopowsys_PowerSystemModel.BusFrom.iri, True)
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, eline.TOBUSKey, int(eline.TOBUS), None, ontopowsys_PowerSystemModel.BusTo.iri, True)  
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, eline.R_Key, float(eline.R), ontocape_derived_SI_units.ohm.iri, ontopowsys_PowerSystemModel.R.iri, True)     
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, eline.X_Key, float(eline.X), ontocape_derived_SI_units.ohm.iri, ontopowsys_PowerSystemModel.X.iri, True) 
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, eline.B_Key, float(eline.B), (t_box.ontocape_derived_SI_units + 'siemens'), ontopowsys_PowerSystemModel.B.iri, True)    
        ModelVariableIRIList.append(varNode)

        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, eline.RateAKey, float(eline.RateA), (t_box.ontocape_derived_SI_units + 'MVA'), ontopowsys_PowerSystemModel.RateA.iri, True)       
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, eline.RateBKey, float(eline.RateB), (t_box.ontocape_derived_SI_units + 'MVA'), ontopowsys_PowerSystemModel.RateB.iri, True)
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, eline.RateCKey, float(eline.RateC), (t_box.ontocape_derived_SI_units + 'MVA'), ontopowsys_PowerSystemModel.RateC.iri, True)
        ModelVariableIRIList.append(varNode)
       
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, eline.RATIOKey, float(eline.RATIO), None, ontopowsys_PowerSystemModel.RatioCoefficient.iri, True)
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, eline.ANGLEKey, float(eline.ANGLE), ontocape_derived_SI_units.degree.iri, ontopowsys_PowerSystemModel.Angle.iri, True)    
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, eline.STATUSKey, int(eline.STATUS), None, ontopowsys_PowerSystemModel.BranchStatus.iri, True)    
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, eline.ANGMINKey, float(eline.ANGMIN), ontocape_derived_SI_units.degree.iri, ontopowsys_PowerSystemModel.AngleMin.iri, True)   
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, eline.ANGMAXKey, float(eline.ANGMAX), ontocape_derived_SI_units.degree.iri, ontopowsys_PowerSystemModel.AngleMax.iri, True) 
        ModelVariableIRIList.append(varNode)            
        
        ## Add Model output Variable to Eline entity
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, eline.LOSS_P_Key, float(eline.LOSS_P), ontocape_derived_SI_units.MW.iri, ontopowsys_PowerSystemModel.PLoss.iri, False)
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, eline.LOSS_Q_Key, float(eline.LOSS_Q), ontocape_derived_SI_units.Mvar.iri, ontopowsys_PowerSystemModel.QLoss.iri, False)  
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, eline.FROMBUSINJECTION_P_Key, float(eline.FROMBUSINJECTION_P), ontocape_derived_SI_units.MW.iri, (t_box.ontopowsys_PowerSystemModel + 'FromBusInjectionP'), False)     
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, eline.FROMBUSINJECTION_Q_Key, float(eline.FROMBUSINJECTION_Q),  ontocape_derived_SI_units.Mvar.iri, (t_box.ontopowsys_PowerSystemModel + 'FromBusInjectionQ'), False) 
        ModelVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, eline.TOBUSINJECTION_P_Key, float(eline.TOBUSINJECTION_P), ontocape_derived_SI_units.MW.iri, (t_box.ontopowsys_PowerSystemModel + 'ToBusInjectionP'), False)    
        ModelVariableIRIList.append(varNode)

        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, eline.TOBUSINJECTION_Q_Key, float(eline.TOBUSINJECTION_Q),  ontocape_derived_SI_units.Mvar.iri, (t_box.ontopowsys_PowerSystemModel + 'ToBusInjectionQ'), False)       
        ModelVariableIRIList.append(varNode)         
        
        # print(g.serialize(format="pretty-xml").decode("utf-8"))

        containerOfModelVariableIRIList.append(ModelVariableIRIList)
        containerOfAgentIRIList.append(AgentIRI)
        containerOfELineNodeIRI.append([ELineNodeIRI])

    ## add derviation 
    ## derivationClient.createAsyncDerivation(list(ModelVariableIRIList), AgentIRI, [ELineNodeIRI], False)
    derivationClient.bulkCreateAsyncDerivations(containerOfModelVariableIRIList, containerOfAgentIRIList, containerOfELineNodeIRI, [False for iri in containerOfAgentIRIList])

    # generate/update OWL files
    if updateLocalOWLFile == True:    
        # Store/update the generated owl files 
        defaultStoredPath = ObjectSet[BranchObjectNameList[0]].StoreGeneratedOWLs
        filepath = specifyValidFilePath(defaultStoredPath, OWLFileStoragePath, updateLocalOWLFile)     
        if filepath[-1:] != "\\": 
            filepath_ = filepath + '\\' + 'BranchModel_' + str(numOfBus) + '_Bus_Grid'  + TTL
        else:
            filepath_ = filepath + 'BranchModel_' + str(numOfBus) + '_Bus_Grid'  + TTL
        storeGeneratedOWLs(g, filepath_)
    
    sparql_client = PySparqlClient(updateEndpointIRI, updateEndpointIRI)   #(endpoint_iri, endpoint_iri)
    sparql_client.uploadOntology(filepath_)

    return

if __name__ == '__main__':    
    jpsBaseLibGW = JpsBaseLib()
    jpsBaseLibGW.launchGateway()

    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")
    jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.derivation.*")

    endPointURL = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test1/sparql"
    storeClient = jpsBaseLib_view.RemoteStoreClient(endPointURL, endPointURL)

    topologyNodeIRI_10Bus = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerGridTopology_b22aaffa-fd51-4643-98a3-ff72ee04e21e" 
    topologyNodeIRI_29Bus = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerGridTopology_6017554a-98bb-4896-bc21-e455cb6b3958" 
    powerSystemModelIRI_10bus = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerSystemModel_22fe8504-f3bb-403c-9363-34b258d59712"
    powerSystemModelIRI_29bus = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerSystemModel_19e259a0-7f23-4e42-aed9-bcc66d001cec"
    AgentIRI = "http://www.example.com/triplestore/agents/Service__XXXAgent#Service"

    ## set up the derivationInstanceBaseURL
    derivationInstanceBaseURL = dt.baseURL + '/' + dt.topNode + '/'
    ## initialise the derivationClient
    derivationClient = jpsBaseLib_view.DerivationClient(storeClient, derivationInstanceBaseURL)

    OrderedBusNodeIRIList= ['http://www.theworldavatar.com/kb/ontopowsys/BusNode_d6046ef2-6909-4f20-808f-cd9aa01c8ae5', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_ebace1f4-7d3a-44f6-980e-a4b844de670b', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_1f3c4462-3472-4949-bffb-eae7d3135591', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_f17335d2-53f6-4044-9d09-c3d9438c0950', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_c4d7dcca-a7f5-4887-a460-31706ab7ec9c', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_024c0566-d9f0-497d-955e-f7f4e55d4296', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_2d76797b-c638-460e-b73c-769e29785466', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_55285d5a-1d0e-4b1f-8713-246d601671e5', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_6202e767-3077-4910-9cb7-a888e80af788', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_84f6905c-d4cb-409f-861f-ea66fe25ddd0']

    BranchModelKGInstanceCreator(10, topologyNodeIRI_10Bus, powerSystemModelIRI_10bus, "2022-06-15T16:24:29.371941+00:00", AgentIRI, OrderedBusNodeIRIList, derivationClient, endPointURL, "defaultBranchInitialiser", ' ', None, True, "default")
    # createModel_ELine(10, topologyNodeIRI_29Bus, powerSystemModelIRI_29bus, AgentIRI, OrderedBusNodeIRIList, derivationClient, "preSpecifiedBranchInitialiser", None, True, "default")
    print('***********************Terminated***********************')