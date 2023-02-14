##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 04 May 2022          #
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

"""Create an instance of Class UKDigitalTwin"""
dt = UKDT.UKDigitalTwin()

"""Create an object of Class UKDigitalTwinTBox"""
t_box = T_BOX.UKDigitalTwinTBox()

"""Create an object of Class UKPowerPlantDataProperty"""
ukpp = UKpp.UKPowerPlant()

"""Blazegraph UK digital tiwn"""
endpoint_label = endpointList.ukdigitaltwin['label']
endpoint_iri = endpointList.ukdigitaltwin['queryendpoint_iri']

"""Sleepycat storage path"""
userSpecifiePath_Sleepycat = None # user specified path
userSpecified_Sleepycat = False # storage mode: False: default, True: user specified

"""T-Box URI"""
ontocape_upper_level_system     = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
ontocape_derived_SI_units       = owlready2.get_ontology(t_box.ontocape_derived_SI_units).load()
ontocape_mathematical_model     = owlready2.get_ontology(t_box.ontocape_mathematical_model).load()
ontopowsys_PowerSystemModel     = owlready2.get_ontology(t_box.ontopowsys_PowerSystemModel).load()
ontoecape_space_and_time_extended = owlready2.get_ontology(t_box.ontoecape_space_and_time_extended).load()

"""User specified folder path"""
filepath = None
userSpecified = False

"""EBus Conjunctive graph identifier"""
model_ELine_cg_id = "http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_ELine"

### Functions ###
"""Main function: create the named graph Model_EBus and their sub graphs each ELine"""
def createModel_ELine(numOfBus, topologyNodeIRI, powerSystemModelIRI, powerSystemNodetimeStamp, AgentIRI, OrderedBusNodeIRIList, derivationClient, updateEndpointIRI,\
    initialiserMethod, splitCharacter, OWLFileStoragePath, updateLocalOWLFile = True, storeType = "default"): 
    ## Query the eline topological information and geometry information, the return is a dictionary 
    ## Query returns the ELineNode, From_Bus, To_Bus, Value_Length_ELine; if the branch is composed by OHL of different voltage levels, the query will also return the number of the OHL
    ELineTopoAndGeometryInfo, branchVoltageLevel = query_model.queryELineTopologicalInformation(topologyNodeIRI, endpoint_label)
    if len(ELineTopoAndGeometryInfo) == 0:
        raise Exception('ELineTopoAndGeometryInfo is empty, please check the return from queryELineTopologicalInformation.')

    ## TODO: the UKElineModel initialisation function has changed 
    uk_eline_model = UK_PG.UKElineModel(numOfBus, initialiserMethod)
    # uk_topo = UK_Topo.UKPowerGridTopology(numOfBus)

    ## set up the storage path and Sleepycat
    defaultPath_Sleepycat = uk_eline_model.SleepycatStoragePath
    # topoAndConsumpPath_Sleepycat = uk_topo.SleepycatStoragePath
    defaultStoredPath = uk_eline_model.StoreGeneratedOWLs
    filepath = specifyValidFilePath(defaultStoredPath, OWLFileStoragePath, updateLocalOWLFile)
    if filepath == None:
        return    
    store = LocalGraphStore(storeType)
    # topo_info, busInfoArrays, branchTopoInfoArrays, branchPropertyArrays = createTopologicalInformationPropertyInstance(numOfBus, numOfBranch)
    global userSpecifiePath_Sleepycat, userSpecified_Sleepycat 
    ## create conjunctive graph storing the generated graphs in a specified Sleepycat on-disc graph store
    if isinstance(store, Sleepycat): 
        print('The store is Sleepycat')
        cg_model_ELine = ConjunctiveGraph(store=store, identifier = model_ELine_cg_id)
        if userSpecifiePath_Sleepycat == None and userSpecified_Sleepycat:
            print('****Needs user to specify a Sleepycat storage path****')
            userSpecifiePath_Sleepycat = selectStoragePath()
            userSpecifiePath_Sleepycat_ = userSpecifiePath_Sleepycat + '\\' + 'ConjunctiveGraph_UKPowerGrid_ELine'
            sl = cg_model_ELine.open(userSpecifiePath_Sleepycat_, create = False) 
            
        elif os.path.exists(defaultPath_Sleepycat) and not userSpecified_Sleepycat:
            print('****Non user specified Sleepycat storage path, will use the default storage path****')
            sl = cg_model_ELine.open(defaultPath_Sleepycat, create = False)        
        else:
            print('****Create Sleepycat store with its default path****')
            sl = cg_model_ELine.open(defaultPath_Sleepycat, create = True)   
        
        if sl == NO_STORE:
        # There is no underlying Sleepycat infrastructure, so create it
            cg_model_ELine.open(defaultPath_Sleepycat, create = True)
        else:
            assert sl == VALID_STORE, "The underlying sleepycat store is corrupt"
    else:
        print('Store is IOMemery')
     
    print('################START createModel_ELine#################')
    ontologyIRI = dt.baseURL + SLASH + dt.topNode + SLASH + str(uuid.uuid4())
    namespace = UK_PG.ontopowsys_namespace  
    ## ElectricalBusModel node IRI 
    ElectricalELineModelIRI = namespace + uk_eline_model.ModelELineKey + str(uuid.uuid4()) # root node
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
    # g.add((URIRef(powerSystemModelIRI), RDF.type, URIRef(ontopowsys_PowerSystemModel.PowerSystemModel.iri)))
    
    ret_branch_array = []
    for eline in ELineTopoAndGeometryInfo:         
    # if ELineTopoAndGeometryInfo[0] != None: # test
    #     eline = ELineTopoAndGeometryInfo[0] # test

        ELineNodeIRI = eline['ELineNode']
        ## link the ElectricalELineModelIRI with topology ELineNodeIRI
        g.add((URIRef(ELineNodeIRI), URIRef(ontocape_upper_level_system.isModeledBy.iri), URIRef(ElectricalELineModelIRI)))
        ## specify the initialisation method for each branch instance of branch model
        ###1. create an instance of the BranchPropertyInitialisation class and get the initialiser method by applying the 'getattr' function 
        initialisation = BPI.BranchPropertyInitialisation()
        initialiser = getattr(initialisation, initialiserMethod)
        ###2. execute the initialiser with the branch model instance as the function argument 
        # ## TODO: when initialise the 29-bus model, please check if ELineNodeIRI is the right node to use
        uk_eline_model = initialiser(ELineNodeIRI, uk_eline_model, eline, branchVoltageLevel, OrderedBusNodeIRIList, endpoint_label) 

        ret_branch_array.append(str(int(uk_eline_model.FROMBUS)) + splitCharacter + str(int(uk_eline_model.TOBUS)) + splitCharacter + str(float(uk_eline_model.R)) + splitCharacter + str(float(uk_eline_model.X)) + splitCharacter + \
            str(float(uk_eline_model.B)) + splitCharacter + str(float(uk_eline_model.RateA)) + splitCharacter + str(float(uk_eline_model.RateB)) + splitCharacter + str(float(uk_eline_model.RateC)) + splitCharacter + str(float(uk_eline_model.RATIO)) + splitCharacter + \
            str(float(uk_eline_model.ANGLE)) + splitCharacter + str(int(uk_eline_model.STATUS)) + splitCharacter + str(float(uk_eline_model.ANGMIN)) + splitCharacter + str(float(uk_eline_model.ANGMAX)))
  
        ModelInputVariableIRIList = []
        # AddModelVariable to Eline entity
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, uk_eline_model.FROMBUSKey, int(uk_eline_model.FROMBUS), None, ontopowsys_PowerSystemModel.BusFrom.iri)
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, uk_eline_model.TOBUSKey, int(uk_eline_model.TOBUS), None, ontopowsys_PowerSystemModel.BusTo.iri)  
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, uk_eline_model.R_Key, float(uk_eline_model.R), ontocape_derived_SI_units.ohm.iri, ontopowsys_PowerSystemModel.R.iri)     
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, uk_eline_model.X_Key, float(uk_eline_model.X), ontocape_derived_SI_units.ohm.iri, ontopowsys_PowerSystemModel.X.iri) 
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, uk_eline_model.B_Key, float(uk_eline_model.B), (t_box.ontocape_derived_SI_units + 'siemens'), ontopowsys_PowerSystemModel.B.iri)    
        ModelInputVariableIRIList.append(varNode)

        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, uk_eline_model.RateAKey, float(uk_eline_model.RateA), (t_box.ontocape_derived_SI_units + 'MVA'), ontopowsys_PowerSystemModel.RateA.iri)       
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, uk_eline_model.RateBKey, float(uk_eline_model.RateB), (t_box.ontocape_derived_SI_units + 'MVA'), ontopowsys_PowerSystemModel.RateB.iri)
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, uk_eline_model.RateCKey, float(uk_eline_model.RateC), (t_box.ontocape_derived_SI_units + 'MVA'), ontopowsys_PowerSystemModel.RateC.iri)
        ModelInputVariableIRIList.append(varNode)
       
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, uk_eline_model.RATIOKey, float(uk_eline_model.RATIO), None, ontopowsys_PowerSystemModel.RatioCoefficient.iri)
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, uk_eline_model.ANGLEKey, float(uk_eline_model.ANGLE), ontocape_derived_SI_units.degree.iri, ontopowsys_PowerSystemModel.Angle.iri)    
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, uk_eline_model.STATUSKey, int(uk_eline_model.STATUS), None, ontopowsys_PowerSystemModel.BranchStatus.iri)    
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, uk_eline_model.ANGMINKey, float(uk_eline_model.ANGMIN), ontocape_derived_SI_units.degree.iri, ontopowsys_PowerSystemModel.AngleMin.iri)   
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalELineModelIRI, namespace, uk_eline_model.ANGMAXKey, float(uk_eline_model.ANGMAX), ontocape_derived_SI_units.degree.iri, ontopowsys_PowerSystemModel.AngleMax.iri) 
        ModelInputVariableIRIList.append(varNode)            
        
        # print(g.serialize(format="pretty-xml").decode("utf-8"))

        ## add derviation 
        ## TODO: disable derivationClient
        #derivationClient.createAsyncDerivation(list(ModelInputVariableIRIList), AgentIRI, [ELineNodeIRI], False)

    

    # generate/update OWL files
    if updateLocalOWLFile == True:    
        # Store/update the generated owl files      
        if filepath[-2:] != "\\": 
            filepath_ = filepath + '\\' + 'BranchModel_' + str(numOfBus) + '_Bus_Grid'  + OWL
        else:
            filepath_ = filepath + 'BranchModel_' + str(numOfBus) + '_Bus_Grid'  + OWL
        storeGeneratedOWLs(g, filepath_)
        print(filepath_)
    ## TODO: disable sparql_client
    #sparql_client = PySparqlClient(updateEndpointIRI, updateEndpointIRI)   #(endpoint_iri, endpoint_iri)
    #sparql_client.uploadOntology(filepath_)

    print("...creating loacl file...")
    textfile = open("C:\\Users\\wx243\\Documents\\TheWorldAvatar\\UK_Digital_Twin\\testOPFAnalysis\\branch.txt", "w")
    for r in ret_branch_array:
        textfile.write(r + "\n")
    textfile.close()

    print("################FINISH createModel_ELine#################")

    if isinstance(store, Sleepycat):  
        cg_model_ELine.close()       
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

    createModel_ELine(10, topologyNodeIRI_10Bus, powerSystemModelIRI_10bus, "2022-06-15T16:24:29.371941+00:00", AgentIRI, OrderedBusNodeIRIList, derivationClient, endPointURL, "defaultBranchInitialiser", ' ', None, True, "default")
    # createModel_ELine(10, topologyNodeIRI_29Bus, powerSystemModelIRI_29bus, AgentIRI, OrderedBusNodeIRIList, derivationClient, "preSpecifiedBranchInitialiser", None, True, "default")
    print('***********************Terminated***********************')