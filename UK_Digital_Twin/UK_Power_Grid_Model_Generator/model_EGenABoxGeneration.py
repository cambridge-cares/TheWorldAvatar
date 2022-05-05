##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 04 May 2022          #
##########################################

"""This module is designed to generate and update the A-box of UK power grid model_EGen."""
# run time: around 20 mins

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
from UK_Digital_Twin_Package import UKPowerGridModel as UK_PG
from UK_Digital_Twin_Package import UKPowerPlant as UKpp
from UK_Digital_Twin_Package import UKPowerGridTopology as UK_Topo
from UK_Digital_Twin_Package import UKEnergyConsumption as UKec
from UK_Digital_Twin_Package import CO2FactorAndGenCostFactor as ModelFactor
from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs, selectStoragePath, readFile, specifyValidFilePath
from UK_Power_Grid_Model_Generator.costFunctionParameterInitialiser import costFuncPara
from UK_Power_Grid_Model_Generator.AddModelVariables import AddModelVariable
from UK_Digital_Twin_Package.GraphStore import LocalGraphStore
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel as endpointList

import UK_Power_Grid_Model_Generator.SPARQLQueryUsedInModel as query_model
from UK_Digital_Twin_Package.derivationInterface import createMarkUpDerivation
from pyasyncagent.agent.async_agent import AsyncAgent
from pyasyncagent.kg_operations.sparql_client import PySparqlClient # the import of this agent will need a parckage name werkzeug, install `pip install Werkzeug==2.0.2`, otherwise it will report the error message
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

"""Create an object of Class UKPowerPlant"""
ukpp = UKpp.UKPowerPlant()

"""Create an object of Class CO2FactorAndGenCostFactor"""
ukmf = ModelFactor.ModelFactor()

"""Create an object of Class UKPowerGridModel"""
# uk_egen_model = UK_PG.UKEGenModel()

"""Create an object of Class UKPowerGridTopology"""
uk_topo = UK_Topo.UKPowerGridTopology()

"""Create an object of Class UKEnergyConsumption"""
ukec = UKec.UKEnergyConsumption()

"""Blazegraph UK digital tiwn"""
endpoint_label = endpointList.ukdigitaltwin['lable']
endpoint_iri = endpointList.ukdigitaltwin['queryendpoint_iri']

"""Sleepycat storage path"""
topoAndConsumpPath_Sleepycat = uk_topo.SleepycatStoragePath
powerPlant_Sleepycat = ukpp.SleepycatStoragePath
userSpecifiePath_Sleepycat = None # user specified path
userSpecified_Sleepycat = False # storage mode: False: default, True: user specified

"""OWL file storage path"""
# defaultStoredPath = uk_egen_model.StoreGeneratedOWLs # default path

"""T-Box URI"""
ontocape_upper_level_system     = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
ontocape_derived_SI_units       = owlready2.get_ontology(t_box.ontocape_derived_SI_units).load()
ontocape_mathematical_model     = owlready2.get_ontology(t_box.ontocape_mathematical_model).load()
ontopowsys_PowerSystemModel     = owlready2.get_ontology(t_box.ontopowsys_PowerSystemModel).load()

"""User specified folder path"""
filepath = None
userSpecified = False

"""EGen Conjunctive graph identifier"""
model_EGen_cg_id = "http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EGen"

"""Global variables EGenInfo array and capa_demand_ratio"""
EGenInfo = []
capa_demand_ratio = 0

### Functions ### 
"""Main function: create the named graph Model_EGen and their sub graphs each EGen"""
def createModel_EGen(topologyNodeIRI, powerSystemModelIRI, AgentIRI, OrderedBusNodeIRIList, derivationClient, startTime_of_EnergyConsumption, OPFOrPF, CarbonTax, piecewiseOrPolynomial, pointsOfPiecewiseOrcostFuncOrder, OWLFileStoragePath, updateLocalOWLFile = True, storeType = "default"):
    ## Query generator attributes and the number of the buses
    EGenInfo, numOfBus = list(query_model.queryEGenInfo(topologyNodeIRI, endpoint_label))
    # location = query_model.queryPowerSystemLocation(endpoint_label, topologyNodeIRI)  
    uk_egen_model = UK_PG.UKEGenModel(numOfBus)

    ## Set up the default storage path
    defaultStoredPath = uk_egen_model.StoreGeneratedOWLs
    defaultPath_Sleepycat = uk_egen_model.SleepycatStoragePath
    filepath = specifyValidFilePath(defaultStoredPath, OWLFileStoragePath, updateLocalOWLFile)
    if filepath == None:
        return   
    store = LocalGraphStore(storeType)
    global userSpecifiePath_Sleepycat, userSpecified_Sleepycat, capa_demand_ratio  
    if isinstance(store, Sleepycat):
        print('The store is Sleepycat')
        cg_model_EGen = ConjunctiveGraph(store=store, identifier = model_EGen_cg_id)
        if userSpecifiePath_Sleepycat == None and userSpecified_Sleepycat:
            print('****Needs user to specify a Sleepycat storage path****')
            userSpecifiePath_Sleepycat = selectStoragePath()
            userSpecifiePath_Sleepycat_ = userSpecifiePath_Sleepycat + '\\' + 'ConjunctiveGraph_UKPowerGrid_EGen'
            sl = cg_model_EGen.open(userSpecifiePath_Sleepycat_, create = False) 
            
        elif os.path.exists(defaultPath_Sleepycat) and not userSpecified_Sleepycat:
            print('****Non user specified Sleepycat storage path, will use the default storage path****')
            sl = cg_model_EGen.open(defaultPath_Sleepycat, create = False)        
        else:
            print('****Create Sleepycat store with its default path****')
            sl = cg_model_EGen.open(defaultPath_Sleepycat, create = True)   
        
        if sl == NO_STORE:
        # There is no underlying Sleepycat infrastructure, so create it
            cg_model_EGen.open(defaultPath_Sleepycat, create = True)
        else:
            assert sl == VALID_STORE, "The underlying sleepycat store is corrupt"
    else:
        print('Store is IOMemery')        
   
    capa_demand_ratio = demandAndCapacityRatioCalculator(EGenInfo, topologyNodeIRI, startTime_of_EnergyConsumption)
    
    print('################START createModel_EGen#################')
    ontologyIRI = dt.baseURL + SLASH + dt.topNode + SLASH + str(uuid.uuid4())
    namespace = UK_PG.ontopowsys_namespace  
    ## ElectricalGenerator node IRI 
    ElectricalGeneratorModelIRI = namespace + uk_egen_model.ModelEGenKey + str(uuid.uuid4()) # root node
    ## create a named graph
    g = Graph(store = store, identifier = URIRef(ontologyIRI))
    ## Import T-boxes
    g.set((g.identifier, RDF.type, OWL_NS['Ontology']))
    g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_mathematical_model)))
    g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_upper_level_system)))  
    g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontopowsys_PowerSystemModel))) 
    g.set((g.identifier, RDFS.comment, Literal('This ontology represents mathematical model of the electricity generator of the UK energy system.'))) 
    ## Link topologyNodeIRI with PowerSystemModel and ElectricalGeneratorModelIRI
    g.add((URIRef(powerSystemModelIRI), URIRef(ontopowsys_PowerSystemModel.hasModelingPrinciple.iri), URIRef(topologyNodeIRI)))
    g.add((URIRef(ElectricalGeneratorModelIRI), URIRef(ontocape_upper_level_system.isExclusivelySubsystemOf.iri), URIRef(powerSystemModelIRI)))
    g.add((URIRef(ElectricalGeneratorModelIRI), RDF.type, URIRef(t_box.ontopowsys_PowerSystemModel + 'ElectricalGeneratorModel')))
    g.add((URIRef(powerSystemModelIRI), RDF.type, URIRef(ontopowsys_PowerSystemModel.PowerSystemModel.iri)))
    counter = 0  
    for egen in EGenInfo: 
        if counter > 500:
            break
         # if EGenInfo[0] != None: # test
            # egen = EGenInfo[0] # test
        # ## create a named graph 
        # g = Graph(store = store, identifier = URIRef(ontologyIRI))
        # ## Import T-boxes
        # g.set((g.identifier, RDF.type, OWL_NS['Ontology']))
        # g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_mathematical_model)))
        # g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_upper_level_system)))  
        # g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontopowsys_PowerSystemModel))) 
        # g.set((g.identifier, RDFS.comment, Literal('This ontology represents mathematical model of the electricity generator of the UK energy system.'))) 
        # ## Link topologyNodeIRI with PowerSystemModel and ElectricalGeneratorModelIRI
        # g.add((URIRef(powerSystemModelIRI), URIRef(ontopowsys_PowerSystemModel.hasModelingPrinciple.iri), URIRef(topologyNodeIRI)))
        # g.add((URIRef(ElectricalGeneratorModelIRI), URIRef(ontocape_upper_level_system.isExclusivelySubsystemOf.iri), URIRef(powerSystemModelIRI)))
        # g.add((URIRef(ElectricalGeneratorModelIRI), RDF.type, URIRef(t_box.ontopowsys_PowerSystemModel + 'ElectricalGeneratorModel')))
        # g.add((URIRef(powerSystemModelIRI), RDF.type, URIRef(ontopowsys_PowerSystemModel.PowerSystemModel.iri)))       
   
        generatorNodeIRI = egen[0]
        ## link the ElectricalGeneratorModelIRI with topology generatorNodeIRI
        g.add((URIRef(generatorNodeIRI), URIRef(ontocape_upper_level_system.isModeledBy.iri), URIRef(ElectricalGeneratorModelIRI)))
        
        ModelInputVariableIRIList = []
        ###add cost function parameters###
        if OPFOrPF is True: # when OPFOrPF is true, the model is OPF analysis
            # calculate a, b, c
            uk_egen_costFunc = UK_PG.UKEGenModel_CostFunc(CarbonTax, piecewiseOrPolynomial, pointsOfPiecewiseOrcostFuncOrder) # declear an instance of the UKEGenModel_CostFunc
            uk_egen_costFunc = costFuncPara(uk_egen_costFunc, egen)
            # assign value to attributes
            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_costFunc.CostFuncFormatKey, uk_egen_costFunc.MODEL, None, ontopowsys_PowerSystemModel.CostModel.iri)
            ModelInputVariableIRIList.append(varNode)

            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_costFunc.StartupCostKey, uk_egen_costFunc.STARTUP, ontocape_derived_SI_units.USD.iri, ontopowsys_PowerSystemModel.StartCost.iri)
            ModelInputVariableIRIList.append(varNode)

            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_costFunc.ShutdownCostKey, uk_egen_costFunc.SHUTDOWN, ontocape_derived_SI_units.USD.iri, ontopowsys_PowerSystemModel.StopCost.iri)
            ModelInputVariableIRIList.append(varNode)

            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_costFunc.genCostnKey, uk_egen_costFunc.NCOST, None, ontopowsys_PowerSystemModel.genCostn.iri)
            ModelInputVariableIRIList.append(varNode)

            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_costFunc.genCost_aKey, uk_egen_costFunc.a, None, t_box.ontopowsys_PowerSystemModel + 'ZeroOrderCoefficient')
            ModelInputVariableIRIList.append(varNode)

            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_costFunc.genCost_bKey, uk_egen_costFunc.b, t_box.ontocape_derived_SI_units + 'GBP/MWh', t_box.ontopowsys_PowerSystemModel + 'FirstOrderCoefficient') # undified unit
            ModelInputVariableIRIList.append(varNode)

            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_costFunc.genCost_cKey, uk_egen_costFunc.c, t_box.ontocape_derived_SI_units + 'GBP/MWh', t_box.ontopowsys_PowerSystemModel + 'SecondOrderCoefficient') # undified unit
            ModelInputVariableIRIList.append(varNode)
            
            g.add((URIRef(t_box.ontopowsys_PowerSystemModel + 'ZeroOrderCoefficient'), RDFS.subClassOf, URIRef(t_box.ontopowsys_PowerSystemModel + 'PolynomialCostFunctionParameter'))) 
            g.add((URIRef(t_box.ontopowsys_PowerSystemModel + 'FirstOrderCoefficient'), RDFS.subClassOf, URIRef(t_box.ontopowsys_PowerSystemModel + 'PolynomialCostFunctionParameter'))) 
            g.add((URIRef(t_box.ontopowsys_PowerSystemModel + 'SecondOrderCoefficient'), RDFS.subClassOf, URIRef(t_box.ontopowsys_PowerSystemModel + 'PolynomialCostFunctionParameter'))) 
            g.add((URIRef(t_box.ontopowsys_PowerSystemModel + 'PolynomialCostFunctionParameter'), RDFS.subClassOf, URIRef(t_box.ontopowsys_PowerSystemModel + 'genCostcn-2')))
            
        ###add EGen model parametor###
        
        uk_egen_model_ = initialiseEGenModelVar(uk_egen_model, egen, OrderedBusNodeIRIList, capa_demand_ratio)
        
        g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_model_.BUSNUMKey, int(uk_egen_model_.BUS), None, ontopowsys_PowerSystemModel.BusNumber.iri)
        ModelInputVariableIRIList.append(varNode)
       
        g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_model_.PG_INPUTKey, float(uk_egen_model_.PG_INPUT), ontocape_derived_SI_units.MW.iri, ontopowsys_PowerSystemModel.Pg.iri)
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_model_.QG_INPUTKey, float(uk_egen_model_.QG_INPUT), ontocape_derived_SI_units.Mvar.iri, ontopowsys_PowerSystemModel.Qg.iri)
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_model_.QMAXKey, float(uk_egen_model_.QMAX), ontocape_derived_SI_units.Mvar.iri, ontopowsys_PowerSystemModel.QMax.iri)
        ModelInputVariableIRIList.append(varNode)

        g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_model_.QMINKey, float(uk_egen_model_.QMIN), ontocape_derived_SI_units.Mvar.iri, ontopowsys_PowerSystemModel.QMin.iri)
        ModelInputVariableIRIList.append(varNode)

        g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_model_.VGKey, int(uk_egen_model_.VG), None, ontopowsys_PowerSystemModel.Vg.iri)
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_model_.MBASEKey, int(uk_egen_model_.MBASE), ontocape_derived_SI_units.MVA.iri, ontopowsys_PowerSystemModel.mBase.iri)
        ModelInputVariableIRIList.append(varNode)

        g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_model_.STATUSKey, int(uk_egen_model_.STATUS), None, ontopowsys_PowerSystemModel.Status.iri)
        ModelInputVariableIRIList.append(varNode)

        g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_model_.PMAXKey, float(uk_egen_model_.PMAX), ontocape_derived_SI_units.MW.iri, ontopowsys_PowerSystemModel.PMax.iri)
        ModelInputVariableIRIList.append(varNode)

        g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_model_.PMINKey, float(uk_egen_model_.PMIN), ontocape_derived_SI_units.MW.iri, ontopowsys_PowerSystemModel.PMin.iri) 
        ModelInputVariableIRIList.append(varNode)

        g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_model_.PC1Key, int(uk_egen_model_.PC1), None, ontopowsys_PowerSystemModel.Pc1.iri)
        ModelInputVariableIRIList.append(varNode)

        g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_model_.PC2Key, int(uk_egen_model_.PC2), None, ontopowsys_PowerSystemModel.Pc2.iri)  
        ModelInputVariableIRIList.append(varNode)

        g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_model_.QC1MINKey, int(uk_egen_model_.QC1MIN), None, ontopowsys_PowerSystemModel.QC1Min.iri)
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_model_.QC2MINKey, int(uk_egen_model_.QC2MIN), None, ontopowsys_PowerSystemModel.QC2Min.iri)  
        ModelInputVariableIRIList.append(varNode)

        g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_model_.QC1MAXKey, int(uk_egen_model_.QC1MAX), None, ontopowsys_PowerSystemModel.QC1Max.iri)
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_model_.QC2MAXKey, int(uk_egen_model_.QC2MAX), None, ontopowsys_PowerSystemModel.QC2Max.iri)  
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_model_.RAMP_AGCKey, int(uk_egen_model_.RAMP_AGC), None, ontopowsys_PowerSystemModel.Rampagc.iri)             
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_model_.RAMP_10Key, int(uk_egen_model_.RAMP_10), None, ontopowsys_PowerSystemModel.Ramp10.iri)     
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_model_.RAMP_30Key, int(uk_egen_model_.RAMP_30), None, ontopowsys_PowerSystemModel.Ramp30.iri)     
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_model_.RAMP_QKey, int(uk_egen_model_.RAMP_Q), None, ontopowsys_PowerSystemModel.Rampq.iri)         
        ModelInputVariableIRIList.append(varNode)
        
        g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_model_.APFKey, int(uk_egen_model_.APF), None, ontopowsys_PowerSystemModel.APF.iri)      
        ModelInputVariableIRIList.append(varNode)

        ## add derviation 
        createMarkUpDerivation(list(ModelInputVariableIRIList), AgentIRI, [generatorNodeIRI, egen[5]], derivationClient, False)

        counter += 1

    ## generate/update OWL files
    if updateLocalOWLFile == True: 
        ## Store/update the generated owl files      
        if filepath[-2:] != '\\': 
            filepath_ = filepath + '\\' + 'GenModel_' + str(numOfBus) + '_Bus_Grid_' + str(counter) + OWL
        else:
            filepath_ = filepath + 'GenModel_' + str(numOfBus) + '_Bus_Grid_' + str(counter)  + OWL 
        storeGeneratedOWLs(g, filepath_)
        print(filepath_)
        ## update the graph to endpoint
        #TODO: change the endpoint
        endPointURL = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test3/sparql"
        sparql_client = PySparqlClient(endPointURL, endPointURL)   #(endpoint_iri, endpoint_iri)
        sparql_client.uploadOntology(filepath_)

        # counter += 1


    print("################FINISH createModel_EGen#################")
    if isinstance(store, Sleepycat):  
        cg_model_EGen.close()       
    return

def initialiseEGenModelVar(EGen_Model, egen, OrderedBusNodeIRIList, demand_capa_ratio):
    if not isinstance (EGen_Model, UK_PG.UKEGenModel):
        raise Exception('The first argument should be an instence of UKEGenModel')
    EGen_Model.BUS = int(OrderedBusNodeIRIList.index(egen[5]))
    capa = egen[6]
    EGen_Model.PG_INPUT = capa * demand_capa_ratio    
    primaryFuel = egen[7]
    
    if primaryFuel in ukmf.Renewable:
        EGen_Model.PMAX = EGen_Model.PG_INPUT
        EGen_Model.PMIN = EGen_Model.PG_INPUT
    else:
        EGen_Model.PMAX = capa
        EGen_Model.PMIN = 0
    
    EGen_Model.QMAX = EGen_Model.PMAX
    EGen_Model.QMIN = -EGen_Model.PMAX
    
    return EGen_Model

"""Calculate the sum of capacity and total demanding"""
def demandAndCapacityRatioCalculator(EGenInfo, topologyNodeIRI, startTime_of_EnergyConsumption):
    sum_of_capa = 0
    for eg in EGenInfo:
        sum_of_capa += eg[6]
    print('\\\\\sum_of_capa is: ', sum_of_capa)
    total_demand = query_model.queryTotalElecConsumptionofGBOrUK(endpoint_label, topologyNodeIRI, startTime_of_EnergyConsumption) * 1000 / (24 * 365) 
    print('######total_demand:', total_demand)
    demand_capa_ratio = total_demand/sum_of_capa
    print('demand_capa_ratio is: ', demand_capa_ratio)
    return demand_capa_ratio

if __name__ == '__main__':    
    # createModel_EGen('default', False, 2019, "2017-01-31", 10, 14, 50, 2, 3, None, True) 
    # createModel_EGen('default', False, 2019, "2017-01-31", 29, 99, 50, 2, 3, None, True)

    jpsBaseLibGW = JpsBaseLib()
    jpsBaseLibGW.launchGateway()

    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")
    jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.derivation.*")

    endPointURL = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test3/sparql"
    storeClient = jpsBaseLib_view.RemoteStoreClient(endPointURL, endPointURL)

    topologyNodeIRI = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerGridTopology_7ea91c81-9f7f-4d27-9b75-9b897171bbc4"
    powerSystemModelIRI = "http://www.theworldavatar.com/kb/ontoenergysystem/PowerSystemModel_22fe8504-f3bb-403c-9363-34b258d59712"
    AgentIRI = "http://www.example.com/triplestore/agents/Service__XXXAgent#Service"

    ## set up the derivationInstanceBaseURL
    derivationInstanceBaseURL = dt.baseURL + '/' + dt.topNode + '/'
    ## initialise the derivationClient
    derivationClient = jpsBaseLib_view.DerivationClient(storeClient, derivationInstanceBaseURL)

    OrderedBusNodeIRIList= ['http://www.theworldavatar.com/kb/ontopowsys/BusNode_2b04446d-9a1b-4af3-b7d8-fd40bf670344', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_f5353119-e428-41cf-8b76-01784a8f88c4', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_375a0eaf-adf0-4faf-b5d0-80d3000d9834', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_e950a401-4542-4491-a127-e4e3424f6685', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_1d9a4ab2-7ced-410e-bccd-3caa554f328f', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_fc689f69-536d-426d-a8c1-c0f5b0abd5a2', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_b3af588e-b7f4-4ba7-9d83-d9eb817a8045', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_85818347-cd94-4de3-8bc2-479030c38bcd', 
'http://www.theworldavatar.com/kb/ontopowsys/BusNode_b88721cf-07c9-41b3-9dda-f4970a78cb83', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_c8544b21-d85c-4806-a1ee-7229d1c84b87']
    
    createModel_EGen(topologyNodeIRI, powerSystemModelIRI, AgentIRI, OrderedBusNodeIRIList, derivationClient, \
        "2017-01-31", False, 0, 2, 3, None, True, 'default')
    print('***********************Terminated***********************')