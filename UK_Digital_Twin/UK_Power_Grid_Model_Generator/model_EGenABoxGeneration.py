##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 09 June 2022         #
##########################################

"""This module is designed to generate and update the A-box of UK power grid model_EGen."""
# run time: around 20 mins

from math import ceil
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
from UK_Digital_Twin_Package import UKEnergyConsumption as UKec
from UK_Digital_Twin_Package import CO2FactorAndGenCostFactor as ModelFactor
from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs, selectStoragePath, readFile, specifyValidFilePath
from UK_Power_Grid_Model_Generator.costFunctionParameterInitialiser import costFuncPara
from UK_Power_Grid_Model_Generator.AddModelVariables import AddModelVariable
from UK_Digital_Twin_Package.GraphStore import LocalGraphStore
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel as endpointList

import UK_Power_Grid_Model_Generator.SPARQLQueryUsedInModel as query_model
from UK_Digital_Twin_Package.derivationInterface import createMarkUpDerivation
from pyderivationagent.kg_operations.sparql_client import PySparqlClient # the import of this agent will need a parckage name werkzeug, install `pip install Werkzeug==2.0.2`, otherwise it will report the error message
import uuid
from py4jps.resources import JpsBaseLib

import numpy as np 

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

"""Create an object of Class CO2FactorAndGenCostFactor"""
ukmf = ModelFactor.ModelFactor()

"""Create an object of Class UKPowerGridModel"""
# uk_egen_model = UK_PG.UKEGenModel()

"""Create an object of Class UKPowerGridTopology"""
uk_topo = UK_Topo.UKPowerGridTopology()

"""Create an object of Class UKEnergyConsumption"""
ukec = UKec.UKEnergyConsumption()

"""Blazegraph UK digital tiwn"""
endpoint_label = endpointList.ukdigitaltwin['label']
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
ontoecape_space_and_time_extended = owlready2.get_ontology(t_box.ontoecape_space_and_time_extended).load()

"""User specified folder path"""
filepath = None
userSpecified = False

"""EGen Conjunctive graph identifier"""
model_EGen_cg_id = "http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EGen"

"""Global variables EGenInfo array and capa_demand_ratio"""
EGenInfo = []
capa_demand_ratio = 0
number_of_localOWLFiles = 1

### Functions ### 
"""Main function: create the named graph Model_EGen and their sub graphs each EGen"""
def createModel_EGen(numOfBus:int, topologyNodeIRI, powerSystemModelIRI, powerSystemNodetimeStamp, AgentIRI, OrderedBusNodeIRIList, derivationClient, updateEndpointIRI, startTime_of_EnergyConsumption, \
    OPFOrPF:bool, CarbonTax, piecewiseOrPolynomial, pointsOfPiecewiseOrcostFuncOrder, splitCharacter, OWLFileStoragePath, updateLocalOWLFile = True, storeType = "default"):
    ## Query generator attributes and the number of the buses
    EGenInfo = list(query_model.queryEGenInfo(topologyNodeIRI, endpoint_label))
    # location = query_model.queryPowerSystemLocation(endpoint_label, topologyNodeIRI)  
    ## TODO: the UKEGenModel initialisation function has changed 
    uk_egen_model = UK_PG.UKEGenModel(numOfBus)

    ## Set up the default storage path
    defaultStoredPath = uk_egen_model.StoreGeneratedOWLs
    defaultPath_Sleepycat = uk_egen_model.SleepycatStoragePath
    filepath = specifyValidFilePath(defaultStoredPath, OWLFileStoragePath, updateLocalOWLFile)
    if filepath == None:
        return   
    store = LocalGraphStore(storeType)
    global userSpecifiePath_Sleepycat, userSpecified_Sleepycat, capa_demand_ratio, number_of_localOWLFiles
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
    
    ## calcualte the ratio between total power plant capacity and the total demand; 
        ## 1. If PF, this ratio is used to assign the generator output;
        ## 2. If OPF, this ratio is used make the initial guess of the generator output
    capa_demand_ratio = demandAndCapacityRatioCalculator(EGenInfo, topologyNodeIRI, startTime_of_EnergyConsumption)
    
    print('################START createModel_EGen#################')
    namespace = UK_PG.ontopowsys_namespace  
    ## ElectricalGenerator node IRI 
    ElectricalGeneratorModelIRI = namespace + uk_egen_model.ModelEGenKey + str(uuid.uuid4()) # root node

    counter = 0
    ## The total number of the local owl files should be  generated and each file includes 500 generator enetities
    totalFileNumber = ceil(len(EGenInfo) / 500)

    ret_genCost_array = []
    ret_gen_array = []

    while number_of_localOWLFiles <= totalFileNumber: 
        ## create a named graph
        ontologyIRI = dt.baseURL + SLASH + dt.topNode + SLASH + str(uuid.uuid4())
        g = Graph(store = store, identifier = URIRef(ontologyIRI))
        ## Import T-boxes
        g.set((g.identifier, RDF.type, OWL_NS['Ontology']))
        g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_mathematical_model)))
        g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_upper_level_system)))  
        g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontopowsys_PowerSystemModel))) 
        g.set((g.identifier, RDFS.comment, Literal('This ontology represents mathematical model of the electricity generator of the UK energy system.'))) 
        ## Link topologyNodeIRI with PowerSystemModel and ElectricalGeneratorModelIRI
        g.add((URIRef(powerSystemModelIRI), URIRef(ontopowsys_PowerSystemModel.hasModelingPrinciple.iri), URIRef(topologyNodeIRI)))
        g.add((URIRef(powerSystemModelIRI), URIRef(ontoecape_space_and_time_extended.hasTimestamp.iri), Literal(powerSystemNodetimeStamp, \
            datatype = XSD.dateTimeStamp)))
        g.add((URIRef(ElectricalGeneratorModelIRI), URIRef(ontocape_upper_level_system.isExclusivelySubsystemOf.iri), URIRef(powerSystemModelIRI)))
        g.add((URIRef(ElectricalGeneratorModelIRI), RDF.type, URIRef(t_box.ontopowsys_PowerSystemModel + 'ElectricalGeneratorModel')))
        
        if OPFOrPF: 
            g.add((URIRef(powerSystemModelIRI), RDF.type, URIRef(t_box.ontopowsys_PowerSystemModel + 'OptimalPowerFlowModel')))
        else:
            g.add((URIRef(powerSystemModelIRI), RDF.type, URIRef(t_box.ontopowsys_PowerSystemModel + 'PowerFlowModel')))
        
        g.add((URIRef(t_box.ontopowsys_PowerSystemModel + 'OptimalPowerFlowModel'), RDFS.subClassOf, URIRef(ontopowsys_PowerSystemModel.PowerSystemModel.iri)))
        g.add((URIRef(t_box.ontopowsys_PowerSystemModel + 'PowerFlowModel'), RDFS.subClassOf, URIRef(ontopowsys_PowerSystemModel.PowerSystemModel.iri)))
            
        # for egen in EGenInfo: 
        while counter < (500 * number_of_localOWLFiles) and counter < len(EGenInfo): 
            ## generatorNodeIRI of the topology
            print("This is the generator number " + str(counter) + " out of " + str(len(EGenInfo)))
            egen = EGenInfo[counter]
            generatorNodeIRI = egen[0]
            ## link the ElectricalGeneratorModelIRI with topology generatorNodeIRI
            g.add((URIRef(generatorNodeIRI), URIRef(ontocape_upper_level_system.isModeledBy.iri), URIRef(ElectricalGeneratorModelIRI)))
            
            ## ModelInputVariableIRIList used to store the model input variables IRIs for the creation of the derivation 
            ModelInputVariableIRIList = []
            ###add cost function parameters###
            if OPFOrPF is True: # when OPFOrPF is true, the model is OPF analysis
                ## calculate cost objective function coefficients
                ## TODO: the UKEGenModel initialisation function has changed 
                uk_egen_costFunc = UK_PG.UKEGenModel_CostFunc(CarbonTax, piecewiseOrPolynomial, pointsOfPiecewiseOrcostFuncOrder) # declear an instance of the UKEGenModel_CostFunc
                uk_egen_costFunc = costFuncPara(uk_egen_costFunc, egen)
                
                ret_genCost_array.append(str(int(uk_egen_costFunc.MODEL)) + splitCharacter + str(float(uk_egen_costFunc.STARTUP)) + splitCharacter + str(float(uk_egen_costFunc.SHUTDOWN)) + splitCharacter + str(int(uk_egen_costFunc.NCOST)) + splitCharacter + str(float(uk_egen_costFunc.a)) + splitCharacter + str(float(uk_egen_costFunc.b)))
  
                ## assign value to attributes (linear function)
                g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_costFunc.CostFuncFormatKey, int(uk_egen_costFunc.MODEL), None, ontopowsys_PowerSystemModel.CostModel.iri)
                ModelInputVariableIRIList.append(varNode)

                g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_costFunc.StartupCostKey, float(uk_egen_costFunc.STARTUP), ontocape_derived_SI_units.USD.iri, ontopowsys_PowerSystemModel.StartCost.iri)
                ModelInputVariableIRIList.append(varNode)

                g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_costFunc.ShutdownCostKey, float(uk_egen_costFunc.SHUTDOWN), ontocape_derived_SI_units.USD.iri, ontopowsys_PowerSystemModel.StopCost.iri)
                ModelInputVariableIRIList.append(varNode)

                g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_costFunc.genCostnKey, int(uk_egen_costFunc.NCOST), None, ontopowsys_PowerSystemModel.genCostn.iri)
                ModelInputVariableIRIList.append(varNode)

                g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_costFunc.genCost_aKey, float(uk_egen_costFunc.a), t_box.ontocape_derived_SI_units + 'GBP/MWh', t_box.ontopowsys_PowerSystemModel + 'FirstOrderCoefficient')
                ModelInputVariableIRIList.append(varNode)

                g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_costFunc.genCost_bKey, float(uk_egen_costFunc.b), t_box.ontocape_derived_SI_units + 'GBP/MWh', t_box.ontopowsys_PowerSystemModel + 'ZeroOrderCoefficient')
                ModelInputVariableIRIList.append(varNode)

                # g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, uk_egen_costFunc.genCost_cKey, uk_egen_costFunc.c, t_box.ontocape_derived_SI_units + 'GBP/MWh', t_box.ontopowsys_PowerSystemModel + 'SecondOrderCoefficient') # undified unit
                # ModelInputVariableIRIList.append(varNode)
                
                g.add((URIRef(t_box.ontopowsys_PowerSystemModel + 'ZeroOrderCoefficient'), RDFS.subClassOf, URIRef(t_box.ontopowsys_PowerSystemModel + 'PolynomialCostFunctionParameter'))) 
                g.add((URIRef(t_box.ontopowsys_PowerSystemModel + 'FirstOrderCoefficient'), RDFS.subClassOf, URIRef(t_box.ontopowsys_PowerSystemModel + 'PolynomialCostFunctionParameter'))) 
                # g.add((URIRef(t_box.ontopowsys_PowerSystemModel + 'SecondOrderCoefficient'), RDFS.subClassOf, URIRef(t_box.ontopowsys_PowerSystemModel + 'PolynomialCostFunctionParameter'))) 
                g.add((URIRef(t_box.ontopowsys_PowerSystemModel + 'PolynomialCostFunctionParameter'), RDFS.subClassOf, URIRef(t_box.ontopowsys_PowerSystemModel + 'genCostcn-2')))
                
            ###add EGen model parametor###
            uk_egen_model_ = initialiseEGenModelVar(uk_egen_model, egen, OrderedBusNodeIRIList, capa_demand_ratio)
            
            ret_gen_array.append(str(int(uk_egen_model_.BUS)) + splitCharacter + str(float(uk_egen_model_.PG_INPUT)) + splitCharacter + str(float(uk_egen_model_.QG_INPUT)) + splitCharacter + str(float(uk_egen_model_.QMAX)) + splitCharacter + str(float(uk_egen_model_.QMIN)) + splitCharacter + str(int(uk_egen_model_.VG)) + splitCharacter +\
                str(int(uk_egen_model_.MBASE)) + splitCharacter + str(int(uk_egen_model_.STATUS)) + splitCharacter + str(float(uk_egen_model_.PMAX)) + splitCharacter + str(float(uk_egen_model_.PMIN)) + splitCharacter + str(int(uk_egen_model_.PC1)) + splitCharacter + str(int(uk_egen_model_.PC2)) + splitCharacter + str(int(uk_egen_model_.QC1MIN)) + splitCharacter +\
                str(int(uk_egen_model_.QC2MIN)) + splitCharacter + str(int(uk_egen_model_.QC1MAX)) + splitCharacter + str(int(uk_egen_model_.QC2MAX)) + splitCharacter + str(int(uk_egen_model_.RAMP_AGC)) + splitCharacter + str(int(uk_egen_model_.RAMP_10)) + splitCharacter + str(int(uk_egen_model_.RAMP_30)) + splitCharacter + str(int(uk_egen_model_.RAMP_Q)) + splitCharacter + str(int(uk_egen_model_.APF)))

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

            # print(g.serialize(format="pretty-xml").decode("utf-8"))
        
            ## TODO: disable derivationClient
            ## add derviation 
            derivationClient.createAsyncDerivation(list(ModelInputVariableIRIList), AgentIRI, [generatorNodeIRI], False)
            counter += 1

        print(len(ret_gen_array), len(ret_gen_array)) 

        ## generate/update OWL files
        if updateLocalOWLFile == True: # and (counter == 500 or number_of_localOWLFiles == totalFileNumber): 
            ## Store/update the generated owl files      
            if filepath[-2:] != '\\': 
                filepath_ = filepath + '\\' + 'GenModel_' + str(numOfBus) + '_Bus_Grid_' + str(number_of_localOWLFiles) + TTL
            else:
                filepath_ = filepath + 'GenModel_' + str(numOfBus) + '_Bus_Grid_' + str(number_of_localOWLFiles) + TTL 
            storeGeneratedOWLs(g, filepath_)
            print(filepath_)

            ## TODO: disable sparql_client
            ## update the graph to endpoint
            # sparql_client = PySparqlClient(updateEndpointIRI, updateEndpointIRI)
            # sparql_client.uploadOntology(filepath_)
            ## increase the number of number_of_localOWLFiles
            number_of_localOWLFiles += 1
    
    print("...creating loacl file...")
    textfile = open("C:\\Users\\wx243\\Documents\\TheWorldAvatar\\UK_Digital_Twin\\testOPFAnalysis\\gen.txt", "w")
    for r in ret_gen_array:
        textfile.write(r + "\n")
    textfile.close()

    textfile = open("C:\\Users\\wx243\\Documents\\TheWorldAvatar\\UK_Digital_Twin\\testOPFAnalysis\\genCost.txt", "w")
    for r in ret_genCost_array:
        textfile.write(r + "\n")
    textfile.close()
    
    print("################FINISH createModel_EGen#################")
    if isinstance(store, Sleepycat):  
        cg_model_EGen.close()       
    return

def initialiseEGenModelVar(EGen_Model, egen, OrderedBusNodeIRIList, demand_capa_ratio, windOutputRatio, solarOutputRatio):
    if not isinstance (EGen_Model, UK_PG.UKEGenModel) or not isinstance (EGen_Model, UK_PG.UKEGenModel_CostFunc):
        raise Exception('The first argument should be an instence of UKEGenModel or UKEGenModel_CostFunc')
    EGen_Model.BUS = int(OrderedBusNodeIRIList.index(egen[5])) # the connected bus number of the current generator should be in line with the index of the bus list
    capa = egen[6]
    EGen_Model.PG_INPUT = round((capa * demand_capa_ratio), 4)   
    
    primaryFuel = egen[7]
    if primaryFuel in ukmf.Wind: 
        EGen_Model.PMAX = capa * float(windOutputRatio)
    elif primaryFuel in ukmf.Solar: 
        EGen_Model.PMAX = capa * float(solarOutputRatio)  
    elif primaryFuel in ukmf.Nuclear: 
        EGen_Model.PMAX = capa * 0.55
    elif primaryFuel in ukmf.Hydro: 
        EGen_Model.PMAX = capa * 0.3
    elif primaryFuel in ukmf.PumpHydro: 
        EGen_Model.PMAX = capa * 0.1
    elif primaryFuel in ukmf.Bio: 
        EGen_Model.PMAX = capa * 0.70
    elif primaryFuel in ukmf.SMR:
        EGen_Model.PMAX = capa * 0.94
    else:
        EGen_Model.PMAX = capa * 0.9
    
    EGen_Model.PMIN = 0
    
    # EGen_Model.PMAX = capa
    # EGen_Model.PMIN = 0

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

    ## set up the derivationInstanceBaseURL
    derivationInstanceBaseURL = dt.baseURL + '/' + dt.topNode + '/'
    ## initialise the derivationClient
    derivationClient = jpsBaseLib_view.DerivationClient(storeClient, derivationInstanceBaseURL)

    OrderedBusNodeIRIList= ['http://www.theworldavatar.com/kb/ontopowsys/BusNode_d6046ef2-6909-4f20-808f-cd9aa01c8ae5', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_ebace1f4-7d3a-44f6-980e-a4b844de670b', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_1f3c4462-3472-4949-bffb-eae7d3135591', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_f17335d2-53f6-4044-9d09-c3d9438c0950', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_c4d7dcca-a7f5-4887-a460-31706ab7ec9c', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_024c0566-d9f0-497d-955e-f7f4e55d4296', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_2d76797b-c638-460e-b73c-769e29785466', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_55285d5a-1d0e-4b1f-8713-246d601671e5', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_6202e767-3077-4910-9cb7-a888e80af788', 'http://www.theworldavatar.com/kb/ontopowsys/BusNode_84f6905c-d4cb-409f-861f-ea66fe25ddd0']

    createModel_EGen(10, topologyNodeIRI_10Bus, powerSystemModelIRI, "2022-06-15T16:24:29.371941+00:00", AgentIRI, OrderedBusNodeIRIList, derivationClient, endPointURL,\
        "2017-01-31", True, 18, 2, 2, ' ', None, True, 'default')
    print('***********************Terminated***********************')