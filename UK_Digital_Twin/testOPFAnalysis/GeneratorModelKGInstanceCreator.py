##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 30 June 2022         #
##########################################

"""This module is designed to generate and update the A-box of UK power grid model_EGen."""
from math import ceil
import os
import owlready2
from rdflib.extras.infixowl import OWL_NS
from rdflib import Graph, URIRef, Literal
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
from UK_Digital_Twin_Package.OWLfileStorer import storeGeneratedOWLs, specifyValidFilePath
from UK_Power_Grid_Model_Generator.costFunctionParameterInitialiser import costFuncPara
from UK_Power_Grid_Model_Generator.AddModelVariables import AddModelVariable
from UK_Digital_Twin_Package.GraphStore import LocalGraphStore
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel as endpointList
import UK_Power_Grid_Model_Generator.SPARQLQueryUsedInModel as query_model
from pyderivationagent.kg_operations.sparql_client import PySparqlClient # the import of this agent will need a parckage name werkzeug, install `pip install Werkzeug==2.0.2`, otherwise it will report the error message
import uuid
from py4jps.resources import JpsBaseLib
import numpy as np 
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

"""Create an object of Class UKPowerPlant"""
ukpp = UKpp.UKPowerPlant()

"""Create an object of Class CO2FactorAndGenCostFactor"""
ukmf = ModelFactor.ModelFactor()

"""Create an object of Class UKPowerGridTopology"""
uk_topo = UK_Topo.UKPowerGridTopology()

"""Create an object of Class UKEnergyConsumption"""
ukec = UKec.UKEnergyConsumption()

"""Blazegraph UK digital tiwn"""
endpoint_label = endpointList.ukdigitaltwin['label']
endpoint_iri = endpointList.ukdigitaltwin['queryendpoint_iri']

"""User specified folder path"""
filepath = None
userSpecified = False

"""Global variables EGenInfo array and capa_demand_ratio"""
number_of_localOWLFiles = 1

### Functions ### 
"""Main function: create the named graph Model_EGen and their sub graphs each EGen"""
def GeneratorModelKGInstanceCreator(ObjectSet, GeneratorObjectNameList, retrofitResults, OPFIndicator, newGeneratorType:str, numOfBus:int, topologyNodeIRI, powerSystemModelIRI, powerSystemNodetimeStamp, \
    AgentIRI, derivationClient, updateEndpointIRI, OWLFileStoragePath, updateLocalOWLFile = True, storeType = "default"):
    """T-Box URI"""
    if urllib.request.urlopen("http://www.theworldavatar.com/ontology/").getcode() == 200:
        ontocape_upper_level_system     = owlready2.get_ontology(t_box.ontocape_upper_level_system).load()
        ontocape_derived_SI_units       = owlready2.get_ontology(t_box.ontocape_derived_SI_units).load()
        ontocape_mathematical_model     = owlready2.get_ontology(t_box.ontocape_mathematical_model).load()
        ontopowsys_PowerSystemModel     = owlready2.get_ontology(t_box.ontopowsys_PowerSystemModel).load()
        ontoecape_space_and_time_extended = owlready2.get_ontology(t_box.ontoecape_space_and_time_extended).load()
        ontoeip_powerplant              = owlready2.get_ontology(t_box.ontoeip_powerplant).load()
        ontocape_technical_system      = owlready2.get_ontology(t_box.ontoecape_technical_system).load() 
    else:
        print('---THE WORLD AVATAR NOT FOUND---')
    ## Set up the default storage path
    store = LocalGraphStore(storeType)
    
    global number_of_localOWLFiles  
    print('################START createModel_EGen#################')
    namespace = UK_PG.ontopowsys_namespace  
    ## ElectricalGenerator node IRI 
    ElectricalGeneratorModelIRI = namespace + UK_PG.UKEGenModel.ModelEGenKey + str(uuid.uuid4()) # root node

    counter = 0
    ## The total number of the local owl files should be  generated and each file includes 500 generator enetities
    totalFileNumber = ceil(len(GeneratorObjectNameList) / 500)
    
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
        
        if OPFIndicator: 
            g.add((URIRef(powerSystemModelIRI), RDF.type, URIRef(t_box.ontopowsys_PowerSystemModel + 'OptimalPowerFlowModel')))
        else:
            g.add((URIRef(powerSystemModelIRI), RDF.type, URIRef(t_box.ontopowsys_PowerSystemModel + 'PowerFlowModel')))
        
        g.add((URIRef(t_box.ontopowsys_PowerSystemModel + 'OptimalPowerFlowModel'), RDFS.subClassOf, URIRef(ontopowsys_PowerSystemModel.PowerSystemModel.iri)))
        g.add((URIRef(t_box.ontopowsys_PowerSystemModel + 'PowerFlowModel'), RDFS.subClassOf, URIRef(ontopowsys_PowerSystemModel.PowerSystemModel.iri)))

        containerOfModelVariableIRIList = []
        containerOfAgentIRIList = []
        containerOfGeneratorNodeIRI = []
           
        ### craete generator instances with both input and output atrributes ###
        while counter < (500 * number_of_localOWLFiles) and counter < len(GeneratorObjectNameList): 
            ## generatorNodeIRI of the topology
            print("This is the generator number " + str(counter) + " out of " + str(len(GeneratorObjectNameList)))
            # busName = GeneratorObjectNameList[counter]
            egen = ObjectSet[GeneratorObjectNameList[counter]]
            generatorNodeIRI = egen.generatorNodeIRI
            ## link the ElectricalGeneratorModelIRI with topology generatorNodeIRI
            g.add((URIRef(generatorNodeIRI), URIRef(ontocape_upper_level_system.isModeledBy.iri), URIRef(ElectricalGeneratorModelIRI)))
            
            ## ModelInputVariableIRIList used to store the model input variables IRIs for the creation of the derivation 
            ModelVariableIRIList = []
            
            ## assign model input variables
            ## 1. cost function 
            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.CostFuncFormatKey, int(egen.MODEL), None, ontopowsys_PowerSystemModel.CostModel.iri, True)
            ModelVariableIRIList.append(varNode)

            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.StartupCostKey, float(egen.STARTUP), ontocape_derived_SI_units.USD.iri, ontopowsys_PowerSystemModel.StartCost.iri, True)
            ModelVariableIRIList.append(varNode)

            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.ShutdownCostKey, float(egen.SHUTDOWN), ontocape_derived_SI_units.USD.iri, ontopowsys_PowerSystemModel.StopCost.iri, True)
            ModelVariableIRIList.append(varNode)

            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.genCostnKey, int(egen.NCOST), None, ontopowsys_PowerSystemModel.genCostn.iri, True)
            ModelVariableIRIList.append(varNode)

            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.genCost_aKey, float(egen.a), t_box.ontocape_derived_SI_units + 'GBP/MWh', t_box.ontopowsys_PowerSystemModel + 'FirstOrderCoefficient', True)
            ModelVariableIRIList.append(varNode)

            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.genCost_bKey, float(egen.b), t_box.ontocape_derived_SI_units + 'GBP/MWh', t_box.ontopowsys_PowerSystemModel + 'ZeroOrderCoefficient', True)
            ModelVariableIRIList.append(varNode)

            g.add((URIRef(t_box.ontopowsys_PowerSystemModel + 'ZeroOrderCoefficient'), RDFS.subClassOf, URIRef(t_box.ontopowsys_PowerSystemModel + 'PolynomialCostFunctionParameter'))) 
            g.add((URIRef(t_box.ontopowsys_PowerSystemModel + 'FirstOrderCoefficient'), RDFS.subClassOf, URIRef(t_box.ontopowsys_PowerSystemModel + 'PolynomialCostFunctionParameter')))  
            g.add((URIRef(t_box.ontopowsys_PowerSystemModel + 'PolynomialCostFunctionParameter'), RDFS.subClassOf, URIRef(t_box.ontopowsys_PowerSystemModel + 'genCostcn-2')))
            
           
            ## 2. other input data
            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.BUSNUMKey, int(egen.BUS), None, ontopowsys_PowerSystemModel.BusNumber.iri, True)
            ModelVariableIRIList.append(varNode)
        
            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.PG_INPUTKey, float(egen.PG_INPUT), ontocape_derived_SI_units.MW.iri, ontopowsys_PowerSystemModel.Pg.iri, True)
            ModelVariableIRIList.append(varNode)
            
            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.QG_INPUTKey, float(egen.QG_INPUT), ontocape_derived_SI_units.Mvar.iri, ontopowsys_PowerSystemModel.Qg.iri, True)
            ModelVariableIRIList.append(varNode)
            
            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.QMAXKey, float(egen.QMAX), ontocape_derived_SI_units.Mvar.iri, ontopowsys_PowerSystemModel.QMax.iri, True)
            ModelVariableIRIList.append(varNode)

            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.QMINKey, float(egen.QMIN), ontocape_derived_SI_units.Mvar.iri, ontopowsys_PowerSystemModel.QMin.iri, True)
            ModelVariableIRIList.append(varNode)

            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.VGKey, int(egen.VG), None, ontopowsys_PowerSystemModel.Vg.iri, True)
            ModelVariableIRIList.append(varNode)
            
            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.MBASEKey, int(egen.MBASE), ontocape_derived_SI_units.MVA.iri, ontopowsys_PowerSystemModel.mBase.iri, True)
            ModelVariableIRIList.append(varNode)

            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.STATUSKey, int(egen.STATUS), None, ontopowsys_PowerSystemModel.Status.iri, True)
            ModelVariableIRIList.append(varNode)

            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.PMAXKey, float(egen.PMAX), ontocape_derived_SI_units.MW.iri, ontopowsys_PowerSystemModel.PMax.iri, True)
            ModelVariableIRIList.append(varNode)

            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.PMINKey, float(egen.PMIN), ontocape_derived_SI_units.MW.iri, ontopowsys_PowerSystemModel.PMin.iri, True) 
            ModelVariableIRIList.append(varNode)

            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.PC1Key, int(egen.PC1), None, ontopowsys_PowerSystemModel.Pc1.iri, True)
            ModelVariableIRIList.append(varNode)

            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.PC2Key, int(egen.PC2), None, ontopowsys_PowerSystemModel.Pc2.iri, True)  
            ModelVariableIRIList.append(varNode)

            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.QC1MINKey, int(egen.QC1MIN), None, ontopowsys_PowerSystemModel.QC1Min.iri, True)
            ModelVariableIRIList.append(varNode)
            
            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.QC2MINKey, int(egen.QC2MIN), None, ontopowsys_PowerSystemModel.QC2Min.iri, True)  
            ModelVariableIRIList.append(varNode)

            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.QC1MAXKey, int(egen.QC1MAX), None, ontopowsys_PowerSystemModel.QC1Max.iri, True)
            ModelVariableIRIList.append(varNode)
            
            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.QC2MAXKey, int(egen.QC2MAX), None, ontopowsys_PowerSystemModel.QC2Max.iri, True)  
            ModelVariableIRIList.append(varNode)
            
            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.RAMP_AGCKey, int(egen.RAMP_AGC), None, ontopowsys_PowerSystemModel.Rampagc.iri, True)             
            ModelVariableIRIList.append(varNode)
            
            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.RAMP_10Key, int(egen.RAMP_10), None, ontopowsys_PowerSystemModel.Ramp10.iri, True)     
            ModelVariableIRIList.append(varNode)
            
            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.RAMP_30Key, int(egen.RAMP_30), None, ontopowsys_PowerSystemModel.Ramp30.iri, True)     
            ModelVariableIRIList.append(varNode)
            
            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.RAMP_QKey, int(egen.RAMP_Q), None, ontopowsys_PowerSystemModel.Rampq.iri, True)         
            ModelVariableIRIList.append(varNode)
            
            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.APFKey, int(egen.APF), None, ontopowsys_PowerSystemModel.APF.iri, True)      
            ModelVariableIRIList.append(varNode)

            ## 3. output data
            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.PG_OUTPUTKey, int(egen.PG_OUTPUT), None, ontopowsys_PowerSystemModel.BusNumber.iri, False)
            ModelVariableIRIList.append(varNode)
            
            g, varNode = AddModelVariable(g, ElectricalGeneratorModelIRI, namespace, egen.QG_OUTPUTKey, float(egen.QG_OUTPUT), ontocape_derived_SI_units.Mvar.iri, ontopowsys_PowerSystemModel.Qg.iri, False)
            ModelVariableIRIList.append(varNode)

            # print(g.serialize(format="pretty-xml").decode("utf-8"))

            containerOfModelVariableIRIList.append(ModelVariableIRIList)
            containerOfAgentIRIList.append(AgentIRI)
            containerOfGeneratorNodeIRI.append([generatorNodeIRI])

            counter += 1
        ## add derviation
        ## derivationClient.createAsyncDerivation(list(ModelVariableIRIList), AgentIRI, [generatorNodeIRI], False)
        derivationClient.bulkCreateAsyncDerivations(containerOfModelVariableIRIList, containerOfAgentIRIList, containerOfGeneratorNodeIRI, [False for iri in containerOfAgentIRIList])

        

        ## generate/update OWL files
        if updateLocalOWLFile == True: # and (counter == 500 or number_of_localOWLFiles == totalFileNumber): 
            ## Store/update the generated owl files 
            defaultStoredPath = ObjectSet[GeneratorObjectNameList[0]].StoreGeneratedOWLs
            filepath = specifyValidFilePath(defaultStoredPath, OWLFileStoragePath, updateLocalOWLFile)      
            if filepath[-1:] != '\\': 
                filepath_ = filepath + '\\' + 'GenModel_' + str(numOfBus) + '_Bus_Grid_' + str(number_of_localOWLFiles) + TTL
            else:
                filepath_ = filepath + 'GenModel_' + str(numOfBus) + '_Bus_Grid_' + str(number_of_localOWLFiles) + TTL 
            storeGeneratedOWLs(g, filepath_)
            print(filepath_)

            ## update the graph to endpoint
            sparql_client = PySparqlClient(updateEndpointIRI, updateEndpointIRI)
            sparql_client.uploadOntology(filepath_)
            ## increase the number of number_of_localOWLFiles
            number_of_localOWLFiles += 1
    

    ### create a KG for representing the newly added generators
    ontologyIRI = dt.baseURL + SLASH + dt.topNode + SLASH + str(uuid.uuid4())
    g = Graph(store = store, identifier = URIRef(ontologyIRI))
    ## Import T-boxes
    g.set((g.identifier, RDF.type, OWL_NS['Ontology']))
    # g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_mathematical_model)))
    # g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_upper_level_system)))  
    g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontopowsys_PowerSystemModel))) 
    g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontoeip_powerplant)))
    g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontoecape_technical_system)))
    g.set((g.identifier, RDFS.comment, Literal('This ontology represents the newly added generators and their relationship with the generators being retrofitted.'))) 
    ## Link topologyNodeIRI with PowerSystemModel and ElectricalGeneratorModelIRI
    for oldAndNewGenName in retrofitResults: 
        egen = ObjectSet[oldAndNewGenName[1]]
        newGeneratorNodeIRI = egen.generatorNodeIRI 
        existingGeneratorNodeIRI = egen.toBeRetrofittedGeneratorNodeIRI
        if newGeneratorType == "SMR" or newGeneratorType == "SmallModularReactor":
            newGeneratorType = "SmallModularReactor" 
            primaryfueltype = "Nuclear"
            designcapacity = 470
        EnergyGenerationIRI = dt.baseURL + SLASH + t_box.ontoeipName +  SLASH + ukpp.PowerGenerationKey + str(uuid.uuid4()) 
        GenerationTechnologyClassIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + newGeneratorType 
        GenerationTechnologyIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + newGeneratorType + UNDERSCORE + str(uuid.uuid4())
        PrimaryFuelTypeIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + primaryfueltype + UNDERSCORE + str(uuid.uuid4())
        RequirementsAspectIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + ukpp.RequirementsAspectKey + str(uuid.uuid4()) 
        valueOfRequirementsAspectIRI = dt.baseURL + SLASH + t_box.ontoeipName + SLASH + ukpp.valueKey + str(uuid.uuid4())

        ## create new added generators and their relationship with the generators being retrofitted
        g.add((URIRef(newGeneratorNodeIRI), RDF.type, URIRef(ontoeip_powerplant.PowerGenerator.iri)))
        if oldAndNewGenName[0] is not None: 
            g.add((URIRef(existingGeneratorNodeIRI), URIRef(t_box.ontopowsys_PowerSystemModel + "isReplacedBy"), URIRef(newGeneratorNodeIRI)))
        else:
            g.add((URIRef(existingGeneratorNodeIRI), URIRef(t_box.ontopowsys_PowerSystemModel + "isIntegratedBy"), URIRef(newGeneratorNodeIRI)))
        ## add generation technology
        g.add((URIRef(newGeneratorNodeIRI), URIRef(ontocape_technical_system.realizes.iri), URIRef(EnergyGenerationIRI)))    
        g.add((URIRef(EnergyGenerationIRI), RDF.type, URIRef(ontoeip_powerplant.PowerGeneration.iri))) 
        g.add((URIRef(EnergyGenerationIRI), URIRef(ontoeip_powerplant.usesGenerationTechnology.iri), URIRef(GenerationTechnologyIRI))) 
        ## add primary fuelType 
        g.add((URIRef(GenerationTechnologyIRI), RDF.type, URIRef(GenerationTechnologyClassIRI)))
        g.add((URIRef(GenerationTechnologyClassIRI), RDFS.subClassOf, URIRef(ontoeip_powerplant.PlantGenerationTechnology.iri)))
        g.add((URIRef(EnergyGenerationIRI), URIRef(ontoeip_powerplant.consumesPrimaryFuel.iri), URIRef(PrimaryFuelTypeIRI)))
        g.add((URIRef(PrimaryFuelTypeIRI), RDF.type, URIRef(t_box.ontoeip_powerplant + primaryfueltype)))
        g.add((URIRef(PrimaryFuelTypeIRI), RDFS.label, Literal(str(primaryfueltype))))   
        ## add capacity
        g.add((URIRef(RequirementsAspectIRI), RDF.type, URIRef(t_box.ontoeip_system_requirement + 'DesignCapacity'))) # T-box undefined
        g.add((URIRef(RequirementsAspectIRI), URIRef(ontocape_technical_system.isAchievedThrough.iri), URIRef(EnergyGenerationIRI)))
        g.add((URIRef(RequirementsAspectIRI), URIRef(ontocape_upper_level_system.hasValue.iri), URIRef(valueOfRequirementsAspectIRI)))
        g.add((URIRef(valueOfRequirementsAspectIRI), RDF.type, URIRef(ontocape_upper_level_system.ScalarValue.iri)))
        g.add((URIRef(valueOfRequirementsAspectIRI), URIRef(ontocape_upper_level_system.hasUnitOfMeasure.iri), URIRef(ontocape_derived_SI_units.MW.iri)))
        g.add((URIRef(valueOfRequirementsAspectIRI), URIRef(ontocape_upper_level_system.numericalValue.iri), Literal(float(designcapacity))))
    ## generate/update OWL files
        if updateLocalOWLFile == True: # and (counter == 500 or number_of_localOWLFiles == totalFileNumber): 
            ## Store/update the generated owl files      
            if filepath[-2:] != '\\': 
                filepath_ = filepath + '\\' + 'AddedGenerator_' + str(numOfBus) + '_Bus_Grid_' + str(ElectricalGeneratorModelIRI.split("_")[1]) + TTL
            else:
                filepath_ = filepath + 'GenModel_' + str(numOfBus) + '_Bus_Grid_' + + str(ElectricalGeneratorModelIRI.split("_")[1]) + TTL 
            storeGeneratedOWLs(g, filepath_)
            print(filepath_)

            ## update the graph to endpoint
            sparql_client = PySparqlClient(updateEndpointIRI, updateEndpointIRI)
            sparql_client.uploadOntology(filepath_)   
    
    print("################FINISH createModel_EGen#################") 
    return

def initialiseEGenModelVar(EGen_Model, egen, OrderedBusNodeIRIList, demand_capa_ratio):
    if not isinstance (EGen_Model, UK_PG.UKEGenModel) or not isinstance (EGen_Model, UK_PG.UKEGenModel_CostFunc):
        raise Exception('The first argument should be an instence of UKEGenModel or UKEGenModel_CostFunc')
    EGen_Model.BUS = int(OrderedBusNodeIRIList.index(egen[5])) # the connected bus number of the current generator should be in line with the index of the bus list
    capa = egen[6]
    EGen_Model.PG_INPUT = capa * demand_capa_ratio    
    
    
    #TODO: how to assign the uppper and lower bound of the generator of different types
    # primaryFuel = egen[7]
    # if primaryFuel in ukmf.Renewable:
    #     EGen_Model.PMAX = EGen_Model.PG_INPUT
    #     EGen_Model.PMIN = EGen_Model.PG_INPUT
    # else:
    #     EGen_Model.PMAX = capa
    #     EGen_Model.PMIN = 0
    
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