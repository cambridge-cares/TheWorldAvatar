##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 25 Nov 2021          #
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

"""Remote Endpoint lable and queryendpoint_iri"""
powerPlant_Endpoint = ukpp.endpoint['lable']
topology_Endpoint = uk_topo.endpoint['lable']
energyConsumption_Endpoint = ukec.endpoint['lable']
powerPlant_federated_query_Endpoint = ukpp.endpoint['queryendpoint_iri']
topology_federated_query_Endpoint = uk_topo.endpoint['queryendpoint_iri']
# energyConsumption_federated_query_Endpoint = ukec.endpoint['queryendpoint_iri']


"""Blazegraph UK digital tiwn"""
endpoint_label = endpointList.ukdigitaltwin['lable']
endpoint_url = endpointList.ukdigitaltwin['queryendpoint_iri']

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
def createModel_EGen(storeType, localQuery, version_of_DUKES, startTime_of_EnergyConsumption, numOfBus, numOfBranch, CarbonTax, OWLFileStoragePath, updateLocalOWLFile = True):
    uk_egen_model = UK_PG.UKEGenModel(version_of_DUKES, numOfBus)
    defaultStoredPath = uk_egen_model.StoreGeneratedOWLs
    defaultPath_Sleepycat = uk_egen_model.SleepycatStoragePath
    filepath = specifyValidFilePath(defaultStoredPath, OWLFileStoragePath, updateLocalOWLFile)
    if filepath == None:
        return   
    store = LocalGraphStore(storeType)
    global userSpecifiePath_Sleepycat, userSpecified_Sleepycat, EGenInfo, capa_demand_ratio  
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
            
    EGenInfo = list(query_model.queryEGenInfo(numOfBus, numOfBranch, topoAndConsumpPath_Sleepycat, powerPlant_Sleepycat, localQuery, endpoint_label))

    if EGenInfo == None:
        raise Exception('EGenInfo is empty')
       
    capa_demand_ratio = demandAndCapacityRatioCalculator(EGenInfo, numOfBus, numOfBranch, startTime_of_EnergyConsumption)
    
    # TODO: The location should be the address of the top node UK digital twin which should be specified in the level one when the power plant instance being created 
    #  location = query_model.queryDigitalTwinLocation(endpoint_label, dt.SleepycatStoragePath, localQuery)  
    location = 'http://dbpedia.org/resource/United_Kingdom'
    for egen in EGenInfo:         
    # if EGenInfo[0] != None: # test
    #     egen = EGenInfo[0] # test
        print('################START createModel_EGen#################')
        root_uri = egen[0].split('#')[0]
        namespace = root_uri + HASH
        node_locator = egen[0].split('#')[1]
        root_node = namespace + 'Model_' + node_locator
        father_node = UKDT.nodeURIGenerator(4, dt.powerGridModel, numOfBus, "EGen")
        
        # create a named graph
        g = Graph(store = store, identifier = URIRef(root_uri))
        # Import T-boxes
        g.set((g.identifier, RDF.type, OWL_NS['Ontology']))
        g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_mathematical_model)))
        g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontocape_upper_level_system)))  
        g.add((g.identifier, OWL_NS['imports'], URIRef(t_box.ontopowsys_PowerSystemModel))) 
        # Add root node type and the connection between root node and its father node   
        g.add((URIRef(root_node), URIRef(ontocape_upper_level_system.isExclusivelySubsystemOf.iri), URIRef(father_node)))
        g.add((URIRef(father_node), RDFS.label, Literal("UK_Electrical_Grid_" + str(numOfBus) + "_Bus_" + str(numOfBranch) + "_Branch" )))
        g.add((URIRef(root_node), RDF.type, URIRef(ontocape_mathematical_model.Submodel.iri)))
        g.add((URIRef(root_node), RDF.type, URIRef(ontopowsys_PowerSystemModel.PowerFlowModelAgent.iri)))
        g.add((URIRef(root_node), RDF.type, URIRef(t_box.ontopowsys_PowerSystemModel + 'GeneratorModel'))) # undefined T-box class, the sub-class of PowerFlowModelAgent
        g.add((URIRef(father_node), URIRef(ontocape_upper_level_system.isComposedOfSubsystem.iri), URIRef(root_node)))
        g.add((URIRef(father_node), RDF.type, URIRef(ontocape_mathematical_model.Submodel.iri)))
        # link with EGen node in topology
        g.add((URIRef(root_node), URIRef(ontocape_upper_level_system.models.iri), URIRef(egen[0])))
        g.add((URIRef(egen[0]), URIRef(ontocape_upper_level_system.isModeledBy.iri), URIRef(root_node)))
        
        ###add cost function parameters###
        # calculate a, b, c
        uk_egen_costFunc = UK_PG.UKEGenModel_CostFunc(version_of_DUKES, CarbonTax) # declear an instance of the UKEGenModel_CostFunc
        uk_egen_costFunc = costFuncPara(uk_egen_costFunc, egen, location, localQuery)
        # assign value to attributes
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_costFunc.CostFuncFormatKey, uk_egen_costFunc.MODEL, None, \
                             ontopowsys_PowerSystemModel.CostModel.iri, ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_costFunc.StartupCostKey, uk_egen_costFunc.STARTUP, ontocape_derived_SI_units.USD.iri, \
                             ontopowsys_PowerSystemModel.StartCost.iri, ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_costFunc.ShutdownCostKey, uk_egen_costFunc.SHUTDOWN, ontocape_derived_SI_units.USD.iri, \
                             ontopowsys_PowerSystemModel.StopCost.iri, ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_costFunc.genCostnKey, uk_egen_costFunc.NCOST, None, \
                             ontopowsys_PowerSystemModel.genCostn.iri, ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_costFunc.genCost_aKey, uk_egen_costFunc.a, None, \
                             t_box.ontopowsys_PowerSystemModel + 'genCostcn-2', ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_costFunc.genCost_bKey, uk_egen_costFunc.b, t_box.ontocape_derived_SI_units + 'GBP/MWh', \
                             t_box.ontopowsys_PowerSystemModel + 'genCostcn-2', ontocape_mathematical_model.Parameter.iri) # undified unit
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_costFunc.genCost_cKey, uk_egen_costFunc.c, t_box.ontocape_derived_SI_units + 'GBP/MWh', \
                             t_box.ontopowsys_PowerSystemModel + 'genCostcn-2', ontocape_mathematical_model.Parameter.iri) # undified unit
        g.add((URIRef(namespace + uk_egen_costFunc.genCost_aKey + node_locator), RDFS.label, Literal('Parameter_a')))   
        g.add((URIRef(namespace + uk_egen_costFunc.genCost_bKey + node_locator), RDFS.label, Literal('Parameter_b')))   
        g.add((URIRef(namespace + uk_egen_costFunc.genCost_cKey + node_locator), RDFS.label, Literal('Parameter_c')))   
        
        ###add EGen model parametor###
        # uk_egen_model_ = UK_PG.UKEGenModel(DUKESVersion = version_of_DUKES, numOfBus = numOfBus)
        uk_egen_model_ = initialiseEGenModelVar(uk_egen_model, egen, capa_demand_ratio)
        
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_model_.BUSNUMKey, int(uk_egen_model_.BUS), None, \
                                 ontopowsys_PowerSystemModel.BusNumber.iri, ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_model_.PG_INPUTKey, float(uk_egen_model_.PG_INPUT), ontocape_derived_SI_units.MW.iri, \
                                 ontopowsys_PowerSystemModel.Pg.iri, ontocape_mathematical_model.InputVariable.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_model_.QG_INPUTKey, float(uk_egen_model_.QG_INPUT), ontocape_derived_SI_units.Mvar.iri, \
                                 ontopowsys_PowerSystemModel.Qg.iri, ontocape_mathematical_model.InputVariable.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_model_.QMAXKey, float(uk_egen_model_.QMAX), ontocape_derived_SI_units.Mvar.iri, \
                                 ontopowsys_PowerSystemModel.QMax.iri, ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_model_.QMINKey, float(uk_egen_model_.QMIN), ontocape_derived_SI_units.Mvar.iri, \
                                 ontopowsys_PowerSystemModel.QMin.iri, ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_model_.VGKey, int(uk_egen_model_.VG), None, \
                                 ontopowsys_PowerSystemModel.Vg.iri, ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_model_.MBASEKey, int(uk_egen_model_.MBASE), ontocape_derived_SI_units.MVA.iri, \
                                 ontopowsys_PowerSystemModel.mBase.iri, ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_model_.STATUSKey, int(uk_egen_model_.STATUS), None, \
                                 ontopowsys_PowerSystemModel.Status.iri, ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_model_.PMAXKey, float(uk_egen_model_.PMAX), ontocape_derived_SI_units.MW.iri, \
                                 ontopowsys_PowerSystemModel.PMax.iri, ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_model_.PMINKey, float(uk_egen_model_.PMIN), ontocape_derived_SI_units.MW.iri, \
                                 ontopowsys_PowerSystemModel.PMin.iri, ontocape_mathematical_model.Parameter.iri) 
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_model_.PC1Key, int(uk_egen_model_.PC1), None, \
                                 ontopowsys_PowerSystemModel.Pc1.iri, ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_model_.PC2Key, int(uk_egen_model_.PC2), None, \
                                 ontopowsys_PowerSystemModel.Pc2.iri, ontocape_mathematical_model.Parameter.iri)  
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_model_.QC1MINKey, int(uk_egen_model_.QC1MIN), None, \
                                 ontopowsys_PowerSystemModel.QC1Min.iri, ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_model_.QC2MINKey, int(uk_egen_model_.QC2MIN), None, \
                                 ontopowsys_PowerSystemModel.QC2Min.iri, ontocape_mathematical_model.Parameter.iri)  
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_model_.QC1MAXKey, int(uk_egen_model_.QC1MAX), None, \
                                 ontopowsys_PowerSystemModel.QC1Max.iri, ontocape_mathematical_model.Parameter.iri)
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_model_.QC2MAXKey, int(uk_egen_model_.QC2MAX), None, \
                                 ontopowsys_PowerSystemModel.QC2Max.iri, ontocape_mathematical_model.Parameter.iri)  
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_model_.RAMP_AGCKey, int(uk_egen_model_.RAMP_AGC), None, \
                                 ontopowsys_PowerSystemModel.Rampagc.iri, ontocape_mathematical_model.Parameter.iri)             
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_model_.RAMP_10Key, int(uk_egen_model_.RAMP_10), None, \
                                 ontopowsys_PowerSystemModel.Ramp10.iri, ontocape_mathematical_model.Parameter.iri)     
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_model_.RAMP_30Key, int(uk_egen_model_.RAMP_30), None, \
                                 ontopowsys_PowerSystemModel.Ramp30.iri, ontocape_mathematical_model.Parameter.iri)     
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_model_.RAMP_QKey, int(uk_egen_model_.RAMP_Q), None, \
                                 ontopowsys_PowerSystemModel.Rampq.iri, ontocape_mathematical_model.Parameter.iri)         
        g = AddModelVariable(g, root_node, namespace, node_locator, uk_egen_model_.APFKey, int(uk_egen_model_.APF), None, \
                                 ontopowsys_PowerSystemModel.APF.iri, ontocape_mathematical_model.Parameter.iri)      
               
        # generate/update OWL files
        if updateLocalOWLFile == True: 
            # Store/update the generated owl files      
            if filepath[-2:] != "\\": 
                filepath_ = filepath + "\\" + str(numOfBus) + '_Bus_Model_' + node_locator + OWL
            else:
                filepath_ = filepath + str(numOfBus) + '_Bus_Model_'+ node_locator + OWL
            storeGeneratedOWLs(g, filepath_)

    if isinstance(store, Sleepycat):  
        cg_model_EGen.close()       
    return

def initialiseEGenModelVar(EGen_Model, egen, demand_capa_ratio):
    if not isinstance (EGen_Model, UK_PG.UKEGenModel):
        raise Exception('The first argument should be an instence of UKEGenModel')

    EGen_Model.BUS = egen[6]
    capa = egen[7]
    EGen_Model.PG_INPUT = capa * demand_capa_ratio    
    primaryFuel = egen[8]
    
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
def demandAndCapacityRatioCalculator(EGenInfo, numOfBus, numOfBranch, startTime_of_EnergyConsumption):
    sum_of_capa = 0
    for eg in EGenInfo:
        sum_of_capa += eg[7]
    print('\\\\\sum_of_capa is: ', sum_of_capa)
    total_demand = query_model.queryTotalElecConsumptionofGBOrUK(endpoint_label, numOfBus, numOfBranch, startTime_of_EnergyConsumption) * 1000 / (24 * 365) 
    print('######total_demand:', total_demand)
    demand_capa_ratio = total_demand/sum_of_capa
    print('demand_capa_ratio is: ', demand_capa_ratio)
    return demand_capa_ratio

if __name__ == '__main__':    
    # createModel_EGen('default', False, 2019, "2017-01-31", 10, 14, 50, None, True) 
    createModel_EGen('default', False, 2019, "2017-01-31", 29, 99, 50, None, True)
    print('Terminated')