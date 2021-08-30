##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 24 June 2021         #
##########################################

"""This module lists out the SPARQL queries used in generating the UK Grid Model A-boxes"""
import os, sys, json
from rdflib.graph import ConjunctiveGraph
from rdflib.store import NO_STORE
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.queryInterface import performQuery, performUpdate, performFederatedQuery


qres = []
qres_ = []
capa_PrimaryFuel = []
qres_capa = []
allCapacity = []

###############EGen#############
# Query the located country of the digital twin
def queryDigitalTwinLocation(ukdigitaltwin_Endpoint, SleepycatPath, localQuery):
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    SELECT DISTINCT ?Location
    WHERE
    {
    ?DigitalTwin rdf:type ontocape_upper_level_system:TopLevelSystem .
    ?DigitalTwin ontocape_upper_level_system:hasAddress ?Location .
    }
    """
    global qres
    if localQuery == False and ukdigitaltwin_Endpoint != None: 
        print('remoteQuery')
        res = json.loads(performQuery(ukdigitaltwin_Endpoint, queryStr))
        qres = res[0]['Location']
    elif SleepycatPath != None and localQuery == True:
        dt_cg = ConjunctiveGraph('Sleepycat')
        sl = dt_cg.open(SleepycatPath, create = False)
        if sl == NO_STORE:
            print('Cannot find the UK Digital Twin Topnode sleepycat store')
            return None        
        qres = (list(dt_cg.query(queryStr)))[0][0]
        dt_cg.close()
    return qres

# queryStr_Egen: Query the model EGen and its corresponding power plant iri as well as its cost factors (fixed m&o, varialbe m&o, fuel cost), CO2 emission factor and its connected bus
# queryStr_capacityAndPrimaryFuel: query the capacity and primary fuel of the EGen's corresponding power plant.  

def queryEGenInfo(topoAndConsumpPath_Sleepycat, powerPlant_Sleepycat, localQuery, *endPoints): # endpoints: topology_Endpoint, powerPlant_Endpoint
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
    PREFIX ontopowsys_PowSysPerformance: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX meta_model_topology: <http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>
    SELECT DISTINCT ?EGen ?PowerGenerator ?FixedMO ?VarMO ?FuelCost ?CO2EmissionFactor ?Bus ?Capacity ?PrimaryFuel
    WHERE
    {
    ?EGen rdf:type ontopowsys_PowSysRealization:PowerGenerator .
    ?EGen ontocape_upper_level_system:isExclusivelySubsystemOf ?PowerGenerator .
    
    ?EGen ontopowsys_PowSysPerformance:hasFixedMaintenanceCost/ ontocape_upper_level_system:hasValue ?v_FixedMO .
    ?v_FixedMO ontocape_upper_level_system:numericalValue ?FixedMO .
    
    ?EGen ontopowsys_PowSysPerformance:hasCost/ ontocape_upper_level_system:hasValue ?v_VarMO .
    ?v_VarMO ontocape_upper_level_system:numericalValue ?VarMO .
    
    ?EGen ontopowsys_PowSysPerformance:hasFuelCost/ ontocape_upper_level_system:hasValue ?v_FuelCost .
    ?v_FuelCost ontocape_upper_level_system:numericalValue ?FuelCost .
    
    ?EGen ontoeip_powerplant:hasEmissionFactor/ ontocape_upper_level_system:hasValue ?v_CO2EmissionFactor .
    ?v_CO2EmissionFactor ontocape_upper_level_system:numericalValue ?CO2EmissionFactor .
    
    ?PowerGeneration_EGen ontoecape_technical_system:isRealizedBy ?EGen .
    ?PowerGeneration_EGen meta_model_topology:isConnectedTo ?Bus .
    
    ?PowerPlant ontoecape_technical_system:hasRealizationAspect ?PowerGenerator .
    ?PowerPlant ontoecape_technical_system:hasRequirementsAspect/ontocape_upper_level_system:hasValue ?v_capa .
    ?v_capa ontocape_upper_level_system:numericalValue ?Capacity .
    
    ?PowerGenerator ontoecape_technical_system:realizes/ontoeip_powerplant:consumesPrimaryFuel ?PrimaryFuel .
    }
    """
    
    global qres, capa_PrimaryFuel
    
    if localQuery == False and len(endPoints) > 0:
        print('remoteQuery')
        res = json.loads(performFederatedQuery(queryStr, *endPoints))
        qres = [[ str(r['EGen']), str(r['PowerGenerator']), float((r['FixedMO'].split('\"^^')[0]).replace('\"','')), float((r['VarMO'].split('\"^^')[0]).replace('\"','')), \
                 float((r['FuelCost'].split('\"^^')[0]).replace('\"','')), float((r['CO2EmissionFactor'].split('\"^^')[0]).replace('\"','')), int(r['Bus'].split('EBus-')[1]), \
                 float((r['Capacity'].split('\"^^')[0]).replace('\"','')), (str(r['PrimaryFuel']).split('#'))[1]] for r in res]
        return qres
            
    elif topoAndConsumpPath_Sleepycat != None and powerPlant_Sleepycat != None and localQuery == True:  
        print('localQuery')
        egen_cg = ConjunctiveGraph('Sleepycat')
        sl = egen_cg.open(topoAndConsumpPath_Sleepycat, create = False)
        if sl == NO_STORE:
            print('Cannot find the UK energy consumption sleepycat store')
            return None
        queryStr_Egen = """
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
        PREFIX ontopowsys_PowSysPerformance: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#>
        PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
        PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
        PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
        PREFIX meta_model_topology: <http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>
        SELECT DISTINCT ?EGen ?PowerGenerator ?FixedMO ?VarMO ?FuelCost ?CO2EmissionFactor ?Bus
        WHERE
        {
        ?EGen rdf:type ontopowsys_PowSysRealization:PowerGenerator .
        ?EGen ontocape_upper_level_system:isExclusivelySubsystemOf ?PowerGenerator .
        
        ?EGen ontopowsys_PowSysPerformance:hasFixedMaintenanceCost/ ontocape_upper_level_system:hasValue ?v_FixedMO .
        ?v_FixedMO ontocape_upper_level_system:numericalValue ?FixedMO .
        
        ?EGen ontopowsys_PowSysPerformance:hasCost/ ontocape_upper_level_system:hasValue ?v_VarMO .
        ?v_VarMO ontocape_upper_level_system:numericalValue ?VarMO .
        
        ?EGen ontopowsys_PowSysPerformance:hasFuelCost/ ontocape_upper_level_system:hasValue ?v_FuelCost .
        ?v_FuelCost ontocape_upper_level_system:numericalValue ?FuelCost .
        
        ?EGen ontoeip_powerplant:hasEmissionFactor/ ontocape_upper_level_system:hasValue ?v_CO2EmissionFactor .
        ?v_CO2EmissionFactor ontocape_upper_level_system:numericalValue ?CO2EmissionFactor .
        
        ?PowerGeneration_EGen ontoecape_technical_system:isRealizedBy ?EGen .
        ?PowerGeneration_EGen meta_model_topology:isConnectedTo ?Bus .
        }
        """
        qres = list(egen_cg.query(queryStr_Egen))
        egen_cg.close()
        
        pp_cg = ConjunctiveGraph('Sleepycat')
        sl = pp_cg.open(powerPlant_Sleepycat, create = False)
        if sl == NO_STORE:
            print('Cannot find the UK power plant store')
            return None
        for q in qres:
            PowerGenerator_iri = str(q[1])
            queryStr_capacityAndPrimaryFuel = """
            PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
            PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
            PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
            SELECT DISTINCT ?Capacity ?PrimaryFuel
            WHERE
            {
            ?PowerPlant ontoecape_technical_system:hasRealizationAspect <%s> .
            ?PowerPlant ontoecape_technical_system:hasRequirementsAspect/ontocape_upper_level_system:hasValue ?v_capa .
            ?v_capa ontocape_upper_level_system:numericalValue ?Capacity .
            
            <%s> ontoecape_technical_system:realizes/ontoeip_powerplant:consumesPrimaryFuel ?PrimaryFuel.
            }
            """ % (PowerGenerator_iri, PowerGenerator_iri)
            qres_ = list(pp_cg.query(queryStr_capacityAndPrimaryFuel))  
            capa_PrimaryFuel.append([float(qres_[0][0]), (str(qres_[0][1]).split('#'))[1]])  
        pp_cg.close()
        
        print(len(qres), len(capa_PrimaryFuel))
        if len(qres) == len(capa_PrimaryFuel):
            EGenInfo = [[ str(r[0]), str(r[1]), float(r[2]), float(r[3]), float(r[4]), float(r[5]), str(r[6]), 0 , 0 ] for r in qres]
            counter = 0
            while counter < len(qres) :
                EGenInfo[counter][6] = int((EGenInfo[counter][6]).split('EBus-')[1])
                EGenInfo[counter][7], EGenInfo[counter][8] = capa_PrimaryFuel[counter]
                counter += 1
            return EGenInfo
        else:
            print('The query is failed')
            return None      

# query the total electricity consumption of a UK official region 
def queryRegionalElecConsumption(energyConsumption_Endpoint, consumption_Sleepycat, localQuery):
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontoeip_system_function: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_function.owl#>
    SELECT DISTINCT ?TotalELecConsumption
    WHERE
    {
    ?Region ontocape_upper_level_system:hasAddress/rdf:type <https://dbpedia.org/ontology/Region> .
    ?Region ontoeip_system_function:consumes/ontocape_upper_level_system:hasValue ?v_TotalELecConsumption .   
    ?v_TotalELecConsumption ontocape_upper_level_system:numericalValue ?TotalELecConsumption .
    }
    """
    global qres
    
    if localQuery == False and energyConsumption_Endpoint != None: 
        print('remoteQuery')
        res = json.loads(performQuery(energyConsumption_Endpoint, queryStr))
        print('query is done')
        regionalConsumption = []
        for r in res: 
            regionalConsumption.append(float(r['TotalELecConsumption']))           
    elif consumption_Sleepycat != None and localQuery == True:  
        dt_cg = ConjunctiveGraph('Sleepycat')
        sl = dt_cg.open(consumption_Sleepycat, create = False)
        if sl == NO_STORE:
            print('Cannot find the UK electricity consumption store')
            return None
        qres = list(dt_cg.query(queryStr))
        dt_cg.close()
        regionalConsumption = []
        for con in qres:        
            con_ = float(con[0])
            regionalConsumption.append(con_)        
    return regionalConsumption   

###############EBus#############
# query the EBus iri and its located area's total electricity consumption 
def queryEBusandRegionalDemand(topo_Consumption_SleepycatPath, localQuery, *endPoints): # endPoints: topology_Endpoint, energyConsumption_Endpoint
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoeip_system_function: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_function.owl#>
    SELECT DISTINCT ?EBus ?TotalELecConsumption
    WHERE
    {
    ?EquipmentConnection_EBus rdf:type ontopowsys_PowSysFunction:PowerEquipmentConnection .
    ?EquipmentConnection_EBus ontoecape_technical_system:isRealizedBy ?EBus . 
    ?EquipmentConnection_EBus ontocape_upper_level_system:hasAddress ?Location .
    ?Location rdf:type <https://dbpedia.org/ontology/Region> .
    
    ?regionalConsumption ontocape_upper_level_system:hasAddress ?Location .
    ?regionalConsumption ontoeip_system_function:consumes/ontocape_upper_level_system:hasValue ?v_TotalELecConsumption .   
    ?v_TotalELecConsumption ontocape_upper_level_system:numericalValue ?TotalELecConsumption .
    }
    """   
    global qres
    if localQuery == False and len(endPoints) > 0:
       print('remoteQuery')
       res = json.loads(performFederatedQuery(queryStr, *endPoints))
       qres = [[ str(r['EBus']), float((r['TotalELecConsumption'].split('\"^^')[0]).replace('\"',''))] for r in res]
    elif topo_Consumption_SleepycatPath != None and localQuery == True:  
        ebus_cg = ConjunctiveGraph('Sleepycat')
        sl = ebus_cg.open(topo_Consumption_SleepycatPath, create = False)
        if sl == NO_STORE:
            print('Cannot find the UK topology sleepycat store')
            return None        
        qres = list(ebus_cg.query(queryStr))
        ebus_cg.close()
    return qres      

###############ELine#############
# queryStr_busConnectionAndLength: query ELine iri and its From_Bus, To_Bus and the Length_ELine.
# queryStr_parallelBranches: the number of OHL_400 and 275kV of an ELine
def queryELineTopologicalInformation(topology_Endpoint, topology_Sleepycat, localQuery):
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>    
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX ontocape_geometry: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    SELECT  ?ELine ?From_Bus ?To_Bus ?Value_Length_ELine ?Num_OHL_400kV ?Num_OHL_275kV
    WHERE
    {
    ?ELine rdf:type ontopowsys_PowSysRealization:OverheadLine .
    ?PowerFlow_ELine ontoecape_technical_system:isRealizedBy ?ELine .
    ?PowerFlow_ELine ontocape_network_system:leaves ?From_Bus .
    ?PowerFlow_ELine ontocape_network_system:enters ?To_Bus .
    
    ?ELine ontocape_geometry:hasShapeRepresentation/ontocape_geometry:has_length ?Length_ELine .
    ?Length_ELine ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Value_Length_ELine .
    
    ?ELine ontocape_upper_level_system:isComposedOfSubsystem ?OHL_400kV . 
    ?OHL_400kV rdf:type ontopowsys_PowSysRealization:OverheadLine .
    ?OHL_400kV ontopowsys_PowSysRealization:hasVoltageLevel "400kV" .
    ?OHL_400kV ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Num_OHL_400kV .    

    ?ELine ontocape_upper_level_system:isComposedOfSubsystem ?OHL_275kV . 
    ?OHL_275kV rdf:type ontopowsys_PowSysRealization:OverheadLine .
    ?OHL_275kV ontopowsys_PowSysRealization:hasVoltageLevel "275kV" .
    ?OHL_275kV ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Num_OHL_275kV .       
    }
    """
    
    global qres 
    
    if localQuery == False and topology_Endpoint != None: 
        print('remoteQuery')
        res = json.loads(performQuery(topology_Endpoint, queryStr))
        print('query is done')
        qres = [[ str(r['ELine']), int(r['From_Bus'].split('EBus-')[1]), int(r['To_Bus'].split('EBus-')[1]), \
                 float((r['Value_Length_ELine'].split('\"^^')[0]).replace('\"','')), int((r['Num_OHL_400kV'].split('\"^^')[0]).replace('\"','')),\
                     int((r['Num_OHL_275kV'].split('\"^^')[0]).replace('\"',''))] for r in res]
    elif topology_Sleepycat != None and localQuery == True:  
        eline_cg = ConjunctiveGraph('Sleepycat')
        sl = eline_cg.open(topology_Sleepycat, create = False)
        if sl == NO_STORE:
            print('Cannot find the UK topology sleepycat store')
            return None
        qres = list(eline_cg.query(queryStr))
        eline_cg.close()
    return qres 

def testLabel():
    qstr = """
    PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> 
    PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> 
    PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> 
    PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#> 
    PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#> 
    SELECT DISTINCT ?Model_EGen ?b_value 
    WHERE
    {
    ?Model_EGen   j5:hasModelVariable ?b .
	?b  a  j3:genCostcn-2  .
	?b  rdfs:label  "Parameter_b"  .
	?b  j2:hasValue ?v_bcostn2 .
	?v_bcostn2   j2:numericalValue ?b_value .
    }
    
    """
    res = json.loads(performQuery('ukpowergridmodel', qstr))
    print(len(res))
    print(res)
    return

if __name__ == '__main__': 
    # sl_path = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Energy_Consumption\\Sleepycat_UKec_UKtopo"
    # sl_path_pp = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Plant\\Sleepycat_UKpp"   
    # iri = 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EGen-479.owl#EGen-479'    
    # res = queryEGenInfo('ukpowergridtopology', 'ukpowerplantkg', None, None, False)
    # res = queryRegionalElecConsumption('ukenergyconsumptionkg', None, False)
    res = queryELineTopologicalInformation('ukpowergridtopology', None, False)
    print (res)
    
    # res = queryEGenInfo(None, None, False, "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridTopology", "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerPlantKG" )
    # print (res[0])
    # SleepycatStoragePath = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\Top_node\\Sleepycat_topnode"
    # res = queryDigitalTwinLocation(None, SleepycatStoragePath, True)
    # res = queryEBusandRegionalDemand(None, False, "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridTopology", "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKEnergyConsumptionKG")
    # print(res)
    # for r in res:
    #     print(res)
    #testLabel()

