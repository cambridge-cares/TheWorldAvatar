##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 24 May 2021          #
##########################################

"""This module lists out the SPARQL queries used in generating the UK Grid Model A-boxes"""
import os, sys, json
from rdflib.graph import ConjunctiveGraph
from rdflib.store import NO_STORE
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.queryInterface import performQuery, performUpdate


qres = []
qres_ = []
capa_PrimaryFuel = []
qres_capa = []
allCapacity = []

def queryDigitalTwinLocation(SleepycatPath):
    global qres
    dt_cg = ConjunctiveGraph('Sleepycat')
    sl = dt_cg.open(SleepycatPath, create = False)
    if sl == NO_STORE:
        print('Cannot find the UK Digital Twin Topnode sleepycat store')
        return None
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
    qres = list(dt_cg.query(queryStr))
    dt_cg.close()
    return qres

###############EGen#############
def queryEGenInfo(topology_Endpoint, powerPlant_Endpoint, topoAndConsumpPath_Sleepycat, powerPlant_Sleepycat, localQuery):
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
    
    global qres, capa_PrimaryFuel
    
    if localQuery == False and topology_Endpoint != None and powerPlant_Endpoint != None: 
        print('remoteQuery')
        res = json.loads(performQuery(topology_Endpoint, queryStr_Egen))
        print('query is done')
        qres = [[ str(r['EGen']), str(r['PowerGenerator']), str(r['FixedMO']), str(r['VarMO']), str(r['FuelCost']), str(r['CO2EmissionFactor']), str(r['Bus'])] for r in res]
        
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
            # qres_ = list(pp_cg.query(queryStr_capacityAndPrimaryFuel))  
            res = json.loads(performQuery(powerPlant_Endpoint, queryStr_capacityAndPrimaryFuel))
            print('query is done')
            capa_PrimaryFuel = [[ float(r['Capacity']), (str(r['PrimaryFuel']).split('#'))[1]] for r in res]
        
    elif topoAndConsumpPath_Sleepycat != None and powerPlant_Sleepycat != None and localQuery == True:  
        print('localQuery')
        egen_cg = ConjunctiveGraph('Sleepycat')
        sl = egen_cg.open(topoAndConsumpPath_Sleepycat, create = False)
        if sl == NO_STORE:
            print('Cannot find the UK energy consumption sleepycat store')
            return None
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
    
    if len(qres) == len(capa_PrimaryFuel):
        EGenInfo = [[ str(r[0]), str(r[1]), float(r[2]), float(r[3]), float(r[4]), float(r[5]), str(r[6]), 0 , 0 ] for r in qres]
        counter = 0
        while counter < len(qres) :
            EGenInfo[counter][6] = int((EGenInfo[counter][6]).split('EBus-')[1])
            EGenInfo[counter][7], EGenInfo[counter][8] = capa_PrimaryFuel[counter]
            counter += 1
    else:
        print('The query is failed')
        return None
    return EGenInfo

# def queryEGenInfo(topology_Endpoint, powerPlant_Endpoint, topoAndConsumpPath_Sleepycat, powerPlant_Sleepycat):
#     global qres
#     egen_cg = ConjunctiveGraph('Sleepycat')
#     sl = egen_cg.open(topoAndConsumpPath_Sleepycat, create = False)
#     if sl == NO_STORE:
#         print('Cannot find the UK topology sleepycat store')
#         return None
#     queryStr = """
#     PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
#     PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
#     PREFIX ontopowsys_PowSysPerformance: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#>
#     PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
#     PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
#     PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
#     PREFIX meta_model_topology: <http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>
#     SELECT DISTINCT ?EGen ?PowerGenerator ?FixedMO ?VarMO ?FuelCost ?CO2EmissionFactor ?Bus
#     WHERE
#     {
#     ?EGen rdf:type ontopowsys_PowSysRealization:PowerGenerator .
#     ?EGen ontocape_upper_level_system:isExclusivelySubsystemOf ?PowerGenerator .
    
#     ?EGen ontopowsys_PowSysPerformance:hasFixedMaintenanceCost/ ontocape_upper_level_system:hasValue ?v_FixedMO .
#     ?v_FixedMO ontocape_upper_level_system:numericalValue ?FixedMO .
    
#     ?EGen ontopowsys_PowSysPerformance:hasCost/ ontocape_upper_level_system:hasValue ?v_VarMO .
#     ?v_VarMO ontocape_upper_level_system:numericalValue ?VarMO .
    
#     ?EGen ontopowsys_PowSysPerformance:hasFuelCost/ ontocape_upper_level_system:hasValue ?v_FuelCost .
#     ?v_FuelCost ontocape_upper_level_system:numericalValue ?FuelCost .
    
#     ?EGen ontoeip_powerplant:hasEmissionFactor/ ontocape_upper_level_system:hasValue ?v_CO2EmissionFactor .
#     ?v_CO2EmissionFactor ontocape_upper_level_system:numericalValue ?CO2EmissionFactor .
    
#     ?PowerGeneration_EGen ontoecape_technical_system:isRealizedBy ?EGen .
#     ?PowerGeneration_EGen meta_model_topology:isConnectedTo ?Bus .
#     }
#     """
#     qres = list(egen_cg.query(queryStr))
#     egen_cg.close()
    
    
#     pp_cg = ConjunctiveGraph('Sleepycat')
#     sl = pp_cg.open(powerPlant_Sleepycat, create = False)
#     if sl == NO_STORE:
#         print('Cannot find the UK power plant store')
#         return None
#     for q in qres:
#         PowerGenerator_iri = str(q[1])
#         queryStr_capacityAndPrimaryFuel = """
#         PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
#         PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
#         PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
#         SELECT DISTINCT ?Capacity ?PrimaryFuel
#         WHERE
#         {
#         ?PowerPlant ontoecape_technical_system:hasRealizationAspect <%s> .
#         ?PowerPlant ontoecape_technical_system:hasRequirementsAspect/ontocape_upper_level_system:hasValue ?v_capa .
#         ?v_capa ontocape_upper_level_system:numericalValue ?Capacity .
        
#         <%s> ontoecape_technical_system:realizes/ontoeip_powerplant:consumesPrimaryFuel ?PrimaryFuel.
#         }
#         """ % (PowerGenerator_iri, PowerGenerator_iri)
#         qres_ = list(pp_cg.query(queryStr_capacityAndPrimaryFuel))  
#         capa_PrimaryFuel.append([float(qres_[0][0]), (str(qres_[0][1]).split('#'))[1]])  
#     pp_cg.close()
    
#     if len(qres) == len(capa_PrimaryFuel):
#         EGenInfo = [[ str(r[0]), str(r[1]), float(r[2]), float(r[3]), float(r[4]), float(r[5]), str(r[6]), 0 , 0 ] for r in qres]
#         counter = 0
#         while counter < len(qres) :
#             EGenInfo[counter][6] = int((EGenInfo[counter][6]).split('EBus-')[1])
#             EGenInfo[counter][7], EGenInfo[counter][8] = capa_PrimaryFuel[counter]
#             counter += 1
#     else:
#         print('The query is failed')
#         return None
#     return EGenInfo
    
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
    ?v_TotalELecConsumption ontocape_upper_level_system:numericalValue ?TotalELecConsumption
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
def queryEBusandRegionalDemand(SleepycatPath):
    global qres
    ebus_cg = ConjunctiveGraph('Sleepycat')
    sl = ebus_cg.open(SleepycatPath, create = False)
    if sl == NO_STORE:
        print('Cannot find the UK topology sleepycat store')
        return None
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
    qres = list(ebus_cg.query(queryStr))
    ebus_cg.close()
    return qres  

###############ELine#############
def queryELineTopologicalInformation(topology_Sleepycat):
    global qres, qres_
    eline_cg = ConjunctiveGraph('Sleepycat')
    sl = eline_cg.open(topology_Sleepycat, create = False)
    if sl == NO_STORE:
        print('Cannot find the UK topology sleepycat store')
        return None
    queryStr_busConnectionAndLength = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>    
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX ontocape_geometry: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    SELECT  ?ELine ?From_Bus ?To_Bus ?Value_Length_ELine
    WHERE
    {
    ?ELine rdf:type ontopowsys_PowSysRealization:OverheadLine .
    ?PowerFlow_ELine ontoecape_technical_system:isRealizedBy ?ELine .
    ?PowerFlow_ELine ontocape_network_system:leaves ?From_Bus .
    ?PowerFlow_ELine ontocape_network_system:enters ?To_Bus .
    
    ?ELine ontocape_geometry:hasShapeRepresentation/ontocape_geometry:has_length ?Length_ELine .
    ?Length_ELine ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Value_Length_ELine .
    }
    """
    
    queryStr_parallelBranches = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>    
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    SELECT  ?ELine ?OHL_400or275kV ?Num_OHL_400or275kV
    WHERE
    {
    ?ELine rdf:type ontopowsys_PowSysRealization:OverheadLine .
    ?PowerFlow_ELine ontoecape_technical_system:isRealizedBy ?ELine .
    
    ?ELine ontocape_upper_level_system:isComposedOfSubsystem ?OHL_400or275kV . 
    ?OHL_400or275kV rdf:type ontopowsys_PowSysRealization:OverheadLine .
    ?OHL_400or275kV ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Num_OHL_400or275kV .        
    }
    """
    qres = list(eline_cg.query(queryStr_busConnectionAndLength))
    qres_ = list(eline_cg.query(queryStr_parallelBranches))
    eline_cg.close()
    
    # Arrange qres
    ELineTopoInfo = [[ str(r[0]), str(r[1]), str(r[2]), float(r[3]), 0 , 0 ] for r in qres]
    for el in ELineTopoInfo:
        el[1] = int(el[1].split('EBus-')[1])
        el[2] = int(el[2].split('EBus-')[1])
    
    # Arrange qres_
    paraBranch  = [[ str(r_[0]), str(r_[1]), int(r_[2]), 0 , 0 ] for r_ in qres_]
    elineName = []
    for p in paraBranch:
        if p[0] in elineName:
            counter_1 = elineName.index(p[0])
            counter_2 = paraBranch.index(p) 
            if counter_2 > counter_1:
                paraBranch[counter_1][3] = str(paraBranch[counter_2][1])
                paraBranch[counter_1][4] = int(paraBranch[counter_2][2])
                del paraBranch[counter_2]
            else:
                print('counter_2 should be larger than counter_1')
                return None
        else:
            elineName.append(p[0])
    
    paraBranch_Dict = {pb[0] : [] for pb in paraBranch}
    counter = 0
    for key in paraBranch_Dict.keys():
        if (paraBranch[counter][1].split('#OHL_')[1]).startswith('400') and (paraBranch[counter][3].split('#OHL_')[1]).startswith('275'):
            paraBranch_Dict[key] = [paraBranch[counter][2], paraBranch[counter][4]] 
        elif (paraBranch[counter][1].split('#OHL_')[1]).startswith('275') and (paraBranch[counter][3].split('#OHL_')[1]).startswith('400'):
            paraBranch_Dict[key] = [paraBranch[counter][4], paraBranch[counter][2]] 
    
    # Append data in paraBranch_Dict to ELineTopoInfo
    for el in ELineTopoInfo:
        if el[0] in paraBranch_Dict.keys():
            el[4], el[5] = paraBranch_Dict[el[0]]
        else:
            print ('Key does not match.')
            return None    
    return ELineTopoInfo 

if __name__ == '__main__': 
    sl_path = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Energy_Consumption\\Sleepycat_UKec_UKtopo"
    sl_path_pp = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Plant\\Sleepycat_UKpp"   
    iri = 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EGen-479.owl#EGen-479'    
    res = queryEGenInfo('ukpowergridtopology', 'ukpowerplantkg', None, None, False)
    print (len(res))
    for r in res:
        print(res[7])

