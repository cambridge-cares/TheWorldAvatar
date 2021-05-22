##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 21 May 2021          #
##########################################

"""This module lists out the SPARQL queries used in generating the UK Grid Model A-boxes"""

from rdflib.graph import ConjunctiveGraph
from rdflib.store import NO_STORE

qres = {}
qres_ = {}
qres_capa = {}
allCapacity = []


###############EGen#############
def queryEGen(SleepycatPath):
    global qres
    egen_cg = ConjunctiveGraph('Sleepycat')
    sl = egen_cg.open(SleepycatPath, create = False)
    if sl == NO_STORE:
        print('Cannot find the UK topology sleepycat store')
        return None
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
    SELECT DISTINCT ?EGen
    WHERE
    {
    ?EGen rdf:type ontopowsys_PowSysRealization:PowerGenerator .
    }
    """
    qres = list(egen_cg.query(queryStr))
    egen_cg.close()
    return qres


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


def queryCostFactors(SleepycatPath, iri_EGen):
    global qres
    dt_cg = ConjunctiveGraph('Sleepycat')
    sl = dt_cg.open(SleepycatPath, create = False)
    if sl == NO_STORE:
        print('Cannot find the UK grid topology store')
        return None
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysPerformance: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
    SELECT DISTINCT ?FixedMO ?VarMO ?FuelCost ?CO2EmissionFactor
    WHERE
    {
    <%s> ontopowsys_PowSysPerformance:hasFixedMaintenanceCost/ ontocape_upper_level_system:hasValue ?v_FixedMO .
    ?v_FixedMO ontocape_upper_level_system:numericalValue ?FixedMO .
    
    <%s> ontopowsys_PowSysPerformance:hasCost/ ontocape_upper_level_system:hasValue ?v_VarMO .
    ?v_VarMO ontocape_upper_level_system:numericalValue ?VarMO .
    
    <%s> ontopowsys_PowSysPerformance:hasFuelCost/ ontocape_upper_level_system:hasValue ?v_FuelCost .
    ?v_FuelCost ontocape_upper_level_system:numericalValue ?FuelCost .
    
    <%s> ontoeip_powerplant:hasEmissionFactor/ ontocape_upper_level_system:hasValue ?v_CO2EmissionFactor .
    ?v_CO2EmissionFactor ontocape_upper_level_system:numericalValue ?CO2EmissionFactor .
    }
    """ % (iri_EGen, iri_EGen, iri_EGen, iri_EGen)
    qres = list(dt_cg.query(queryStr))
    dt_cg.close()
    return qres


def queryCapacity(topology_Sleepycat, powerPlant_Sleepycat, iri_EGen):
    global qres
    pg_cg = ConjunctiveGraph('Sleepycat')
    sl = pg_cg.open(topology_Sleepycat, create = False)
    if sl == NO_STORE:
        print('Cannot find the UK grid topology store')
        return None
    queryStr_PowerGenerator = """
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    SELECT DISTINCT ?PowerGenerator
    WHERE
    {
    <%s> ontocape_upper_level_system:isExclusivelySubsystemOf ?PowerGenerator .
    }
    """ % (iri_EGen)
    qres = list(pg_cg.query(queryStr_PowerGenerator))
    pg_cg.close()
    
    pp_cg = ConjunctiveGraph('Sleepycat')
    sl = pp_cg.open(powerPlant_Sleepycat, create = False)
    if sl == NO_STORE:
        print('Cannot find the UK power plant store')
        return None
    queryStr_capacity = """
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    SELECT DISTINCT ?Capacity
    WHERE
    {
    ?PowerPlant ontoecape_technical_system:hasRealizationAspect <%s> .
    ?PowerPlant ontoecape_technical_system:hasRequirementsAspect/ontocape_upper_level_system:hasValue ?v_capa .
    ?v_capa ontocape_upper_level_system:numericalValue ?Capacity .
    }
    """ % (qres[0][0])
    qres = list(pp_cg.query(queryStr_capacity))
    pp_cg.close()
    return float(qres[0][0])

def queryBusNumber(topology_Sleepycat, iri_EGen):
    global qres
    dt_cg = ConjunctiveGraph('Sleepycat')
    sl = dt_cg.open(topology_Sleepycat, create = False)
    if sl == NO_STORE:
        print('Cannot find the UK grid topology store')
        return None
    queryStr = """
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX meta_model_topology: <http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>
    SELECT DISTINCT ?Bus
    WHERE
    {
    ?PowerGeneration_EGen ontoecape_technical_system:isRealizedBy <%s> .
    ?PowerGeneration_EGen meta_model_topology:isConnectedTo ?Bus .
    }
    """ % iri_EGen
    qres = list(dt_cg.query(queryStr))
    dt_cg.close()
    qres = (str(qres[0][0]).split('EBus-'))[1]
    return int(qres)

def queryPrimaryFuel(topology_Sleepycat, powerPlant_Sleepycat, iri_EGen):
    global qres
    pg_cg = ConjunctiveGraph('Sleepycat')
    sl = pg_cg.open(topology_Sleepycat, create = False)
    if sl == NO_STORE:
        print('Cannot find the UK grid topology store')
        return None
    queryStr_PowerGenerator = """
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    SELECT DISTINCT ?PowerGenerator
    WHERE
    {
    <%s> ontocape_upper_level_system:isExclusivelySubsystemOf ?PowerGenerator .
    }
    """ % (iri_EGen)
    qres = list(pg_cg.query(queryStr_PowerGenerator))
    pg_cg.close()
    
    pp_cg = ConjunctiveGraph('Sleepycat')
    sl = pp_cg.open(powerPlant_Sleepycat, create = False)
    if sl == NO_STORE:
        print('Cannot find the UK power plant store')
        return None
    queryStr_capacity = """
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
    SELECT DISTINCT ?PrimaryFuel
    WHERE
    {   
    <%s> ontoecape_technical_system:realizes/ontoeip_powerplant:consumesPrimaryFuel ?PrimaryFuel.
    }
    """ % (qres[0][0])
    
    qres = list(pp_cg.query(queryStr_capacity))
    pp_cg.close()
    qres = (str(qres[0][0]).split('#'))[1]
    return qres

def queryRegionalElecConsumption(consumption_Sleepycat):
    global qres
    dt_cg = ConjunctiveGraph('Sleepycat')
    sl = dt_cg.open(consumption_Sleepycat, create = False)
    if sl == NO_STORE:
        print('Cannot find the UK electricity consumption store')
        return None
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
    qres = list(dt_cg.query(queryStr))
    dt_cg.close()
    regionalConsumption = []
    for con in qres:        
        con_ = float(con[0])
        regionalConsumption.append(con_)        
    return regionalConsumption

def queryAllCapacity(topology_Sleepycat, powerPlant_Sleepycat):
    global qres, qres_capa, allCapacity
    pg_cg = ConjunctiveGraph('Sleepycat')
    sl = pg_cg.open(topology_Sleepycat, create = False)
    if sl == NO_STORE:
        print('Cannot find the UK grid topology store')
        return None
    queryStr_PowerGenerator = """
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
    SELECT DISTINCT ?PowerGenerator
    WHERE
    {
    ?EGen rdf:type ontopowsys_PowSysRealization:PowerGenerator .
    ?EGen ontocape_upper_level_system:isExclusivelySubsystemOf ?PowerGenerator .
    }
    """
    qres = list(pg_cg.query(queryStr_PowerGenerator))
    pg_cg.close()
    
    pp_cg = ConjunctiveGraph('Sleepycat')
    sl = pp_cg.open(powerPlant_Sleepycat, create = False)
    if sl == NO_STORE:
        print('Cannot find the UK power plant store')
        return None
    for PowerGenerator in qres:
        PowerGenerator_iri = str(PowerGenerator[0])
        queryStr_capacity = """
        PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
        PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
        SELECT DISTINCT ?Capacity
        WHERE
        {
        ?PowerPlant ontoecape_technical_system:hasRealizationAspect <%s> .
        ?PowerPlant ontoecape_technical_system:hasRequirementsAspect/ontocape_upper_level_system:hasValue ?v_capa .
        ?v_capa ontocape_upper_level_system:numericalValue ?Capacity .
        }
        """ % (PowerGenerator_iri)
        qres_capa = list(pp_cg.query(queryStr_capacity))  
        allCapacity.append(float(qres_capa[0][0]))  
    pp_cg.close()
    return allCapacity

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
    return qres, qres_  


if __name__ == '__main__': 
    sl_path = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Energy_Consumption\\Sleepycat_UKec_UKtopo"
    sl_path_pp = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Plant\\Sleepycat_UKpp"   
    iri = 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EGen-479.owl#EGen-479'    
    res1, res2 = queryELineTopologicalInformation(sl_path)
    
    print (res1[0], res1[1])
    print (res2[0], res2[1], res2[3])
    print(len(res1), len(res2))
