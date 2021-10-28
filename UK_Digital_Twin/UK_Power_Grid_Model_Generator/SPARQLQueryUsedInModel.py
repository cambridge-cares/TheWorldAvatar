##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 27 Oct 2021          #
##########################################

"""This module lists out the SPARQL queries used in generating the UK Grid Model A-boxes"""
import os, sys, json
from rdflib.graph import ConjunctiveGraph
from rdflib.store import NO_STORE
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.queryInterface import performQuery, performUpdate, performFederatedQuery
# import numpy as np 
from shapely.wkt import loads
from shapely.geometry import mapping
import geojson
import ast


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

def queryEGenInfo(numOfBus, topoAndConsumpPath_Sleepycat, powerPlant_Sleepycat, localQuery, endPoint_label): # endpoints: topology_Endpoint, powerPlant_Endpoint
    label = "_" + str(numOfBus) + "_"  
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
    PREFIX ontopowsys_PowSysPerformance: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX meta_model_topology: <http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontoeip_system_requirement: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_requirement.owl#>
    SELECT DISTINCT ?EGen ?PowerGenerator ?FixedMO ?VarMO ?FuelCost ?CO2EmissionFactor ?Bus ?Capacity ?PrimaryFuel
    WHERE
    {
    ?Topology rdf:type ontocape_network_system:NetworkSystem .
    ?Topology rdfs:label ?label .
    FILTER regex(?label, "%s") .
    ?Topology ontocape_upper_level_system:isComposedOfSubsystem ?PowerGeneration_EGen .
    ?PowerGeneration_EGen rdf:type ontopowsys_PowSysFunction:PowerGeneration .
    ?PowerGeneration_EGen ontoecape_technical_system:isRealizedBy ?EGen .
     
    ?EGen rdf:type ontopowsys_PowSysRealization:PowerGenerator .
    ?EGen ontocape_upper_level_system:isExclusivelySubsystemOf ?PowerGenerator .
    ?PowerGenerator rdf:type ontoeip_powerplant:PowerGenerator .
    
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
    ?PowerPlant ontoecape_technical_system:hasRequirementsAspect ?pp_capa .
    ?pp_capa rdf:type ontoeip_system_requirement:DesignCapacity .
    ?pp_capa ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Capacity .
    
    ?PowerGenerator ontoecape_technical_system:realizes/ontoeip_powerplant:consumesPrimaryFuel ?PrimaryFuel .
    }
    """% label
    
    global qres, capa_PrimaryFuel
    
    if localQuery == False and len(endPoint_label) > 0:
        print('remoteQuery')
        res = json.loads(performQuery(endPoint_label, queryStr))
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
        PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
        PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
        PREFIX meta_model_topology: <http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>
        SELECT DISTINCT ?EGen ?PowerGenerator ?FixedMO ?VarMO ?FuelCost ?CO2EmissionFactor ?Bus
        WHERE
        {
            
        ?Topology rdf:type ontocape_network_system:NetworkSystem .
        ?Topology rdfs:label ?label .
        FILTER regex(?label, "%s") .
        ?Topology ontocape_upper_level_system:isComposedOfSubsystem ?PowerGeneration_EGen .
        ?PowerGeneration_EGen rdf:type ontopowsys_PowSysFunction:PowerGeneration .
        ?PowerGeneration_EGen ontoecape_technical_system:isRealizedBy ?EGen .    
            
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
        """% label
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
def queryRegionalElecConsumption(endPoint_label, numOfBus, startTime_of_EnergyConsumption, consumption_Sleepycat, localQuery):
    queryStr = """
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontoeip_system_function: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_function.owl#>
    PREFIX ontoecape_space_and_time: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#>
    PREFIX ontocape_derived_SI_units: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#>
    PREFIX ontocape_coordinate_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#>
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    SELECT DISTINCT  ?v_TotalELecConsumption
    WHERE 
    {   
    ?Topology rdf:type ontocape_network_system:NetworkSystem .
    ?Topology rdfs:label ?label .
    FILTER regex(?label, "%s") .
    ?Topology ontocape_upper_level_system:isComposedOfSubsystem ?Bus_node .
    ?Bus_node rdf:type ontopowsys_PowSysFunction:PowerEquipmentConnection .
    ?Bus_node ontocape_upper_level_system:hasAddress ?RegionName .
    ?RegionName rdf:type <https://dbpedia.org/ontology/Region> .   
    
    ?Region ontoeip_system_function:consumes ?Total_ele_consumption . 
    ?Total_ele_consumption ontocape_derived_SI_units:hasTimePeriod/ontocape_upper_level_system:hasValue ?TimePeriod .
    ?TimePeriod ontoecape_space_and_time:hasStartingTime ?startTime .
    ?startTime rdf:type ontocape_coordinate_system:CoordinateValue . 
    ?startTime ontocape_upper_level_system:numericalValue "%s"^^xsd:dateTime .
    
    ?Region ontocape_upper_level_system:hasAddress ?RegionName .
    ?Total_ele_consumption ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?v_TotalELecConsumption .
    }
    """% (numOfBus, startTime_of_EnergyConsumption)
    global qres
    
    if localQuery == False and endPoint_label != None: 
        print('remoteQuery')
        res = json.loads(performQuery(endPoint_label, queryStr))
        print('query is done')
        regionalConsumption = []
        for r in res: 
            regionalConsumption.append(float(r['v_TotalELecConsumption']))           
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
def queryEBusandRegionalDemand(numOfBus, topo_Consumption_SleepycatPath, localQuery, endPoint_label):
    label = "_" + str(numOfBus) + "_"  
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoeip_system_function: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_function.owl#>
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    SELECT DISTINCT ?EBus ?TotalELecConsumption
    WHERE
    {
    ?Topology rdf:type ontocape_network_system:NetworkSystem .
    ?Topology rdfs:label ?label .
    FILTER regex(?label, "%s") .
    ?Topology ontocape_upper_level_system:isComposedOfSubsystem ?EquipmentConnection_EBus .
    ?EquipmentConnection_EBus rdf:type ontopowsys_PowSysFunction:PowerEquipmentConnection .
    ?EquipmentConnection_EBus ontocape_upper_level_system:hasAddress ?Location_region . 
    
    ?EquipmentConnection_EBus ontoecape_technical_system:isRealizedBy ?EBus . 
    ?EquipmentConnection_EBus ontocape_upper_level_system:hasAddress ?Location .
    ?Location rdf:type <https://dbpedia.org/ontology/Region> .
    
    ?regionalConsumption ontocape_upper_level_system:hasAddress ?Location .
    ?regionalConsumption ontoeip_system_function:consumes/ontocape_upper_level_system:hasValue ?v_TotalELecConsumption .   
    ?v_TotalELecConsumption ontocape_upper_level_system:numericalValue ?TotalELecConsumption .
    }
    """ % label  
    global qres
    if localQuery == False and len(endPoint_label) > 0:
       print('remoteQuery')     
       res = json.loads(performQuery(endPoint_label, queryStr))            
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


# Query the total consumption of the regions
def queryElectricityConsumption_Region(startTime_of_EnergyConsumption, topo_Consumption_SleepycatPath, localQuery, endPoint_label):
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoeip_system_function: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_function.owl#>
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX ontocape_derived_SI_units: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#>
    PREFIX ontoecape_space_and_time: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#>
	PREFIX ontocape_coordinate_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#>
    SELECT DISTINCT  ?Region ?TotalELecConsumption 
    WHERE
    {
    
    ?Region rdf:type <https://dbpedia.org/ontology/Region> .    
    ?place ontocape_upper_level_system:hasAddress ?Region .
    ?place ontoeip_system_function:consumes ?regionalConsumption . 
    ?regionalConsumption ontocape_derived_SI_units:hasTimePeriod/ontocape_upper_level_system:hasValue ?TimePeriod .
    ?TimePeriod ontoecape_space_and_time:hasStartingTime ?startTime .
    ?startTime rdf:type ontocape_coordinate_system:CoordinateValue . 
    ?startTime ontocape_upper_level_system:numericalValue "%s"^^xsd:dateTime .
    ?regionalConsumption ontocape_upper_level_system:hasValue ?v_TotalELecConsumption .   
    ?v_TotalELecConsumption ontocape_upper_level_system:numericalValue ?TotalELecConsumption .
    }
    """ % startTime_of_EnergyConsumption  
    global qres
    if localQuery == False and len(endPoint_label) > 0:
       print('remoteQuery')     
       res = json.loads(performQuery(endPoint_label, queryStr)) 
       return res            
       # qres = [[ str(r['Region']), float((r['TotalELecConsumption'].split('\"^^')[0]).replace('\"',''))] for r in res]
    elif topo_Consumption_SleepycatPath != None and localQuery == True:  
        ebus_cg = ConjunctiveGraph('Sleepycat')
        sl = ebus_cg.open(topo_Consumption_SleepycatPath, create = False)
        if sl == NO_STORE:
            raise Exception('Cannot find the UK topology sleepycat store')
                 
        qres = list(ebus_cg.query(queryStr))
        ebus_cg.close()
    return qres 
         
# query the total electricity consumption of each address area
def queryElectricityConsumption_LocalArea(startTime_of_EnergyConsumption, ukdigitaltwin_iri, ONS):    
    queryStr = """  
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX bibtex: <https://zeitkunst.org/bibtex/0.2/bibtex.owl#> 
    PREFIX ont: <http://www.opengis.net/ont/geosparql#>
    PREFIX ont_sparql: <http://www.opengis.net/ont/geosparql#>
    PREFIX db: <https://dbpedia.org/ontology/>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoeip_system_function: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_function.owl#>
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX ontocape_derived_SI_units: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#>
    PREFIX ontoecape_space_and_time: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#>
	PREFIX ontocape_coordinate_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#>
    SELECT DISTINCT  ?Area_LACode ?TotalELecConsumption (GROUP_CONCAT(?Geo_Info;SEPARATOR = '***') AS ?Geo_InfoList)
    WHERE
    {   
    ?AddressArea rdf:type <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#AddressArea> .
    ?AddressArea bibtex:hasURL ?Area_id_url .   
    ?AddressArea db:areaCode ?Area_LACode .
    ?place ontocape_upper_level_system:hasAddress ?AddressArea .
    ?place ontoeip_system_function:consumes ?regionalConsumption . 
    ?regionalConsumption ontocape_derived_SI_units:hasTimePeriod/ontocape_upper_level_system:hasValue ?TimePeriod .
    ?TimePeriod ontoecape_space_and_time:hasStartingTime ?startTime .
    ?startTime rdf:type ontocape_coordinate_system:CoordinateValue . 
    ?startTime ontocape_upper_level_system:numericalValue "%s"^^xsd:dateTime .
    ?regionalConsumption ontocape_upper_level_system:hasValue ?v_TotalELecConsumption .   
    ?v_TotalELecConsumption ontocape_upper_level_system:numericalValue ?TotalELecConsumption .
    
    OPTIONAL { ?Area_id_url a <http://statistics.data.gov.uk/def/statistical-geography#Statistical-Geography> .
    ?Area_id_url ont:hasGeometry ?geometry . 
    ?geometry ont_sparql:asWKT ?Geo_Info . }      
    
    } GROUP BY ?TotalELecConsumption ?Area_LACode
    """ % startTime_of_EnergyConsumption  
      
    res = json.loads(performFederatedQuery(queryStr, ukdigitaltwin_iri, ONS)) 
    for r in res:
      for key in r.keys():
          if '\"^^' in  r[key] :
            r[key] = (r[key].split('\"^^')[0]).replace('\"','') 
       
    for r in res: 
        if len(r["Geo_InfoList"]) == 0:
            raise Exception(r["Area_id_url"], "does't have the geographical attributes.")
        elif "***" in r['Geo_InfoList']:
            r['Geo_InfoList'] = r['Geo_InfoList'].split("***")[0]            
        r['Geo_InfoList'] = loads(r['Geo_InfoList'])
    
    return res            
    

###############ELine#############
# branchGeometryQueryCreator is developed to constuct a query string used to retrieve the branch's geometry information according to its parallel connection of each branch
def branchGeometryQueryCreator(label, branch_voltage_level): 
    PREFIX = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>    
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX ontocape_geometry: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    """
    
    SELECT_CLAUSE = """
    SELECT DISTINCT ?ELine ?From_Bus ?To_Bus ?Value_Length_ELine """
    
    for voltage in branch_voltage_level:
       SELECT_CLAUSE += "?Num_OHL_" + str(voltage) + " "
        
    WHERE_CLAUSE = """
    WHERE
    {
    ?Topology rdfs:label ?label .
    FILTER regex(?label, "%s") .
    ?Topology ontocape_upper_level_system:isComposedOfSubsystem ?PowerFlow_ELine .
    ?PowerFlow_ELine rdf:type ontopowsys_PowSysFunction:PowerFlow .   
    ?PowerFlow_ELine ontoecape_technical_system:isRealizedBy ?ELine .
    ?ELine rdf:type ontopowsys_PowSysRealization:OverheadLine .
    ?PowerFlow_ELine ontocape_network_system:leaves ?From_Bus .
    ?PowerFlow_ELine ontocape_network_system:enters ?To_Bus .

    ?ELine ontocape_geometry:hasShapeRepresentation/ontocape_geometry:has_length ?Length_ELine .
    ?Length_ELine ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Value_Length_ELine .

    """% label 
    
    for voltage in branch_voltage_level: 
        OHL = "?OHL_" + str(voltage)
        Num_OHL = "?Num_OHL_" + str(voltage)
        
        WHERE_CLAUSE += """?ELine ontocape_upper_level_system:isComposedOfSubsystem %s . 
    %s rdf:type ontopowsys_PowSysRealization:OverheadLine .
    %s ontopowsys_PowSysRealization:hasVoltageLevel "%s" .
    %s ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .    
    """% (OHL, OHL, OHL, voltage, OHL, Num_OHL)
    WHERE_CLAUSE += "} ORDER BY ASC(?ELine)"
        
    queryStr =  PREFIX + SELECT_CLAUSE + WHERE_CLAUSE
    return queryStr    
    
# queryELineTopologicalInformation is developed to perform the query for branch topological information and its geometry information
def queryELineTopologicalInformation(numOfBus, ukdigitaltwin_endpointlabel, topology_Sleepycat, localQuery):
    label = "_" + str(numOfBus) + "_"  
    
    query_branch_voltage_level = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>    
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX ontocape_geometry: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    SELECT DISTINCT ?OHL_voltage_level
    WHERE
    {
    ?Topology rdf:type ontocape_network_system:NetworkSystem .
    ?Topology rdfs:label ?label .
    FILTER regex(?label, "%s") .
    ?Topology ontocape_upper_level_system:isComposedOfSubsystem ?PowerFlow_ELine .
    ?PowerFlow_ELine rdf:type ontopowsys_PowSysFunction:PowerFlow .   
    ?PowerFlow_ELine ontoecape_technical_system:isRealizedBy ?ELine .
    ?ELine rdf:type ontopowsys_PowSysRealization:OverheadLine .
       
    ?ELine ontocape_upper_level_system:isComposedOfSubsystem ?OHL . 
    ?OHL rdf:type ontopowsys_PowSysRealization:OverheadLine .
    ?OHL ontopowsys_PowSysRealization:hasVoltageLevel ?OHL_voltage_level .   
    }
    """% label
    
    global qres 
    
    if localQuery == False and ukdigitaltwin_endpointlabel != None: 
        print('Query the branch_voltage_level')
        res = json.loads(performQuery(ukdigitaltwin_endpointlabel, query_branch_voltage_level))
        print('Branch_voltage_level query is done')
        branch_voltage_level =  [str(r['OHL_voltage_level']) for r in res]
        queryStr = branchGeometryQueryCreator(label, branch_voltage_level)
        print('Query Branch Geometry')
        res = json.loads(performQuery(ukdigitaltwin_endpointlabel, queryStr))
        print('branchGeometryQuery is done')
        for r in res:
          for key in r.keys():
              if '\"^^' in  r[key] :
                r[key] = (r[key].split('\"^^')[0]).replace('\"','') 
        return res, branch_voltage_level 
    elif topology_Sleepycat != None and localQuery == True: 
        print('*************WARNING: Local Query is specified, the return is a list instead of a dictionary.*************')       
        eline_cg = ConjunctiveGraph('Sleepycat')
        sl = eline_cg.open(topology_Sleepycat, create = False)
        if sl == NO_STORE:
            print('Cannot find the UK topology sleepycat store')
            return []
        branch_voltage_level = list(eline_cg.query(query_branch_voltage_level))
        queryStr = branchGeometryQueryCreator(label, branch_voltage_level)
        qres = list(eline_cg.query(queryStr))  
        eline_cg.close()
        return qres, branch_voltage_level 


if __name__ == '__main__': 
    # sl_path = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Energy_Consumption\\Sleepycat_UKec_UKtopo"
    # sl_path_pp = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Plant\\Sleepycat_UKpp"   
    # iri = 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EGen-479.owl#EGen-479'   
    ONS_json = "http://statistics.data.gov.uk/sparql.json"
    ukdigitaltwinendpoint = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin/sparql"
    # res = queryEGenInfo('ukpowergridtopology', 'ukpowerplantkg', None, None, False)
    # res = queryRegionalElecConsumption('ukdigitaltwin', 10, "2017-01-31", None, False)
    # res = queryElectricityConsumption_Region("2017-01-31", None, False, 'ukdigitaltwin')
    # res = queryElectricityConsumption_LocalArea("2017-01-31", ukdigitaltwinendpoint, ONS_json)
    res, a = queryELineTopologicalInformation(29, 'ukdigitaltwin', None, False)
    # res = branchGeometryQueryCreator('10', ['275kV', '400kV'])
    # res = queryEGenInfo(None, None, False, "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridTopology", "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerPlantKG" )
    # print (res[0])
    # SleepycatStoragePath = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\Top_node\\Sleepycat_topnode"
    # res = queryDigitalTwinLocation(None, SleepycatStoragePath, True)
    # res = queryEBusandRegionalDemand(10, None, False, "ukdigitaltwin")
    # geo = res[0]['Geo_InfoList']
    #print(geo.geom_type)    
    # print(res)
    for r in res:
        print(r['ELine'])
    

