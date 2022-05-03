# The purpose of this module is to group certain tasks you
# wish to perform on the KG
#============================================================
# get the jpsBaseLibGW instance from the jpsSingletons module
# For further information, please check the jps-base_lib, py4jps

###############################################
# Extended by: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 24 June 2021              #
###############################################

import os, sys
BASE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, BASE)
#from jpsSingletons import jpsBaseLibGW
from py4jps.resources import JpsBaseLib
jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()

# create a JVM module view and use it to import the required java classes
jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

def performQuery(kb, query, isQuery = True, isUpdate = False):
    # perform an example sparqle query, see the jps-base-lib docs for further details
    KGRouter = jpsBaseLib_view.StoreRouter
    KGClient = KGRouter.getStoreClient(KGRouter.HTTP_KB_PREFIX + kb, isQuery, isUpdate)
    try:
        response = KGClient.executeQuery((query))
        return str(response)
    except:
        print("***WARNING:KGClient has not been created successfully.****")

def performUpdate(kb, query, isQuery = True, isUpdate = True):
    # perform an example sparqle query, see the jps-base-lib docs for further details
    KGRouter = jpsBaseLib_view.StoreRouter 
    KGClient = KGRouter.getStt(str(KGRouter.HTTP_KB_PREFIX) + str(kb), isQuery, isUpdate)
    try:
        response = KGClient.executeUpdate((query))
        return str(response)
    except:
        print("KGClient has not been created successfully.")
  
def performFederatedQuery(query, queryendpoints:list):
    # perform an example sparqle query, see the jps-base-lib docs for further details   
    RemoteKnowledgeBaseClient = jpsBaseLib_view.RemoteStoreClient()
    try: 
        response = RemoteKnowledgeBaseClient.executeFederatedQuery(list(queryendpoints), query)
        return str(response)
    except:
        print("***WARNING:RemoteKnowledgeBaseClient has not been created successfully***")


if __name__ == '__main__':  

    queryStr_topo = """
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
    SELECT DISTINCT ?PowerGenerator ?FixedMO ?VarMO ?FuelCost ?CO2EmissionFactor ?Bus ?Capacity ?PrimaryFuel
    WHERE
    {
    <http://www.theworldavatar.com/kb/ontoenergysystem/PowerGridTopology_7ea91c81-9f7f-4d27-9b75-9b897171bbc4> ontocape_upper_level_system:isComposedOfSubsystem ?PowerGenerator . 
    ?PowerGenerator rdf:type ontoeip_powerplant:PowerGenerator . 
    
    ?PowerGenerator ontopowsys_PowSysPerformance:hasFixedMaintenanceCost/ ontocape_upper_level_system:hasValue ?v_FixedMO .
    ?v_FixedMO ontocape_upper_level_system:numericalValue ?FixedMO .
    
    ?PowerGenerator ontopowsys_PowSysPerformance:hasCost/ ontocape_upper_level_system:hasValue ?v_VarMO .
    ?v_VarMO ontocape_upper_level_system:numericalValue ?VarMO .
    
    ?PowerGenerator ontopowsys_PowSysPerformance:hasFuelCost/ ontocape_upper_level_system:hasValue ?v_FuelCost .
    ?v_FuelCost ontocape_upper_level_system:numericalValue ?FuelCost .
    
    ?PowerGenerator ontoeip_powerplant:hasEmissionFactor/ ontocape_upper_level_system:hasValue ?v_CO2EmissionFactor .
    ?v_CO2EmissionFactor ontocape_upper_level_system:numericalValue ?CO2EmissionFactor .
    
    ?PowerGenerator meta_model_topology:hasOutput ?Bus .
    ?Bus rdf:type ontopowsys_PowSysRealization:BusNode .  
    
    ?PowerPlant ontoecape_technical_system:hasRealizationAspect ?PowerGenerator .
    ?PowerPlant ontoecape_technical_system:hasRequirementsAspect ?pp_capa .
    ?pp_capa rdf:type ontoeip_system_requirement:DesignCapacity .
    ?pp_capa ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Capacity .
    
    ?PowerGenerator ontoecape_technical_system:realizes/ontoeip_powerplant:consumesPrimaryFuel ?PrimaryFuel .
    }
    """

    ukdigitaltwinendpoint = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test2/sparql"
    # res = performFederatedQuery(queryStr1, ["https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridTopology", "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKEnergyConsumptionKG"])
    ONS_json = "http://statistics.data.gov.uk/sparql.json" 
    # ONS_json = "http://statistics.data.gov.uk/sparql"
    # res = performFederatedQuery(queryStr, [ukdigitaltwinendpoint, ukdigitaltwinendpoint])
    # res = performFederatedQuery(queryONS, [ONS_json, ukdigitaltwinendpoint])
    res = performQuery('ukdigitaltwin_test2', queryStr_topo)
    print(res)
    
    
    
    