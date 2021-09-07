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
from jpsSingletons import jpsBaseLibGW

# create a JVM module view and use it to import the required java classes
jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

# this function shows how to do a simple KG query
def performQuery(kb, query, isQuery = True, isUpdate = False):
    # perform an example sparqle query, see the jps-base-lib docs for further details
    KGRouter = jpsBaseLib_view.KGRouter
    KGClient = KGRouter.getKnowledgeBaseClient(KGRouter.HTTP_KB_PREFIX+ str(kb), isQuery, isUpdate)
    if type(KGClient) == 'NoneType':       
        print('KGClient in the query interfaced has not been created successfully. Please check if the endpoint is already added into the lookup table of Blazegraph.')
    response = KGClient.executeQuery((query))
    return str(response)

def performUpdate(kb, query, isQuery = True, isUpdate = True):
    # perform an example sparqle query, see the jps-base-lib docs for further details
    KGRouter = jpsBaseLib_view.KGRouter
    KGClient = KGRouter.getKnowledgeBaseClient(KGRouter.HTTP_KB_PREFIX+ str(kb), isQuery, isUpdate)
    if type(KGClient) == 'NoneType':       
        print('KGClient in the query interfaced has not been created successfully. Please check if the endpoint is already added into the lookup table of Blazegraph.')
    response = KGClient.executeUpdate((query))
    return str(response)

def performFederatedQuery(query, *queryendpoints):
    # perform an example sparqle query, see the jps-base-lib docs for further details   
    RemoteKnowledgeBaseClient = jpsBaseLib_view.RemoteKnowledgeBaseClient()
    if len(queryendpoints) == 0:
        print('Please specify the remote query endpoints.')
        return None    
    endpoints = []
    for ed in queryendpoints:        
        endpoints.append(str(ed))  
    response = RemoteKnowledgeBaseClient.executeFederatedQuery(endpoints, query)
    return str(response)

if __name__ == '__main__':  
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX dbo: <https://dbpedia.org/ontology/>
    SELECT DISTINCT ?Bus_node ?Location_region ?areacode
    WHERE
    {
    ?Bus_node rdf:type ontopowsys_PowSysFunction:PowerEquipmentConnection .
    ?Bus_node ontocape_upper_level_system:hasAddress ?Location_region . 
    
    ?Location_region rdf:type <https://dbpedia.org/ontology/Region> .
    ?Location_region dbo:areaCode ?areacode .
    
    }
    """ 
    res = performFederatedQuery(queryStr, "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridTopology", "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKEnergyConsumptionKG")
    print(res)
    
    
    
    