# The purpose of this module is to group certain tasks you
# wish to perform on the KG
#============================================================
# get the jpsBaseLibGW instance from the jpsSingletons module
# For further information, please check the jps-base_lib, py4jps

###############################################
# Extended by: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 07 June 2022              #
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
    KGClient = KGRouter.getStoreClient(KGRouter.HTTP_KB_PREFIX + str(kb), isQuery, isUpdate)
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
    qstr = """
    SELECT *
    WHERE{
        ?s ?p ?o .
    } LIMIT 10
    """
    ukdigitaltwinendpoint = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test2/sparql"
    # res = performFederatedQuery(queryStr1, ["https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridTopology", "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKEnergyConsumptionKG"])
    ONS_json = "http://statistics.data.gov.uk/sparql.json" 
    # ONS_json = "http://statistics.data.gov.uk/sparql"
    # res = performFederatedQuery(queryStr, [ukdigitaltwinendpoint, ukdigitaltwinendpoint])
    # res = performFederatedQuery(queryONS, [ONS_json, ukdigitaltwinendpoint])
    res = performQuery('ukdigitaltwin_test2', qstr)
    print(res)
    
    
    
    