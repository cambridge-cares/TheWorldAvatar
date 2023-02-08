# The purpose of this module is to group certain tasks you
# wish to perform on the KG
#============================================================
# get the jpsBaseLibGW instance from the jpsSingletons module
# For further information, please check the jps-base_lib, py4jps

###############################################
# Extended by: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 07 June 2022              #
###############################################

# import os, sys
# BASE = os.path.dirname(os.path.abspath(__file__))
# sys.path.insert(0, BASE)
#from jpsSingletons import jpsBaseLibGW
from py4jps.resources import JpsBaseLib
import gc
jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()

# create a JVM module view and use it to import the required java classes
jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

def performQuery(queryendpoint:str, query):
    # perform an example sparqle query, see the jps-base-lib docs for further details
    ## KGRouter = jpsBaseLib_view.StoreRouter
    KGClient = jpsBaseLib_view.RemoteStoreClient(queryendpoint)
    ### KGClient = KGRouter.getStoreClient(KGRouter.HTTP_KB_PREFIX + str(kb), isQuery, isUpdate)
    response = KGClient.executeQuery((query))
    return str(response)

def performUpdate(kb, query, isQuery = True, isUpdate = True):
    # perform an example sparqle query, see the jps-base-lib docs for further details
    KGRouter = jpsBaseLib_view.StoreRouter 
    KGClient = KGRouter.getStoreClient(str(KGRouter.HTTP_KB_PREFIX) + str(kb), isQuery, isUpdate)
    try:
        response = KGClient.executeUpdate((query))
        return str(response)
    except:
        print("KGClient has not been created successfully.")
  
def performFederatedQuery(query, queryendpoints:list):
    ## perform an example sparqle query, see the jps-base-lib docs for further details   
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
    } LIMIT 1
    """
    ukdigitaltwinendpoint = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test2/sparql"
    # res = performFederatedQuery(queryStr1, ["https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridTopology", "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKEnergyConsumptionKG"])
    ONS_json = "http://statistics.data.gov.uk/sparql.json" 
    # ONS_json = "http://statistics.data.gov.uk/sparql"
    # res = performFederatedQuery(qstr, [ukdigitaltwinendpoint, ukdigitaltwinendpoint])
    # res = performFederatedQuery(queryONS, [ONS_json, ukdigitaltwinendpoint])
    res = performQuery("ukdigitaltwin_test3", qstr)
    
    i = 0
    while i < 20:
    ## res = performQuery(ONS_json, qstr)
        res = performFederatedQuery(qstr, [ONS_json, ONS_json])
        i += 1
        print(i)
    print(res)
    
    
    
    