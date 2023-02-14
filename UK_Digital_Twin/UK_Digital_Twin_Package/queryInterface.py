# The purpose of this module is to group certain tasks you
# wish to perform on the KG
#============================================================
# get the jpsBaseLibGW instance from the jpsSingletons module
# For further information, please check the jps-base_lib, py4jps

###############################################
# Extended by: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 13 Feb 2023               #
###############################################
from py4jps.resources import JpsBaseLib

jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()

# create a JVM module view and use it to import the required java classes
jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

def performQuery(queryendpoint:str, query):
    KGClient = jpsBaseLib_view.RemoteStoreClient(queryendpoint)
    response = KGClient.executeQuery((query))
    return str(response)

def performUpdate(kb, query, isQuery = True, isUpdate = True):
    KGRouter = jpsBaseLib_view.StoreRouter 
    KGClient = KGRouter.getStoreClient(str(KGRouter.HTTP_KB_PREFIX) + str(kb), isQuery, isUpdate)
    try:
        response = KGClient.executeUpdate((query))
        return str(response)
    except:
        print("KGClient has not been created successfully.")
  
def performFederatedQuery(query, queryendpoints:list):   
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
    ONS_json = "http://statistics.data.gov.uk/sparql.json" 
    # ONS_json = "http://statistics.data.gov.uk/sparql"
    # res = performFederatedQuery(qstr, [ukdigitaltwinendpoint, ukdigitaltwinendpoint])
    # res = performFederatedQuery(queryONS, [ONS_json, ukdigitaltwinendpoint])
    res = performQuery(ONS_json, qstr)
    print(res)
 
    
    
    
    