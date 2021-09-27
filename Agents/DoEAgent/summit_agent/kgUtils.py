# The purpose of this module is to provide utility functions
# to interact with the knowledge graph
#============================================================
# get the jpsBaseLibGW instance from the jpsSingletons module
from .jpsSingletons import jpsBaseLibGW

# this function shows how to do a simple KG query
def performQuery(namespace, query, isQuery=True, isUpdate=False):
    # create a JVM module view and use it to import the required java classes
    jpsBaseLib_view = jpsBaseLibGW.createModuleView()
    jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")
    
    # KGRouter = jpsBaseLib_view.KGRouter
    # KGClient = KGRouter.getKnowledgeBaseClient(str(KGRouter.HTTP_KB_PREFIX)+namespace, isQuery, isUpdate)
    # kgRoot + "namespace/" + NAMESPACE + "/sparql"
    
    KGClient = jpsBaseLib_view.RemoteStoreClient("http://theworldavatar.com/blazegraph/namespace/"+namespace+"/sparql")

    response = KGClient.execute(query)

    return str(response)
