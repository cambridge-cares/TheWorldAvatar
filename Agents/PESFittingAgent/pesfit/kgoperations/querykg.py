from pesfit.kgoperations.javagateway import jpsBaseLibGW
import json


jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

def querykg(sparqlEndPoint=None, queryStr=None):
    # perform an example sparqle query, see the jps-base-lib docs for further details
    #StoreRouter = jpsBaseLib_view.StoreRouter
    #StoreClient = StoreRouter.getStoreClient(sparqlEndPoint, True, False)
    #KGClient = jpsBaseLib_view.RemoteKnowledgeBaseClient(sparqlEndPoint)
    KGClient = jpsBaseLib_view.RemoteStoreClient(sparqlEndPoint)
    response = json.loads(str(KGClient.executeQuery(queryStr)))
    return response