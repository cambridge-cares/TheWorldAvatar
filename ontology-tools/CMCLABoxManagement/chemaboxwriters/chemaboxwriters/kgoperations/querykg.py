from chemaboxwriters.kgoperations.javagateway import jpsBaseLibGW
import json

jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

def querykg(sparqlEndPoint=None, queryStr=None):

    StoreClient = jpsBaseLib_view.RemoteStoreClient(sparqlEndPoint)
    response = json.loads(str(StoreClient.executeQuery(queryStr)))
    return response