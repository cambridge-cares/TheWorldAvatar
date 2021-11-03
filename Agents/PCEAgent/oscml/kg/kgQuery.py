from oscml.kg.kgGateway import jpsBaseLibGW
import json

jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

def queryKG(sparqlEndPoint=None, queryStr=None):
    StoreRouter = jpsBaseLib_view.StoreRouter
    StoreClient = StoreRouter.getStoreClient(sparqlEndPoint, True, False)
    response = StoreClient.executeQuery(queryStr)
    response = json.loads(str(response))
    return response