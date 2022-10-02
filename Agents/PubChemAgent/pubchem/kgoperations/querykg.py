from pubchem.kgoperations.javagateway import jpsBaseLibGW
import json


jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

jpsBaseLib_update = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_update,"uk.ac.cam.cares.jps.base.insert.*")


def querykg(sparqlEndPoint=None, queryStr=None):
    # perform an example sparqle query, see the jps-base-lib docs for further details
    #StoreRouter = jpsBaseLib_view.StoreRouter
    #StoreClient = StoreRouter.getStoreClient(sparqlEndPoint, True, False)
    #response = json.loads(str(StoreClient.executeQuery(queryStr)))
    print(sparqlEndPoint)
    Client = jpsBaseLib_view.RemoteStoreClient(sparqlEndPoint,sparqlEndPoint,None, None)
    response = json.loads(str(Client.executeQuery(queryStr)))
    
    return response

def insertkg(sparqlEndPoint=None, queryStr=None):
    # perform an example sparqle query, see the jps-base-lib docs for further details
    #StoreRouter = jpsBaseLib_view.StoreRouter
    #StoreClient = StoreRouter.getStoreClient(sparqlEndPoint, True, False)
    #response = json.loads(str(StoreClient.executeQuery(queryStr)))
    print(sparqlEndPoint)
    Client = jpsBaseLib_update.RemoteStoreClient(sparqlEndPoint,sparqlEndPoint,None, None)
    print(type(Client))
    response = json.loads(str(Client.executeUpdate(queryStr)))
    
    return response