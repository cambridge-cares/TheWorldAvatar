from pubchem.kgoperations.javagateway import jpsBaseLibGW
import json


jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

def querykg(sparqlEndPoint=None, queryStr=None):
    # perform an example sparqle query, see the jps-base-lib docs for further details
    #StoreRouter = jpsBaseLib_view.StoreRouter
    #StoreClient = StoreRouter.getStoreClient(sparqlEndPoint, True, False)
    #response = json.loads(str(StoreClient.executeQuery(queryStr)))
    print(sparqlEndPoint)
    Client = jpsBaseLib_view.RemoteStoreClient(sparqlEndPoint,sparqlEndPoint,None, None)
    response = json.loads(str(Client.executeQuery(queryStr)))
    
    return response