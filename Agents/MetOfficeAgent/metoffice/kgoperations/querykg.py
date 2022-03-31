# The purpose of this module is to provide functionality to execute
# KG queries using the Store Router from the JPB_BASE_LIB

from metoffice.kgoperations.javagateway import jpsBaseLibGW
import json


jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

def querykg(sparqlEndPoint=None, queryStr=None):
    # perform an example sparql query, see the jps-base-lib docs for further details
    StoreRouter = jpsBaseLib_view.StoreRouter
    # Get StoreClientInterface Java object to sparqlEndPoint with
    # query operations enabled (True) and update operations disabled (False)
    StoreClient = StoreRouter.getStoreClient(sparqlEndPoint, True, False)
    response = json.loads(str(StoreClient.executeQuery(queryStr)))
    return response