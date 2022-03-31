# The purpose of this module is to provide functionality to execute
# KG updates using the Store Router from the JPB_BASE_LIB

from metoffice.kgoperations.javagateway import jpsBaseLibGW
import json


jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

def updatekg(sparqlEndPoint=None, queryStr=None):
    # perform an example sparql query, see the jps-base-lib docs for further details
    StoreRouter = jpsBaseLib_view.StoreRouter
    # Get StoreClientInterface Java object to sparqlEndPoint with both
    # query and update operations enabled (True)
    StoreClient = StoreRouter.getStoreClient(sparqlEndPoint, True, True)
    response = json.loads(str(StoreClient.executeUpdate(queryStr)))
    return response