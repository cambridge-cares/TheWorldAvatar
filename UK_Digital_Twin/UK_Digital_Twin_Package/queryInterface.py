# The purpose of this module is to group certain tasks you
# wish to perform on the KG
#============================================================
# get the jpsBaseLibGW instance from the jpsSingletons module
# import os, sys
# BASE = os.path.dirname(os.path.abspath(__file__))
# sys.path.insert(0, BASE)
from jpsSingletons import jpsBaseLibGW

# create a JVM module view and use it to import the required java classes
jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

# this function shows how to do a simple KG query
def performQuery(kb, query, isQuery = True, isUpdate = False):
    # perform an example sparqle query, see the jps-base-lib docs for further details
    KGRouter = jpsBaseLib_view.KGRouter
    KGClient = KGRouter.getKnowledgeBaseClient(KGRouter.HTTP_KB_PREFIX+ str(kb), isQuery, isUpdate)
    print(type(KGRouter), type(KGClient))
    response = KGClient.executeQuery((query))
    return str(response)

def performUpdate(kb, query, isQuery = True, isUpdate = True):
    # perform an example sparqle query, see the jps-base-lib docs for further details
    KGRouter = jpsBaseLib_view.KGRouter
    KGClient = KGRouter.getKnowledgeBaseClient(KGRouter.HTTP_KB_PREFIX+ str(kb), isQuery, isUpdate)
    response = KGClient.executeUpdate((query))
    return str(response)
