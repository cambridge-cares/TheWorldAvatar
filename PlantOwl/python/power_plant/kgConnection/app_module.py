# The purpose of this module is to group certain tasks you
# wish to perform on the KG
#============================================================
# get a single JPSGateway instance, jps, from the jpsSingleton

# create a JVM module view for this module and use it to import
# the required java classes
from kgConnection.jpsSingleton import jps

app_module1_view = jps.createModuleView()
jps.importPackages(app_module1_view,"uk.ac.cam.cares.jps.base.query.*")

# this function shows how to do a simple KG query
def doTask(query, kb, isQuery, isUpdate):
    # perform an example sparqle query, see the jps-base-lib docs for further details
    KGRouter = app_module1_view.KGRouter
    KGClient = KGRouter.getKnowledgeBaseClient(KGRouter.HTTP_KB_PREFIX+kb, isQuery, isUpdate)
    response = KGClient.executeQuery((query))
    return str(response)

