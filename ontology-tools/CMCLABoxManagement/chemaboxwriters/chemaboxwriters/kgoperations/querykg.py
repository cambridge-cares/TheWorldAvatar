from chemaboxwriters.kgoperations.javagateway import jpsBaseLibGW

jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

def querykg(sparqlEndPoint=None, queryStr=None):

    KGClient = jpsBaseLib_view.RemoteKnowledgeBaseClient(sparqlEndPoint)
    response = str(KGClient.executeQuery(queryStr))
    return response