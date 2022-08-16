from mopsagent.javagateway import jpsBaseLibGW
from mopsagent.utils import params

jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

def queryKG(what=None, queryStr=None):

    KGClient = jpsBaseLib_view.RemoteKnowledgeBaseClient(params.LOCAL_KG_SPARQL)
    response = str(KGClient.executeQuery(queryStr))
    return response

def queryKG2(what=None, queryStr2=None):

    KGClient = jpsBaseLib_view.RemoteKnowledgeBaseClient(params.LOCAL_KG_SPARQL)
    response2 = str(KGClient.executeQuery(queryStr2))
    return response2

def queryKG3(what=None, queryStr3=None):

    KGClient = jpsBaseLib_view.RemoteKnowledgeBaseClient(params.LOCAL_KG_SPARQL)
    response3 = str(KGClient.executeQuery(queryStr3))
    return response3