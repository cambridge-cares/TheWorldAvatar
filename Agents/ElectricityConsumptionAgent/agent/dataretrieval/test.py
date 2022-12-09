# The purpose of this module is to group certain tasks you
# wish to perform on the KG
#============================================================
from py4jps.resources import JpsBaseLib
from agent.kgutils.kgclient import KGClient
import json

# create a JVM module view and use it to import the required java classes
jpsBaseLibGW = JpsBaseLib()
jpsBaseLibGW.launchGateway()

# this function shows how to do a simple KG query
query ="""
select (count(?x) as ?nOfTripes)
{
  ?x ?y ?z .
  }
    """

jpsBaseLib_view = jpsBaseLibGW.createModuleView()
jpsBaseLibGW.importPackages(jpsBaseLib_view,"uk.ac.cam.cares.jps.base.query.*")

# query_endpoint=update_endpoint='http://localhost:8080/blazegraph/namespace/ontogasgrid/sparql'
query_endpoint=update_endpoint='http://localhost:8080/blazegraph/namespace/ontogasgrid/sparql'
kg_client = KGClient(query_endpoint, update_endpoint)
query_endpoint = kg_client.kg_client.setQueryEndpoint(query_endpoint)
result = kg_client.performQuery(query)
print(result)