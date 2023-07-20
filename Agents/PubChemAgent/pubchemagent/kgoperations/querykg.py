from pubchemagent.kgoperations.javagateway import jpsBaseLibGW
from pyderivationagent.kg_operations import PySparqlClient

class kg_operations():   

    def __init__(self, sparqlEndpint):
        self.kg_client = PySparqlClient(sparqlEndpint,sparqlEndpint)    
    
    def querykg(self, queryStr=None):
        response = self.kg_client.performQuery(queryStr)    
        return response
    
    def insertkg(self, insertStr=None):
        self.kg_client.performUpdate(insertStr)
