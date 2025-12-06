from pubchemagent.kgoperations.javagateway import jpsBaseLibGW
from pyderivationagent.kg_operations import PySparqlClient
from pubchemagent.utils.default_configs import QUERY_ENDPOINT
import os

BLAZEGRAPH_USER = os.getenv('BG_USER')
BLAZEGRAPH_PASSWORD = os.getenv('BG_PASSWORD')

class kg_operations():   

    def __init__(self, sparqlEndpint):
        if (not BLAZEGRAPH_USER and not BLAZEGRAPH_PASSWORD) or sparqlEndpint==QUERY_ENDPOINT:
            self.kg_client = PySparqlClient(sparqlEndpint,sparqlEndpint)     
        else:
            self.kg_client = PySparqlClient(sparqlEndpint,sparqlEndpint,kg_user=BLAZEGRAPH_USER,kg_password=BLAZEGRAPH_PASSWORD) 
    
    def querykg(self, queryStr=None):
        response = self.kg_client.performQuery(queryStr)    
        return response
    
    def insertkg(self, insertStr=None):
        self.kg_client.performUpdate(insertStr)
