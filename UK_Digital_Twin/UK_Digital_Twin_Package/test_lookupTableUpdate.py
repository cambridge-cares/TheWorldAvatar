
from queryInterface import performQuery, performUpdate
import json

# router kg info
kb = 'ontokgrouter'
ontokgrouter_endpoint = "http://www.theworldavatar.com/blazegraph/namespace/ontokgrouter/sparql"

# new repo
endpoint_iri = "http://www.theworldavatar.com/kb/ontokgrouter/ukdigitaltwin"
queryendpoint_iri = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKDigitalTwin"
updateendpoint_iri = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKDigitalTwin/statements"
label = "ukdigitaltwin"



def updateLookUpTable(endpoint_iri, queryendpoint_iri, updateendpoint_iri, label):
    """Sets the current SPARQL query"""
    query = """
            PREFIX ontokgrouter: <http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            INSERT DATA {
                <%s> ontokgrouter:hasQueryEndpoint "%s" .
                <%s> ontokgrouter:hasUpdateEndpoint "%s" .
                <%s> rdf:type ontokgrouter:TargetResource .
                <%s> rdfs:label "%s" .                
            }
            """ % (endpoint_iri, queryendpoint_iri, endpoint_iri, updateendpoint_iri, endpoint_iri, endpoint_iri, label)
    return query 


if __name__ == '__main__':    
    querystr = updateLookUpTable(endpoint_iri, queryendpoint_iri, updateendpoint_iri, label)  
    # print (querystr)
    performUpdate(kb, querystr)
    print("we are done")
    
    