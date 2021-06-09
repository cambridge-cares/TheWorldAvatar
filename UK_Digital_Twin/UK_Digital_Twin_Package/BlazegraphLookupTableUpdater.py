##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 09 June 2021         #
##########################################

"""This module defines the function used to update the Lookup table in the Blazegraph"""
import json
from queryInterface import performQuery, performUpdate
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLable

lookupTableLable = EndPointConfigAndBlazegraphRepoLable.lookupTable['lable']

def updateLookUpTable(endpoint_iri, queryendpoint_iri, updateendpoint_iri, label):
    # check whether the repo label is already exited 
    check_lable_query = """
            PREFIX ontokgrouter: <http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            SELECT ?repo_endpoint_iri
            WHERE {
                ?repo_endpoint_iri rdf:type ontokgrouter:TargetResource .
                ?repo_endpoint_iri rdfs:label "%s" .                
            }
            """ % label
    repo_endpoint_iri = json.loads(performQuery(lookupTableLable, check_lable_query))
    
    if repo_endpoint_iri[0]['repo_endpoint_iri'] == endpoint_iri:
        print ('The repository has already been recorded by the lookup table in Blazegraph.')
        return
    else:
        # update the new added repo information into the Blazegraph lookup tabel
        update_repo_info = """
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
        
        performUpdate(lookupTableLable, update_repo_info)      
        print('The new repository information has been updated in the lookup table in Blazegraph.')   
        return  

# if __name__ == '__main__': 
#     # new repo
#     endpoint_iri = "http://www.theworldavatar.com/kb/ontokgrouter/ukdigitaltwin"
#     queryendpoint_iri = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKDigitalTwin"
#     updateendpoint_iri = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKDigitalTwin/statements"
#     label = "ukdigitaltwin"
#     updateLookUpTable(endpoint_iri, queryendpoint_iri, updateendpoint_iri, label)

    
    