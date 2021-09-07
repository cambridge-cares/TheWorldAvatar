##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 10 June 2021         #
##########################################

"""This module defines the function used to update the Lookup table in the Blazegraph"""
import json
from queryInterface import performQuery, performUpdate
import EndPointConfigAndBlazegraphRepoLabel

lookupTableLable = EndPointConfigAndBlazegraphRepoLabel.lookupTable['lable']

"""updateLookUpTable Function can operate as a updater with three modes, adding new repo information, deleting the existing ones or modify the lookup table, e.g. replacing the lable name, etc."""
# The default mode is adding/inserting.
def updateLookUpTable(endpoint_iri, queryendpoint_iri, updateendpoint_iri, label, isInsert = True, isDelete = False, isModification = False, modificationQueryStr = None):
    # check the update mode setting
    if isModification == False: 
        if (isInsert == True and isDelete == True) or (isInsert == False and isDelete == False):
            print('The update mode does not set in a proper way.')
            return   
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
        
        if len(repo_endpoint_iri) == 0 and isInsert == True:
            # update the Blazegraph lookup table by adding new repo information
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
            print('The new repository information is added in the lookup table in Blazegraph.')   
            return 
        elif len(repo_endpoint_iri) == 0 and isDelete == True:
            # running in a delete mode
            print('The repository does not exist. There is no need to delete it.') 
            return
        elif repo_endpoint_iri[0]['repo_endpoint_iri'] == endpoint_iri and isInsert == True:
            print ('The repository has already been recorded by the lookup table in Blazegraph.')
            return
        elif repo_endpoint_iri[0]['repo_endpoint_iri'] == endpoint_iri and isDelete == True:
            delete_repo_info = """
                    PREFIX ontokgrouter: <http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#>
                    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                    DELETE DATA {
                        <%s> ontokgrouter:hasQueryEndpoint "%s" .
                        <%s> ontokgrouter:hasUpdateEndpoint "%s" .
                        <%s> rdf:type ontokgrouter:TargetResource .
                        <%s> rdfs:label "%s" .                
                    }
                    """ % (endpoint_iri, queryendpoint_iri, endpoint_iri, updateendpoint_iri, endpoint_iri, endpoint_iri, label)
            
            performUpdate(lookupTableLable, delete_repo_info)      
            print ('The repository information has been deleted from Blazegraph lookup table.')
            return
    elif isModification == True and modificationQueryStr != None:
        # running in modification mode
        performUpdate(lookupTableLable, modificationQueryStr) 
        print('The repository information has been modified.')

def deleteLookUpTable_withoutResources(endpoint_iri, queryendpoint_iri, updateendpoint_iri, label, isDelete = True):
    test = """
            PREFIX ontokgrouter: <http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            INSERT DATA {
                <%s> rdf:type ontokgrouter:TargetResource .
            }
            """ % (endpoint_iri)
    performUpdate(lookupTableLable, test)  
    # print(delete_repo_info)
    print ('The repository information has been deleted from Blazegraph lookup table.')
    return
   
        
if __name__ == '__main__': 
    endpoint_iri = "http://statistics.data.gov.uk/sparql.json"
    queryendpoint_iri = "http://statistics.data.gov.uk/sparql.json"
    updateendpoint_iri = "http://statistics.data.gov.uk/sparql.json"
    # queryendpoint_iri = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKDigitalTwin"
    # updateendpoint_iri = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKDigitalTwin/statements"
    label = "ons"
    # deleteLookUpTable_withoutResources(endpoint_iri, queryendpoint_iri, updateendpoint_iri, label)
    updateLookUpTable(endpoint_iri, queryendpoint_iri, updateendpoint_iri, label, True,False)