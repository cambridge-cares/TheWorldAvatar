##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 15 June 2022         #
##########################################

"""This module lists out the SPARQL queries used in generating the UK Grid Model A-boxes"""
import os, sys, json
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.queryInterface import performQuery

def queryNumberOfBus(topologyNodeIRI, endpoint_label):
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontoenergysystem: <http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>  
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
    SELECT DISTINCT (COUNT(?BusNodeIRI) AS ?NumberOfBus)
    WHERE
    {
    <%s> ontocape_upper_level_system:isComposedOfSubsystem ?BusNodeIRI . 
    <%s> rdf:type ontoenergysystem:PowerGridTopology . 
    ?BusNodeIRI rdf:type ontopowsys_PowSysRealization:BusNode . 
    }
    """ % (topologyNodeIRI, topologyNodeIRI)
    
    print('...remoteQuery queryNumberOfBus...')
    res = json.loads(performQuery(endpoint_label, queryStr))[0]["NumberOfBus"]
    print('...queryNumberOfBus is done...')
    return int(res)

if __name__ == '__main__':    
    print(queryNumberOfBus("http://www.theworldavatar.com/kb/ontoenergysystem/PowerGridTopology_b22aaffa-fd51-4643-98a3-ff72ee04e21e", "ukdigitaltwin_test2"))
    