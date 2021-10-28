##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 28 Oct 2021          #
##########################################

"""This module is developed as an assets finder which is able to return all assets located in a given area indicated by LA code"""
"""The geographical information is queried from ONS http://statistics.data.gov.uk/sparql"""

import os, sys, json
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.queryInterface import performQuery, performUpdate, performFederatedQuery


STATUS_LIVE = 'live'
STATUS_TERMINATED = 'terminated'

# class assetsFinder():
#     def __init__(self, DUKESVersion = 2019, numOfBus = 10, initialiserMethod = 'defaultBranchInitialiser', Location = 'http://dbpedia.org/resource/United_Kingdom'):
        
#         self.StoreGeneratedOWLs = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\" + str(numOfBus) + "_bus\\ELine\\"
#         self.SleepycatStoragePath = "C:\\Users\\wx243\\Desktop\\KGB\\1 My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Grid\\" + str(numOfBus) + "_bus\\ELine\\Sleepycat_EBus"
#         self.BranchProperty =  UKElineModel.DataPath + str(numOfBus) + '_bus/branch_properties.csv' # the branch prop should be calculated from the raw data
#         self.BranchModelInitialisation = UKElineModel.DataPath + str(numOfBus) + '_bus/BranchModelInitialisation.csv'             
#         if initialiserMethod == 'defaultBranchInitialiser':
#             self.headerBranchProperty = ["voltage_level_kV", "R_MVA/km", "X_MVA/km", "B_MVA/km", "MVA\n"]
#         elif initialiserMethod == 'preSpecifiedBranchInitialiser':
#             self.headerBranchProperty = ["Bus1", "Bus2", "R", "X", "B", "RateA", "RateB", "RateC", "ratio",	"angle", "status", "angmin", "angmax\n"]
#         else:
#             self.headerBranchProperty = []
        
#         self.DUKESVersion = DUKESVersion
        
        
    

def assetFinder(givenLACode, ONS_Endpoint = "http://statistics.data.gov.uk/sparql.json", *assetsKGendpoint):
    givenLACode = str(givenLACode).upper()
    # check whether the give LA code is alive
    status_LACode = checkLACodeAlive(givenLACode, ONS_Endpoint)
    if status_LACode == STATUS_LIVE:
        pass
    elif status_LACode == STATUS_TERMINATED:
        raise Exception('The currrent status of this LA code is terminated, please refer to a alive one.')
    else:
        raise Exception('The currrent status of this LA code is unclear, please check the existing of this LA code.')
        
        
    
    
    
    return AssetsList


def checkLACodeAlive(givenLACode, ONS_Endpoint = "http://statistics.data.gov.uk/sparql.json"):
    queryStr = """
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX foi: <http://publishmydata.com/def/ontology/foi/code>
    SELECT ?status
    WHERE {
      ?area <http://publishmydata.com/def/ontology/foi/code> "%s"^^xsd:string .
      ?area rdf:type ons:Statistical-Geography .      
      ?area ons:status ?status .
    }"""%str(givenLACode)
    
    status = json.loads(performQuery(ONS_Endpoint, queryStr)) 

    return status['status']





