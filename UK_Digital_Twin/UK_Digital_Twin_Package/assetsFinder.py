##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 04 Nov 2021          #
##########################################

"""This module is developed as an assets finder which is able to return all assets located in a given area indicated by LA code"""
"""The geographical information is queried from ONS http://statistics.data.gov.uk/sparql"""

import os, sys, json
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.queryInterface import performQuery, performUpdate, performFederatedQuery

"""The status of LA code"""
STATUS_LIVE = 'live'
STATUS_TERMINATED = 'terminated'

"""Country Code"""
UK = "The United Kingdom"
GB = "Great Britain"
ENGLAND_AND_WALES = "England_and_Wales"
ENGLAND = "England"
WALES = "Walse"
SCOTLAND = "Scotland"




"""Asset type"""
POWERPLANT = "<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#PowerPlant>"
ALLASSET = "" #TODO: define a class named as asset
    

def assetFinder(givenLACode, assetsKGendpoint, ONS_Endpoint = "http://statistics.data.gov.uk/sparql.json", *assetType):
    givenLACode = str(givenLACode).upper()
    # check whether the give LA code is alive
    status_LACode = checkLACodeAlive(givenLACode, ONS_Endpoint)
    if status_LACode == STATUS_LIVE:
        pass
    elif status_LACode == STATUS_TERMINATED:
        raise Exception('The currrent status of this LA code is terminated, please refer to a alive one.')
    else:
        raise Exception('The currrent status of this LA code is unclear, please check the existing of this LA code.')
        
    # check the LA code of which country
    # 1/UK 2/GB 3/England_and_Wales 4/England 5/Walse 6/scotland 7/Northern_Ireland 
    CountryCheckResult = countryChecker(givenLACode, ONS_Endpoint) 
    
        
        
    queryStr = """
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX foi: <http://publishmydata.com/def/ontology/foi/code>
    SELECT ?LACodeOfLittleArea
    WHERE {
    ?littleAreas <http://publishmydata.com/def/ontology/foi/within> ?givenPlace .
    ?givenPlace <http://publishmydata.com/def/ontology/foi/code> "%s"^^xsd:string .
    ?littleAreas ons:status 'live'^^xsd:string .
    ?littleAreas <http://publishmydata.com/def/ontology/foi/code> ?LACodeOfLittleArea .
    FILTER NOT EXISTS { ?littleAreas rdf:type <http://statistics.data.gov.uk/def/postcode/unit> .}
    # FILTER NOT EXISTS { ?littleAreas <http://publishmydata.com/def/ontology/foi/memberOf> <http://statistics.data.gov.uk/def/geography/collection/E00> .}
    # FILTER NOT EXISTS { ?littleAreas <http://publishmydata.com/def/ontology/foi/memberOf> <http://statistics.data.gov.uk/def/geography/collection/E01> .}
    # FILTER NOT EXISTS { ?littleAreas <http://publishmydata.com/def/ontology/foi/memberOf> <http://statistics.data.gov.uk/def/geography/collection/E02> .}
    # FILTER NOT EXISTS { ?littleAreas <http://publishmydata.com/def/ontology/foi/memberOf> <http://statistics.data.gov.uk/def/geography/collection/E03> .}
    # FILTER NOT EXISTS { ?littleAreas <http://publishmydata.com/def/ontology/foi/memberOf> <http://statistics.data.gov.uk/def/geography/collection/E04> .}
    # FILTER NOT EXISTS { ?littleAreas <http://publishmydata.com/def/ontology/foi/memberOf> <http://statistics.data.gov.uk/def/geography/collection/E05> .}
    # FILTER NOT EXISTS { ?littleAreas <http://publishmydata.com/def/ontology/foi/memberOf> <http://statistics.data.gov.uk/def/geography/collection/E06> .}
    # FILTER NOT EXISTS { ?littleAreas <http://publishmydata.com/def/ontology/foi/memberOf> <http://statistics.data.gov.uk/def/geography/collection/E07> .}
    # FILTER NOT EXISTS { ?littleAreas <http://publishmydata.com/def/ontology/foi/memberOf> <http://statistics.data.gov.uk/def/geography/collection/E08> .}
    # FILTER NOT EXISTS { ?littleAreas <http://publishmydata.com/def/ontology/foi/memberOf> <http://statistics.data.gov.uk/def/geography/collection/E09> .}
    }
    ORDER BY ASC(?LACode)
    LIMIT 1000
    # LIMIT 98000
    """%str(givenLACode)     
    
    res = json.loads(performQuery(ONS_Endpoint, queryStr))
    littleAreasList = [ area['LACodeOfLittleArea'] for area in res]    
    
    
    AssetList = []
    for lacode in littleAreasList:
        query_asset = """
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
        PREFIX ontocape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
        PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
        PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
        PREFIX dbo: <https://dbpedia.org/ontology/>
        SELECT DISTINCT ?powerPlant
        WHERE
        {      	
        ?powerPlant a ontoeip_powerplant:PowerPlant .
        ?powerPlant ontocape_upper_level_system:hasAddress ?Region .
        ?Region dbo:areaCode '%s' .        
        }
        
        """%str(lacode).strip('\n')
        res = json.loads(performQuery(assetsKGendpoint, query_asset))
        if len(res) != 0:
            print(res)
            AssetList.append(res['powerPlant'])
        
    return AssetList
 # This two query can be merged into one and attach the LA code type to the assets

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
    print(status)
    status = status[0]['status']
    print('The status of the given LA code is:', status)
    return status


def countryChecker(givenLACode, ONS_Endpoint = "http://statistics.data.gov.uk/sparql.json"):
    givenLACode = str(givenLACode).strip(" ").strip("\n").strip("\r").lstrip()
    if givenLACode[0] == "K": 
        if givenLACode[2] == "2":
            return UK
        
            
    
    queryStr = """
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX foi: <http://publishmydata.com/def/ontology/foi/code>
    SELECT ?countryCode
    WHERE {
      ?area <http://publishmydata.com/def/ontology/foi/code> "%s"^^xsd:string .
      ?area rdf:type ons:Statistical-Geography .      
      ?area ons:status ?status .
    }"""%str(givenLACode)
    
    status = json.loads(performQuery(ONS_Endpoint, queryStr))
    print(status)
    status = status[0]['status']
    print('The status of the given LA code is:', status)
    return countryCode

if __name__ == '__main__': 
    
    res = assetFinder('E92000001', 'ukdigitaltwin', 'ons')
    print(res, len(res))


