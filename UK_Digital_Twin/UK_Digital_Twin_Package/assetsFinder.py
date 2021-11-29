##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 16 Nov 2021          #
##########################################

"""This module is developed as an assets finder which is able to return all assets located in a given area indicated by LA code"""
"""The geographical information is queried from ONS http://statistics.data.gov.uk/sparql"""

import os, sys, json
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.queryInterface import performQuery, performUpdate, performFederatedQuery
from shapely.wkt import loads
from shapely.geometry import mapping
import geojson
import ast

"""The status of LA code"""
STATUS_LIVE = 'live'
STATUS_TERMINATED = 'terminated'
UNION = 'UNION' 

"""Country Code"""
UK = "The United Kingdom"
GB = "Great Britain"
ENGLAND_AND_WALES = "England_and_Wales"
ENGLAND = "England"
WALES = "Wales"
SCOTLAND = "Scotland"
NORTHERN_IRELAND = "Northern_Ireland"

"""Asset type"""
POWERPLANT = "<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#PowerPlant>"
ALLASSET = "<http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#Asset>"

"""LA code"""
K02000001 = 'K02000001'
K03000001 = 'K03000001'
K04000001 = 'K04000001'  
E92000001 = 'E92000001' 
W92000004 = 'W92000004' 
W08000001 = 'W08000001'
S92000003 = 'S92000003'
S04000001 = 'S04000001'
N92000002 = 'N92000002'
N07000001 = 'N07000001'
E12000001 = 'E12000001'
E12000002 = 'E12000002'
E12000003 = 'E12000003'
E12000004 = 'E12000004'
E12000005 = 'E12000005'
E12000006 = 'E12000006'
E12000007 = 'E12000007'
E12000008 = 'E12000008'
E12000009 = 'E12000009'

"""Main Function: This function is developed to find the asset within a given area based on the LA code hierarchy provided from the ONS dataset"""
"""Function arguments:
    @givenLACode: the LA code indicates the given data for searching the assets located in.
    @assetsKGendpoint: the endpoint iri of the UK digital twin used for faderated query. 
    @assetsKGendpointLabel: the label of the UK digital twin endpoint used for remote query.
    @ONS_Endpoint: the endpoint iri of the ONS Endpoint used for faderated query. 
    @ONS_EndpointLabel: the label of the ONS endpoint used for remote query.
    @*assetType: the assets type
    """
def assetFinder(givenLACode, assetsKGendpoint, assetsKGendpointLabel, ONS_Endpoint, ONS_EndpointLabel, *assetType):
    givenLACode = str(givenLACode).upper().strip(" ").strip("\n").strip("\r").lstrip()
    # check whether the give LA code is alive
    status_LACode = checkLACodeAlive(givenLACode, ONS_EndpointLabel)
    if status_LACode == STATUS_LIVE:
        pass
    elif status_LACode == STATUS_TERMINATED:
        raise Exception('The currrent status of this LA code is terminated, please refer to a live one.')
    else:
        raise Exception('The currrent status of this LA code is unclear, please check the existing of this LA code.')
    
    # The list is the LA code is the area that can be used to find the assets located in it
    areaList = [K02000001, K03000001, K04000001, E92000001, W92000004, W08000001, S92000003, S04000001, N92000002, N07000001, \
                E12000001, E12000002, E12000003, E12000004, E12000005, E12000006, E12000007, E12000008, E12000009]
    if not givenLACode in areaList:
        raise Exception('The given LA code should wihtin the list', areaList)

    # Due to some missing hierarchy from the ONS linked dataset, 
    # this givenLACodeList is to specifically complement the missing 'within' relationship and some homonymous LA code         
    givenLACodeList = []
    if  givenLACode == K04000001:
        givenLACodeList = [E92000001, W08000001]        
    elif givenLACode == W92000004:
        givenLACodeList= [W08000001]
    elif givenLACode == N92000002:
        givenLACodeList = [N07000001]
    elif givenLACode == S92000003:
        givenLACodeList = [S04000001] 
    elif givenLACode == K03000001:
        givenLACodeList = [E92000001, W08000001, S04000001]   
    elif givenLACode == K02000001:
        givenLACodeList = [E92000001, W08000001, S04000001, N07000001]   
    else:
        givenLACodeList.append(givenLACode)
    
    # Multiple asset types can be concated with 'UNION' in SPARQL query
    assetTypeTripleTemplate = ''
    for at in assetType:
        assetTypeTripleTemplate += '{ ?Asset rdf:type ' + str(at).strip(" ").strip("\n").strip("\r").lstrip() + ' } ' + UNION + ' ' 
    assetTypeTripleTemplate = assetTypeTripleTemplate[:-6] + '.'     
    
    AssetList = []
    for givenLACode in givenLACodeList:
        query_assetWithinGivenLACode = """
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
        PREFIX foi: <http://publishmydata.com/def/ontology/foi/code>
        PREFIX dbo: <https://dbpedia.org/ontology/>
        PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
        SELECT DISTINCT ?Asset
        WHERE
        {
        %s # the Union pattern of assets
        
        ?Asset ontocape_upper_level_system:hasAddress ?locatedArea .
        ?locatedArea dbo:areaCode ?LACode .  
        ?area <http://publishmydata.com/def/ontology/foi/code> ?LACode .
        {?area <http://publishmydata.com/def/ontology/foi/within>  ?within .
        ?within <http://publishmydata.com/def/ontology/foi/code> '%s' .} UNION { ?area <http://publishmydata.com/def/ontology/foi/code> '%s' .} .

        }
        """%(str(assetTypeTripleTemplate), givenLACode, givenLACode)    
        res_assetWithinGivenLACode = json.loads(performFederatedQuery(query_assetWithinGivenLACode, assetsKGendpoint, ONS_Endpoint))
       
        if len(res_assetWithinGivenLACode) > 0:    
            for r in res_assetWithinGivenLACode:
                AssetList.append(r['Asset'])
        
    return AssetList

"""This function is developed to check whether the given LA code is still in its alive sataus"""
def checkLACodeAlive(givenLACode, ONS_EndpointLabel):
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
    
    status = json.loads(performQuery(ONS_EndpointLabel, queryStr))
    if len(status) == 1:
        status = status[0]['status']
    else:
        raise Exception('Please check the LA code, there is no status returned as the LA code may be invalid.')
    print('The status of the given LA', givenLACode,'is:', status)
    return status
        
    
if __name__ == '__main__':   
    ukdigitaltwin = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin/sparql"
    ons = "http://statistics.data.gov.uk/sparql.json"
    res = assetFinder('E12000001', ukdigitaltwin, 'ukdigitaltwin', ons, 'ons', POWERPLANT)
    print(res, len(res))


