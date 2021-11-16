##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 14 Nov 2021          #
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
WALES = "Walse"
SCOTLAND = "Scotland"
NORTHERN_IRELAND = "Northern_Ireland"

"""Asset type"""
POWERPLANT = "<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#PowerPlant>"
ALLASSET = "<http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#Asset>"
    

def assetFinder(givenLACode, assetsKGendpoint, assetsKGendpointLabel, ONS_Endpoint, ONS_EndpointLabel, *assetType):
    givenLACode = str(givenLACode).upper().strip(" ").strip("\n").strip("\r").lstrip()
    # check whether the give LA code is alive
    status_LACode = checkLACodeAlive(givenLACode, ONS_EndpointLabel)
    if status_LACode == STATUS_LIVE:
        pass
    elif status_LACode == STATUS_TERMINATED:
        raise Exception('The currrent status of this LA code is terminated, please refer to a alive one.')
    else:
        raise Exception('The currrent status of this LA code is unclear, please check the existing of this LA code.')

    areaList = ['K02000001', 'K03000001', 'K04000001', 'E92000001', 'W92000004', 'W08000001', 'S92000003', 'S04000001', 'N92000002', 'N07000001', \
                'E12000001', 'E12000002', 'E12000003', 'E12000004', 'E12000005', 'E12000006', 'E12000007', 'E12000008', 'E12000009']
    if not givenLACode in areaList:
        raise Exception('The given LA code should wihtin the list', areaList)
        
    givenLACodeList = []
    if  givenLACode == 'K04000001':
        givenLACodeList = ['E92000001', 'W08000001']        
    elif givenLACode == 'W92000004':
        givenLACodeList= ['W08000001']
    elif givenLACode == 'N92000002':
        givenLACodeList = ['N07000001']
    elif givenLACode == 'S92000003':
        givenLACodeList = ['S04000001'] 
    elif givenLACode == 'K03000001':
        givenLACodeList = ['E92000001', 'W08000001', 'S04000001']   
    elif givenLACode == 'K02000001':
        givenLACodeList = ['E92000001', 'W08000001', 'S04000001', 'N07000001']   
    else:
        givenLACodeList.append(givenLACode)
    
    # check the asset's LA code type
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
        %s # the Union pattern 
        
        ?Asset ontocape_upper_level_system:hasAddress ?locatedArea .
        ?locatedArea dbo:areaCode ?LACode .  
        ?area <http://publishmydata.com/def/ontology/foi/code> ?LACode .
        {?area <http://publishmydata.com/def/ontology/foi/within>  ?within .
        ?within <http://publishmydata.com/def/ontology/foi/code> '%s' .} UNION { ?area <http://publishmydata.com/def/ontology/foi/code> '%s' .} .
        
        #?area ons:status ?status . 
        #?Asset space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem .
        #?CoordinateSystem space_and_time_extended:hasProjectedCoordinate_x/ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?x_coordinate . # longitude is east/west
        #?CoordinateSystem space_and_time_extended:hasProjectedCoordinate_y/ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?y_coordinate . # latitude is north/south
        
        }
        """%(str(assetTypeTripleTemplate), givenLACode, givenLACode)    
        res_assetWithinGivenLACode = json.loads(performFederatedQuery(query_assetWithinGivenLACode, assetsKGendpoint, ONS_Endpoint))
       
        if len(res_assetWithinGivenLACode) > 0:    
            for r in res_assetWithinGivenLACode:
                AssetList.append(r['Asset'])
        
    return AssetList
        
   
def assetLACodeTypeChecker(assetsKGendpoint, ONS_Endpoint, CountryCheckResult, assetTypeTripleTemplate):
    if CountryCheckResult == UK:
        CountryLACode = '<http://statistics.data.gov.uk/id/statistical-geography/K02000001>'
    elif CountryCheckResult == GB: 
        CountryLACode = '<http://statistics.data.gov.uk/id/statistical-geography/K03000001>'
    elif CountryCheckResult == ENGLAND_AND_WALES: 
        CountryLACode = '<http://statistics.data.gov.uk/id/statistical-geography/K04000001>'
    elif CountryCheckResult == ENGLAND:
        CountryLACode = '<http://statistics.data.gov.uk/id/statistical-geography/E92000001>'
    # If the country == walse, NI, scotlands, check if they in the hierarchy
    elif CountryCheckResult == WALES:
        CountryLACode = '<http://statistics.data.gov.uk/id/statistical-geography/W92000004>'
    elif CountryCheckResult == SCOTLAND:
       CountryLACode = '<http://statistics.data.gov.uk/id/statistical-geography/S92000003>'
    elif CountryCheckResult == NORTHERN_IRELAND:
       CountryLACode = '<http://statistics.data.gov.uk/id/statistical-geography/N92000002>'
       
    query_assetLACodeStatus = """
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX foi: <http://publishmydata.com/def/ontology/foi/code>
    PREFIX dbo: <https://dbpedia.org/ontology/>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    SELECT DISTINCT ?Asset 
    WHERE 
    {
    %s # the Union pattern 
    
    ?Asset ontocape_upper_level_system:hasAddress ?locatedArea .
    ?locatedArea dbo:areaCode ?LACode .  
    ?area <http://publishmydata.com/def/ontology/foi/code> ?LACode .
    ?area rdf:type ons:Statistical-Geography .  
    ?area <http://publishmydata.com/def/ontology/foi/within> %s . 
    ?area ons:status "%s"^^xsd:string .
    
    #?Asset space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem .
    #?CoordinateSystem space_and_time_extended:hasProjectedCoordinate_x/ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?x_coordinate . # longitude is east/west
    #?CoordinateSystem space_and_time_extended:hasProjectedCoordinate_y/ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?y_coordinate . # latitude is north/south
    
    }"""%(str(assetTypeTripleTemplate), CountryLACode, STATUS_LIVE)
    
    res_assetLACodeStatus = json.loads(performFederatedQuery(query_assetLACodeStatus, assetsKGendpoint, ONS_Endpoint))
    if len(res_assetLACodeStatus) > 0:    
        assetWithInvalidLACode = [ [r['Asset'], float(r['x_coordinate']), float(r['y_coordinate'])] for r in res_assetLACodeStatus ]
    else:
        assetWithInvalidLACode = []
    
    query_assetLACodeType = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    PREFIX ontocape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
    PREFIX ons_se: <http://statistics.data.gov.uk/def/statistical-entity#>
    PREFIX dbo: <https://dbpedia.org/ontology/>
    SELECT DISTINCT ?LACodeType
    WHERE
    { 
    %s # the Union pattern 
     	
    ?Asset ontocape_upper_level_system:hasAddress ?locatedArea .
    ?locatedArea dbo:areaCode ?LACode .  
    ?area <http://publishmydata.com/def/ontology/foi/code> ?LACode .
    ?area rdf:type ons:Statistical-Geography . 
    ?area <http://publishmydata.com/def/ontology/foi/within> %s .      
    }
    """%(str(assetTypeTripleTemplate), CountryLACode)
    
    res_assetLACodeType = json.loads(performFederatedQuery(query_assetLACodeType, assetsKGendpoint, ONS_Endpoint))
    
    assetLACodeTypeList = []
    for r in res_assetLACodeType:
        code = r['LACodeType'].split('entity/').strip(" ").strip("\n").strip("\r")
        r['LACodeType'] = int(code[1] + code[2])
        assetLACodeTypeList.append(r['LACodeType'])
           
    return assetWithInvalidLACode, assetLACodeTypeList


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


def countryChecker(givenLACode):
    givenLACode = str(givenLACode).strip(" ").strip("\n").strip("\r").lstrip()
    if givenLACode[0] == "K": 
        if givenLACode[2] == "2":
            return UK
        elif givenLACode[2] == "3":
            return GB
        elif givenLACode[2] == "4":
            return ENGLAND_AND_WALES
    elif givenLACode[0] == "E":
        return ENGLAND
    elif givenLACode[0] == "W":
        return WALES
    elif givenLACode[0] == "S":
        return SCOTLAND
    elif givenLACode[0] == "N":
        return NORTHERN_IRELAND
    else:
        raise Exception('The given LA code is illegal.')
        
def checkEngland(givenLACode, ONS_Endpoint):
    #TODO: when finished, test all the query strings aginst the ONS endpoint
    # extract the code type of the given LA code
    indexLACode = givenLACode[1] + givenLACode[2]
    LACodeList = []
    needFurtherProcess = False
    # TODO: do not forget to inlude the given LA code itself
    
    if not indexLACode in ['01', '02', '05', '06', '07', '08', '09', '10', '12', '92']:
        print('####The given LA code is not in the hierarchy.####')
        # check if the LA code belongs to E15 European Electoral Region, if it is, convert it into E12
        if indexLACode == '15':
            givenLACode[2] = '2'
            LACodeList.append(givenLACode)
        # check if the LA code belongs to E04 Civil Parish, if it is, query all little areas under the place on level upper than the given one (normally it would be E08)
        elif indexLACode == '04':
            query_withinAreaOfE04 = """  
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
            PREFIX db: <https://dbpedia.org/ontology/>
            PREFIX bibtex: <https://zeitkunst.org/bibtex/0.2/bibtex.owl#> 
            # PREFIX bibtex: <http://purl.org/net/nknouf/ns/bibtex#>
            PREFIX ont: <http://www.opengis.net/ont/geosparql#>
            PREFIX ont_sparql: <http://www.opengis.net/ont/geosparql#>
            SELECT DISTINCT ?WithinLAcode
            WHERE
            {
            ?givenPlace a <http://statistics.data.gov.uk/def/statistical-geography#Statistical-Geography> .
            ?givenPlace <http://publishmydata.com/def/ontology/foi/code> "%s"^^xsd:string .
            ?givenPlace <http://publishmydata.com/def/ontology/foi/within> ?WithinLAcode .
            }
            """ %str(givenLACode)
            ret = json.loads(performQuery(ONS_Endpoint, query_withinAreaOfE04))  
            
            withinCodeList, withinLACodeList = [], [] # code refers to the type of the place and LA code is the full code representing a place
            
            for code in ret:
                if 'E' in str(code['WithinLAcode']):
                    withinCode = int(code['WithinLAcode'][1] + code['WithinLAcode'][2])
                    withinCodeList.append(withinCode)
                    withinLACodeList.append(str(code['WithinLAcode']))                      
            minWithinCode = min(withinCodeList)        
            i_min = withinCodeList.index(minWithinCode)         
            minWithinLACode = withinLACodeList(i_min)
            
            query_parentOfminWithinLACode = """  
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
            PREFIX db: <https://dbpedia.org/ontology/>
            PREFIX bibtex: <https://zeitkunst.org/bibtex/0.2/bibtex.owl#> 
            # PREFIX bibtex: <http://purl.org/net/nknouf/ns/bibtex#>
            PREFIX ont: <http://www.opengis.net/ont/geosparql#>
            PREFIX ont_sparql: <http://www.opengis.net/ont/geosparql#>
            PREFIX ons_sg: <http://statistics.data.gov.uk/def/statistical-geography#>
            SELECT DISTINCT ?parentOfminWithinLACode
            WHERE
            {
            ?givenPlace a <http://statistics.data.gov.uk/def/statistical-geography#Statistical-Geography> .
            ?givenPlace <http://publishmydata.com/def/ontology/foi/code> "%s"^^xsd:string .
            ?givenPlace ons_sg:parentcode ?parentOfminWithinLACode .
            }
            """ %str(minWithinLACode)
            
            ret = json.loads(performQuery(ONS_Endpoint, query_parentOfminWithinLACode)) 
            if len(ret) == 1:
                parentOfminWithinLACode = ret[0]['parentOfminWithinLACode']                
            else:
                raise Exception('The parent code does not exist for', minWithinLACode)
        
        
        
        
        
        print('The given LA code is not in the hierarchy. It is going to query the boundary of the given area.')
        
        query_geometryAttribute = """  
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        PREFIX db: <https://dbpedia.org/ontology/>
        PREFIX bibtex: <https://zeitkunst.org/bibtex/0.2/bibtex.owl#> 
        # PREFIX bibtex: <http://purl.org/net/nknouf/ns/bibtex#>
        PREFIX ont: <http://www.opengis.net/ont/geosparql#>
        PREFIX ont_sparql: <http://www.opengis.net/ont/geosparql#>
        SELECT DISTINCT ?Geo_Info
        WHERE
        {
        ?givenPlace a <http://statistics.data.gov.uk/def/statistical-geography#Statistical-Geography> .
        ?givenPlace <http://publishmydata.com/def/ontology/foi/code> "%s"^^xsd:string .
        ?givenPlace ont:hasGeometry ?geometry . 
        ?geometry ont_sparql:asWKT ?Geo_Info .           
        }
        """ %str(givenLACode)
        print('****Starts querying the ONS endpoint for the geometry of the given area.')
        ret = json.loads(performQuery(ONS_Endpoint, query_geometryAttribute))  
        print('****Finish querying the ONS endpoint for the geometry of the given area.')
        if len(ret) == 0:
            raise Exception('The given LA code has no geometry attribute.')
        boundary = ret[0]['Geo_Info']
        geojson_string = geojson.dumps(mapping(loads(boundary)))
        boundary = ast.literal_eval(geojson_string)
    
    if indexLACode == '00':
        return LACodeList.append(givenLACode), needFurtherProcess
        
        
    if indexLACode in  ['00', '01', '02', '05']:
        print('WARNING: The given LA code might be smaller than the one attached to the assets, there might be no assets found.')
        # TODO: if the E00, there will no smaller areas under it
    
    
        
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

    return 
    
if __name__ == '__main__':   
    ukdigitaltwin = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin/sparql"
    ons = "http://statistics.data.gov.uk/sparql.json"
    res = assetFinder('K03000001', ukdigitaltwin, 'ukdigitaltwin', ons, 'ons', POWERPLANT)
    print(res, len(res))


