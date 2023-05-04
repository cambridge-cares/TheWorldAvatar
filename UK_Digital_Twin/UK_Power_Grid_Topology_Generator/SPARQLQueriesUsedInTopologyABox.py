##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 12 April 2022        #
##########################################

"""This module lists out the SPARQL queries used in generating the UK Grid Topology A-boxes"""

from logging import raiseExceptions
import os, sys, json
from rdflib.graph import ConjunctiveGraph
from rdflib.store import NO_STORE
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.queryInterface import performQuery, performUpdate, performFederatedQuery
from UK_Digital_Twin_Package.LACodeOfOfficialRegion import LACodeOfOfficialRegion
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel
from collections import Counter
from shapely.wkt import loads
from shapely.geometry import mapping
import urllib.parse
import requests
from rfc3987 import parse
import geojson
import ast

## query the ElectricitySystemIRI
def queryElectricitySystemIRI(endpoint_label, ElectricitySystemName):
    try:
        LACode = str(LACodeOfOfficialRegion[ElectricitySystemName])
    except KeyError:
        print("!!!Please provide a valid name of the ElectricitySystemName, which can be ", LACodeOfOfficialRegion.keys())
    
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontoenergysystem: <http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#>
    SELECT DISTINCT ?ElectricitySystemIRI 
    WHERE
    {
    ?ElectricitySystemIRI rdf:type ontoenergysystem:ElectricPowerSystem .
    ?ElectricitySystemIRI ontoenergysystem:hasRelevantPlace/ontoenergysystem:hasLocalAuthorityCode "%s" . 
    }
    """ % (LACode)
    print("...Querying ElectricitySystemIRI...")
    try:
        ElectricitySystemIRI = json.loads(performQuery(endpoint_label, queryStr))[0]['ElectricitySystemIRI']  
        print("...Querying ElectricitySystemIRI is done...")
    except IndexError: 
        print("!!!The seleced area does not exist any electricity system.!!!")
    else:
        return str(ElectricitySystemIRI)

## query all PowerGenerators and their located region, latitude, longitude, PrimaryFuel and GenerationTechnology
def queryPowerPlantAttributes(endPoint_label):
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    PREFIX ontocape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
    PREFIX ontoenergysystem: <http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#>
    SELECT DISTINCT ?PowerGenerator ?LACode_PP ?PP_lat_lon ?PrimaryFuel ?GenerationTechnology
    WHERE
    {      	
    ?powerPlant a ontoeip_powerplant:PowerPlant .
    ?powerPlant ontoenergysystem:hasRelevantPlace ?LocatedPlace .
    ?LocatedPlace rdf:type ontoenergysystem:AdministrativeDivision .
    ?LocatedPlace ontoenergysystem:hasLocalAuthorityCode ?LACode_PP .
    
    ?powerPlant ontocape_technical_system:hasRealizationAspect ?PowerGenerator . 
    ?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:consumesPrimaryFuel/rdf:type ?PrimaryFuel .
    ?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:usesGenerationTechnology/rdf:type ?GenerationTechnology .  
    
    ?powerPlant ontoenergysystem:hasWGS84LatitudeLongitude ?PP_lat_lon .
    }
    """
    print('...remoteQuery queryPowerPlantAttributes...')
    res = json.loads(performQuery(endPoint_label, queryStr))
    print('...queryPowerPlantAttributes is done...')
    for r in res:
        r['PP_lat_lon'] = [float(r['PP_lat_lon'].split('#')[0]), float(r['PP_lat_lon'].split('#')[1])]
    return res
    
# The query for the Region Boundaries returned from ONS
def queryRegionBoundaries(ONS_Endpoint_label):
    queryStr_england_region = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
    PREFIX ons_geosparql: <http://www.opengis.net/ont/geosparql#>
    SELECT DISTINCT ?LACode_area (GROUP_CONCAT(?areaBoundary;SEPARATOR = '***') AS ?Geo_InfoList)
    WHERE
    {
    ?area ons:status "live" .
    ?area rdf:type ons:Statistical-Geography .
    ?area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/E12> .
    ?area <http://publishmydata.com/def/ontology/foi/code> ?LACode_area .
    ?area ons_geosparql:hasGeometry ?geometry .
    ?geometry ons_geosparql:asWKT ?areaBoundary .
    } GROUP BY ?LACode_area
    """
    # Due to the limitation of the returned result exerted on the ONS endpoint, the query has to be splited into two parts
    queryStr_SWN = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
    PREFIX ons_geosparql: <http://www.opengis.net/ont/geosparql#>
    SELECT DISTINCT ?LACode_area (GROUP_CONCAT(?areaBoundary;SEPARATOR = '***') AS ?Geo_InfoList)
    WHERE
    {
    ?area ons:status "live" .
    ?area rdf:type ons:Statistical-Geography .
    { ?area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/W92> .} UNION 
    { ?area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/S92> .} UNION
    { ?area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/N92> .} 
   
    ?area <http://publishmydata.com/def/ontology/foi/code> ?LACode_area .
    ?area ons_geosparql:hasGeometry ?geometry .
    ?geometry ons_geosparql:asWKT ?areaBoundary .
    } GROUP BY ?LACode_area   
    """

    encodedString_1 = urllib.parse.quote(queryStr_england_region)
    getString_1 = "http://statistics.data.gov.uk/sparql.json?query=" + str(encodedString_1)

    encodedString_2 = urllib.parse.quote(queryStr_SWN)
    getString_2 = "http://statistics.data.gov.uk/sparql.json?query=" + str(encodedString_2)

    print('...HTTP GET queryRegionBoundaries...')
    r1 = requests.get(getString_1, timeout=60)
    r2 = requests.get(getString_2, timeout=60)

    # print(json.loads(r1.text))
    # print(json.loads(r2.text))

    res_england_region = json.loads(r1.text)['results']['bindings']# [0]['LACode_Region']['value'] ## FIXME: this
    res_SWN = json.loads(r2.text)['results']['bindings']# [0]['LACode_Region']['value'] ## FIXME: this

    # print(len(res_england_region))

    print('...HTTP GET queryRegionBoundaries is done...')
    if int(r1.status_code) != 200 or int(r2.status_code) != 200:
        print('The Region Boundaries cannot be found.')
        return None

    for swn in res_SWN:
        res_england_region.append(swn)
    
    if len(res_england_region) != 12:
        raise Exception('The number of the region should be 12 in total but the number of queried is ' + str(len(res)))
    # clear the symbols in the query results
    for r in res_england_region:
      for key in r.keys():
          if '\"^^' in  r[key] :
            r[key] = (r[key].split('\"^^')[0]).replace('\"','') 
            
    # Check the availability of the geometry of each area
    for r in res_england_region:
      if len(r["Geo_InfoList"]["value"]) == 0: 
          raise Exception('There is one place does not have geometry information which is', r["LACode_area"], ', please check the query string and the place status in ONS.')
      elif "***" in r['Geo_InfoList']["value"]:
          r['Geo_InfoList']["value"] = r['Geo_InfoList']["value"].split("***")[0]
      r['Geo_InfoList']["value"] = loads(r['Geo_InfoList']["value"]) # convert wkt into shapely polygons
    return res_england_region

# if __name__ == '__main__':
#     res = queryRegionBoundaries('ons')

## This function is designed to find the region which the given area within in 
def queryWithinRegion(LACode:str, ONS_Endpoint):
    LACode = str(LACode)
    typeCode = int(LACode[1] + LACode[2])
    if ONS_Endpoint == str(EndPointConfigAndBlazegraphRepoLabel.ONS['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.ONS['endpoint_iri'])
    elif parse(ONS_Endpoint, rule='IRI'):
        endPointIRI = ONS_Endpoint
    else:
        raiseExceptions("!!!!Please provide a valid ONS_Endpoint!!!!")

    if LACode[0] == 'E':
        if not typeCode >= 11: # E11, E12 and other places whose code is larger than 11 are not included in any areas
            LACode = " <http://statistics.data.gov.uk/id/statistical-geography/" + LACode + ">"
            queryStr = """
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
            PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
            PREFIX ons_geosparql: <http://www.opengis.net/ont/geosparql#>
            PREFIX foi: <http://publishmydata.com/def/ontology/foi/>
            SELECT DISTINCT ?LACode_Region
            WHERE
            {
            %s foi:within+ ?Region .
            ?Region ons:status "live" .
            ?Region ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/E12> .
            ?Region <http://publishmydata.com/def/ontology/foi/code> ?LACode_Region .
            }
            """%LACode

            encodedString = urllib.parse.quote(queryStr)
            getString = "http://statistics.data.gov.uk/sparql.json?query=" + str(encodedString)

            print('...HTTP GET WithinRegion of a given LA code...')
            r = requests.get(getString, timeout=60)
            RegionOrCountry = json.loads(r.text)['results']['bindings'][0]['LACode_Region']['value']
            # RegionOrCountry = json.loads(r.text)['results']['bindings']
            # for result in RegionOrCountry:
            #     if result['LACode_Region']['value'][:3] == 'E12':
            #         RegionOrCountry = result['LACode_Region']['value']
            #         break
            print('...HTTP GET queryWithinRegion is done...')
            if int(r.status_code) != 200:
                # raise Exception('The within region of the given LA code cannot be found, please check if the given LA code is in the hierarchy.')
                print('The within region of the given LA code cannot be found, please check if the given LA code is in the hierarchy.')
                return None
        else :
            # raise Exception('The given LA coed is ', LACode,' which is not within any region of England.')
            print('The given LA coed is ', LACode,' which is not within any region of England.')
            return None
    elif LACode[0] == 'W':
        RegionOrCountry = ['W92000004', 'W08000001']    
    elif LACode[0] == 'S':
        RegionOrCountry = ['S92000003', 'S04000001']      
    elif LACode[0] == 'N':
        RegionOrCountry = ['N92000002', 'N07000001']
    else:     
        # raise Exception('The given area does not have a within region, please check the given LA code.')   
        print('The given area does not have a within region, please check the given LA code.')
        return None
    return RegionOrCountry

## This query is used to query the boundary of the GB and Northern Ireland
def queryGBOrNIBoundary(ONS_Endpoint_label):
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
    PREFIX ons_geosparql: <http://www.opengis.net/ont/geosparql#>
    SELECT DISTINCT ?LACode_area (GROUP_CONCAT(?areaBoundary;SEPARATOR = '***') AS ?Geo_InfoList)
    WHERE
    {
    ?area ons:status "live" .
    ?area rdf:type ons:Statistical-Geography .
    { ?area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/K03> .} UNION 
    { ?area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/N92> .}
    ?area <http://publishmydata.com/def/ontology/foi/code> ?LACode_area .
    ?area ons_geosparql:hasGeometry ?geometry .
    ?geometry ons_geosparql:asWKT ?areaBoundary .
    } GROUP BY ?LACode_area
    """
    encodedString = urllib.parse.quote(queryStr)
    getString = "http://statistics.data.gov.uk/sparql.json?query=" + str(encodedString)

    print('...HTTP GET queryGBOrNIBoundary...')
    r = requests.get(getString, timeout=60)
    res = json.loads(r.text)['results']['bindings']# [0]['LACode_Region']['value']
    print('...HTTP GET queryGBOrNIBoundary is done...')
    if int(r.status_code) != 200:
        raise Exception('The queryGBOrNIBoundary has returned nothing from ONS server.')
    res = [ {"LACode_area": re["LACode_area"]['value'], "Geo_InfoList": re["Geo_InfoList"]['value']}  for re in res]
    for r in res:
      for key in r.keys():
          if '\"^^' in  r[key]:
            r[key] = (r[key].split('\"^^')[0]).replace('\"','') 
    for r in res:
      if len(r["Geo_InfoList"]) == 0: 
          raise Exception('There is one place does not have geometry information which is', r["LACode_area"], ', please check the query string and the place status in ONS.')
      elif "***" in r['Geo_InfoList']:
          r['Geo_InfoList'] = r['Geo_InfoList'].split("***")[0]
      r['Geo_InfoList'] = loads(r['Geo_InfoList']) # convert wkt into shapely polygons
    return res      

def queryifWithin(LACode_toBeCheck, givenLACode, ONS_Endpoint_label):
    if ONS_Endpoint_label == str(EndPointConfigAndBlazegraphRepoLabel.ONS['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.ONS['endpoint_iri'])
    elif parse(ONS_Endpoint_label, rule='IRI'):
        endPointIRI = ONS_Endpoint_label
    else:
        raiseExceptions("!!!!Please provide a valid endpoint IRI!!!!")

    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
    PREFIX ons_geosparql: <http://www.opengis.net/ont/geosparql#>
    PREFIX foi: <http://publishmydata.com/def/ontology/foi/>
    ASK  
    {
    ?areaToBeChecked <http://publishmydata.com/def/ontology/foi/code> "%s" .
    ?areaGiven <http://publishmydata.com/def/ontology/foi/code> "%s" .
    ?areaToBeChecked foi:within ?areaGiven .
    }
    """%(str(LACode_toBeCheck), str(givenLACode))
    print('...query ifWithin condition...')
    res = json.loads(performQuery(endPointIRI, queryStr))  
    print('...queryifWithin is done...')
    res = res[0]['ASK']
    return res 

def queryEnglandAndWalesAndScotlandBounderies(ONS_Endpoint_label):
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
    PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
    PREFIX ons_geosparql: <http://www.opengis.net/ont/geosparql#>
    PREFIX ons_foi: <http://publishmydata.com/def/ontology/foi/>
    SELECT DISTINCT ?area (GROUP_CONCAT(?areaBoundary;SEPARATOR = '***') AS ?Geo_InfoList)
    WHERE
    {
    ?area ons:status "live" .
    ?area rdf:type ons:Statistical-Geography .
    { ?area ons_foi:code "K04000001" .} UNION 
    { ?area ons_foi:code "E92000001" .} UNION
    { ?area ons_foi:code "W92000004" .} UNION
    { ?area ons_foi:code "S92000003" .} 
    ?area ons_geosparql:hasGeometry ?geometry .
    ?geometry ons_geosparql:asWKT ?areaBoundary .
    } GROUP BY ?area
    """
    print('...query EnglandAndWalesAndScotlandBounderies...')
    res = json.loads(performQuery(ONS_Endpoint_label, queryStr))  
    print('...queryEnglandAndWalesAndScotlandBounderies is done...')
    # clear the symbols in the query results
    for r in res:
      for key in r.keys():
          if '\"^^' in  r[key]:
            r[key] = (r[key].split('\"^^')[0]).replace('\"','') 
    for r in res:
      if len(r["Geo_InfoList"]) == 0: 
          raise Exception('There is one place does not have geometry information which is', r["area"], ', please check the query string and the place status in ONS.')
      elif "***" in r['Geo_InfoList']:
          r['Geo_InfoList'] = r['Geo_InfoList'].split("***")[0]
      r['Geo_InfoList'] = loads(r['Geo_InfoList']) # convert wkt into shapely polygons
      if "K04000001" in str(r['area']):
          EngAndWalesBound = r['Geo_InfoList']
      elif "E92000001" in str(r['area']):
          EngBound = r['Geo_InfoList']
      elif "W92000004" in str(r['area']):
          WalesBound = r['Geo_InfoList']
      elif "S92000003" in str(r['area']):
          ScotlandBound = r['Geo_InfoList']
    return EngAndWalesBound, EngBound, WalesBound, ScotlandBound           

if __name__ == '__main__':
    # sl_pp = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Power_Plant\\Sleepycat_UKpp"
    # sl_topo = "C:\\Users\\wx243\\Desktop\\KGB\\My project\\1 Ongoing\\4 UK Digital Twin\\A_Box\\UK_Energy_Consumption\\Sleepycat_UKec_UKtopo"
    iri = "http://dbpedia.org/resource/West_Midlands_(county)"  
    test_region = "http://dbpedia.org/resource/North_West_England"
    # scot_iri = 'http://dbpedia.org/resource/Scotland'
    # res = queryRegionBoundaries('ons')
    # res = queryRegionBoundaries_testJSON('ons')
    #res = queryEnglandAndWalesAndScotlandBounderies('ons')
    # print(res)
    # res = queryPowerPlantAttributes('ukdigitaltwin_test2')
    # res = queryGBOrNIBoundary('ons')
    res = queryWithinRegion('E07000066', 'ons')
    # res = queryCardiffBound('ons')
    # res = queryifWithin('E12000007', 'K03000001', 'ons')
    # res = queryElectricitySystemIRI('ukdigitaltwin_test2', 'Great_Britain')
    print(res)
    # print(res[0], len(res))
    
   
   
   
   
   
   
   
   
   