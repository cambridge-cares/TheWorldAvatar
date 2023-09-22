##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 21 Sept 2023         #
##########################################

"""This module lists out the SPARQL queries used in generating the UK Grid Topology A-boxes"""

from logging import raiseExceptions
import os, sys, json
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.queryInterface import performQuery
from UK_Digital_Twin_Package.LACodeOfOfficialRegion import LACodeOfOfficialRegion
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel
from UK_Digital_Twin_Package.iris import *
from shapely.wkt import loads
from rfc3987 import parse

## query the ElectricitySystemIRI
def queryElectricitySystemIRI(endPoint, ElectricitySystemName):
    if endPoint == str(EndPointConfigAndBlazegraphRepoLabel.UKPowerSystemBaseWorld['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.UKPowerSystemBaseWorld['endpoint_iri'])
    elif parse(endPoint, rule='IRI'):
        endPointIRI = endPoint
    else:
        raiseExceptions("!!!!Please provide a valid ONS_Endpoint!!!!")

    try:
        LACode = str(LACodeOfOfficialRegion[ElectricitySystemName])
    except KeyError:
        print("!!!Please provide a valid name of the ElectricitySystemName, which can be ", LACodeOfOfficialRegion.keys())
    
    queryStr = f"""
    SELECT DISTINCT *
    WHERE
    {{
    ?ElectricitySystemIRI   <{RDF_TYPE}> <{ONTOENERGYSYSTEM_ELECTRICPOWERSYSTEM}> ;
                            <{ONTOENERGYSYSTEM_HASRELEVANTPLACE}>/<{ONTOENERGYSYSTEM_HASLOCALAUTHORITYCODE}> "{LACode}" . 
    }}
    """ 
    print("...Querying ElectricitySystemIRI...")
    try:
        ElectricitySystemIRI = json.loads(performQuery(endPointIRI, queryStr))[0]['ElectricitySystemIRI']  
        print("...Querying ElectricitySystemIRI is done...")
    except IndexError: 
        print("!!!The seleced area does not exist any electricity system.!!!")
    else:
        return str(ElectricitySystemIRI)

## query all PowerGenerators and their located region, latitude, longitude, PrimaryFuel and GenerationTechnology
def queryPowerPlantAttributes(endPoint_label):
    if endPoint_label == str(EndPointConfigAndBlazegraphRepoLabel.UKPowerSystemBaseWorld['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.UKPowerSystemBaseWorld['endpoint_iri'])
    elif parse(endPoint_label, rule='IRI'):
        endPointIRI = endPoint_label
    else:
        raiseExceptions("!!!!Please provide a valid ONS_Endpoint!!!!")

    queryStr = f"""
    SELECT DISTINCT ?PowerGenerator ?LACode_PP ?PP_lat_lon ?PrimaryFuel ?GenerationTechnology
    WHERE
    {{      	
    ?powerPlant <{RDF_TYPE}> <{ONTOEIP_POWERPLANT_POWERPLANT}> ;
                <{ONTOENERGYSYSTEM_HASRELEVANTPLACE}> ?LocatedPlace ;
                <{ONTOCAPE_TECHNICAL_SYSTEM_HASREALIZATIONASPECT}> ?PowerGenerator ;
                <{ONTOENERGYSYSTEM_HASWGS84LATITUDELONGITUDE}> ?PP_lat_lon .

    ?LocatedPlace <{RDF_TYPE}> <{ONTOENERGYSYSTEM_ADMINISTRATIVEDIVISION}> ;
                  <{ONTOENERGYSYSTEM_HASLOCALAUTHORITYCODE}> ?LACode_PP .
    
    ?PowerGenerator <{ONTOECAPE_TECHNICAL_SYSTEM_REALIZES}>/<{ONTOEIP_POWERPLANT_CONSUMESPRIMARYFUEL}>/<{RDF_TYPE}> ?PrimaryFuel ;
                    <{ONTOECAPE_TECHNICAL_SYSTEM_REALIZES}>/<{ONTOEIP_POWERPLANT_USESGENERATIONTECHNOLOGY}>/<{RDF_TYPE}> ?GenerationTechnology .  
    }}
    """
    
    # queryStr = """
    # PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    # PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    # PREFIX ontocape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    # PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    # PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
    # PREFIX ontoenergysystem: <http://www.theworldavatar.com/ontology/ontoenergysystem/OntoEnergySystem.owl#>
    # SELECT DISTINCT ?PowerGenerator ?LACode_PP ?PP_lat_lon ?PrimaryFuel ?GenerationTechnology
    # WHERE
    # {      	
    # ?powerPlant a ontoeip_powerplant:PowerPlant .
    # ?powerPlant ontoenergysystem:hasRelevantPlace ?LocatedPlace .
    # ?LocatedPlace rdf:type ontoenergysystem:AdministrativeDivision .
    # ?LocatedPlace ontoenergysystem:hasLocalAuthorityCode ?LACode_PP .
    
    # ?powerPlant ontocape_technical_system:hasRealizationAspect ?PowerGenerator . 
    # ?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:consumesPrimaryFuel/rdf:type ?PrimaryFuel .
    # ?PowerGenerator ontocape_technical_system:realizes/ontoeip_powerplant:usesGenerationTechnology/rdf:type ?GenerationTechnology .  
    
    # ?powerPlant ontoenergysystem:hasWGS84LatitudeLongitude ?PP_lat_lon .
    # }
    # """
    print('...remoteQuery queryPowerPlantAttributes...')
    res = json.loads(performQuery(endPointIRI, queryStr))
    print('...queryPowerPlantAttributes is done...')
    for r in res:
        r['PP_lat_lon'] = [float(r['PP_lat_lon'].split('#')[0]), float(r['PP_lat_lon'].split('#')[1])]
    return res
    
# The query for the Region Boundaries returned from ONS
def queryRegionBoundaries(ONS_Endpoint_label):
    if ONS_Endpoint_label == str(EndPointConfigAndBlazegraphRepoLabel.ONS['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.ONS['endpoint_iri'])
    elif parse(ONS_Endpoint_label, rule='IRI'):
        endPointIRI = ONS_Endpoint_label
    else:
        raiseExceptions("!!!!Please provide a valid ONS_Endpoint!!!!")

    queryStr_england_region = f"""
    SELECT DISTINCT ?LACode_area (GROUP_CONCAT(?areaBoundary;SEPARATOR = '***') AS ?Geo_InfoList)
    WHERE
    {{
    ?area   <{RDF_TYPE}> <{ONS_STATISTICAL_GEOGRAPHY}> ;
            <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/E12> ;
            <http://publishmydata.com/def/ontology/foi/code> ?LACode_area ;
            <{ONS_GEOSPARQL_HASGEOMETRY}>/<{ONS_GEOSPARQL_ASWKT}> ?areaBoundary .
    }} GROUP BY ?LACode_area
    """

    # queryStr_england_region = """
    # PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    # PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    # PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
    # PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
    # PREFIX ons_geosparql: <http://www.opengis.net/ont/geosparql#>
    # SELECT DISTINCT ?LACode_area (GROUP_CONCAT(?areaBoundary;SEPARATOR = '***') AS ?Geo_InfoList)
    # WHERE
    # {
    # #?area ons:status "live" .
    # ?area rdf:type ons:Statistical-Geography .
    # ?area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/E12> .
    # ?area <http://publishmydata.com/def/ontology/foi/code> ?LACode_area .
    # ?area ons_geosparql:hasGeometry ?geometry .
    # ?geometry ons_geosparql:asWKT ?areaBoundary .
    # } GROUP BY ?LACode_area
    # """

    # Due to the limitation of the returned result exerted on the ONS endpoint, the query has to be splited into two parts
    queryStr_SWN = f"""
    SELECT DISTINCT ?LACode_area (GROUP_CONCAT(?areaBoundary;SEPARATOR = '***') AS ?Geo_InfoList)
    WHERE
    {{
    ?area <{RDF_TYPE}> <{ONS_STATISTICAL_GEOGRAPHY}> .
    {{ ?area <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/W92> .}} UNION 
    {{ ?area <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/S92> .}} UNION
    {{ ?area <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/N92> .}} 
   
    ?area <http://publishmydata.com/def/ontology/foi/code> ?LACode_area ;
          <{ONS_GEOSPARQL_HASGEOMETRY}>/<{ONS_GEOSPARQL_ASWKT}> ?areaBoundary .
    }} GROUP BY ?LACode_area   
    """

    # queryStr_SWN = """
    # PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    # PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    # PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
    # PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
    # PREFIX ons_geosparql: <http://www.opengis.net/ont/geosparql#>
    # SELECT DISTINCT ?LACode_area (GROUP_CONCAT(?areaBoundary;SEPARATOR = '***') AS ?Geo_InfoList)
    # WHERE
    # {
    # #?area ons:status "live" .
    # ?area rdf:type ons:Statistical-Geography .
    # { ?area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/W92> .} UNION 
    # { ?area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/S92> .} UNION
    # { ?area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/N92> .} 
   
    # ?area <http://publishmydata.com/def/ontology/foi/code> ?LACode_area .
    # ?area ons_geosparql:hasGeometry ?geometry .
    # ?geometry ons_geosparql:asWKT ?areaBoundary .
    # } GROUP BY ?LACode_area   
    # """

    ### -- Use the sub set of the ONS data deployed in the digital ocean --###
    print('...starts queryRegionBoundaries...')
    res_england_region = json.loads(performQuery(endPointIRI, queryStr_england_region)) # [0]['LACode_Region']
    res_SWN = json.loads(performQuery(endPointIRI, queryStr_SWN)) # [0]['LACode_Region']
    print('...finishes queryRegionBoundaries...')  

    for swn in res_SWN:
        res_england_region.append(swn)
    
    if len(res_england_region) != 12:
        raise Exception('The number of the region should be 12 in total but the number of queried is ' + str(len(res_england_region)))
    # clear the symbols in the query results
    for r in res_england_region:
      for key in r.keys():
          if '\"^^' in  r[key] :
            r[key] = (r[key].split('\"^^')[0]).replace('\"','') 
            
    # Check the availability of the geometry of each area
    for r in res_england_region:
      if len(r["Geo_InfoList"]) == 0: 
          raise Exception('There is one place does not have geometry information which is', r["LACode_area"], ', please check the query string and the place status in ONS.')
      elif "***" in r['Geo_InfoList']:
          r['Geo_InfoList'] = r['Geo_InfoList'].split("***")[0]
      r['Geo_InfoList'] = loads(r['Geo_InfoList']) # convert wkt into shapely polygons
    return res_england_region

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
            queryStr = f"""
            SELECT DISTINCT ?LACode_Region
            WHERE
            {{
            {LACode} <{FOI_WITHIN}>+ ?Region .
            ?Region <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/E12> ;
                        <http://publishmydata.com/def/ontology/foi/code> ?LACode_Region .
            }}
            """

            # queryStr = """
            # PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            # PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            # PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
            # PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
            # PREFIX ons_geosparql: <http://www.opengis.net/ont/geosparql#>
            # PREFIX foi: <http://publishmydata.com/def/ontology/foi/>
            # SELECT DISTINCT ?LACode_Region
            # WHERE
            # {
            # %s foi:within+ ?Region .
            # # ?Region ons:status "live" .
            # ?Region ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/E12> .
            # ?Region <http://publishmydata.com/def/ontology/foi/code> ?LACode_Region .
            # }
            # """%LACode

   
            ### -- Use the sub set of the ONS data deployed in the digital ocean --###
            print('...starts queryWithinRegion...')
            RegionOrCountry = json.loads(performQuery(endPointIRI, queryStr))[0]['LACode_Region']
            print('...finishes queryWithinRegion...')  
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
    if ONS_Endpoint_label == str(EndPointConfigAndBlazegraphRepoLabel.ONS['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.ONS['endpoint_iri'])
    elif parse(ONS_Endpoint_label, rule='IRI'):
        endPointIRI = ONS_Endpoint_label
    else:
        raiseExceptions("!!!!Please provide a valid endpoint!!!!")

    queryStr = f"""
    SELECT DISTINCT ?LACode_area (GROUP_CONCAT(?areaBoundary;SEPARATOR = '***') AS ?Geo_InfoList)
    WHERE
    {{
    ?area <{RDF_TYPE}> <{ONS_STATISTICAL_GEOGRAPHY}> .
    {{ ?area <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/K03> .}} UNION 
    {{ ?area <{ONS_ENTITY_CODE}> <http://statistics.data.gov.uk/id/statistical-entity/N92> .}}
    ?area <http://publishmydata.com/def/ontology/foi/code> ?LACode_area ;
          <{ONS_GEOSPARQL_HASGEOMETRY}>/<{ONS_GEOSPARQL_ASWKT}> ?areaBoundary .
    }} GROUP BY ?LACode_area
    """

    # queryStr = """
    # PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    # PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    # PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
    # PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
    # PREFIX ons_geosparql: <http://www.opengis.net/ont/geosparql#>
    # SELECT DISTINCT ?LACode_area (GROUP_CONCAT(?areaBoundary;SEPARATOR = '***') AS ?Geo_InfoList)
    # WHERE
    # {
    # # ?area ons:status "live" .
    # ?area rdf:type ons:Statistical-Geography .
    # { ?area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/K03> .} UNION 
    # { ?area ons_entity:code <http://statistics.data.gov.uk/id/statistical-entity/N92> .}
    # ?area <http://publishmydata.com/def/ontology/foi/code> ?LACode_area .
    # ?area ons_geosparql:hasGeometry ?geometry .
    # ?geometry ons_geosparql:asWKT ?areaBoundary .
    # } GROUP BY ?LACode_area
    # """

    ### -- Use the sub set of the ONS data deployed in the digital ocean --###
    print('...starts queryGBOrNIBoundary...')
    res = json.loads(performQuery(endPointIRI, queryStr))
    print('...finishes queryGBOrNIBoundary...')  

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

    queryStr = f"""
    ASK  
    {{
    ?areaToBeChecked <http://publishmydata.com/def/ontology/foi/code> "{LACode_toBeCheck}" ;
                     <{FOI_WITHIN}> ?areaGiven .
    ?areaGiven <http://publishmydata.com/def/ontology/foi/code> "{givenLACode}" .
    }}
    """

    # queryStr = """
    # PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    # PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    # PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
    # PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
    # PREFIX ons_geosparql: <http://www.opengis.net/ont/geosparql#>
    # PREFIX foi: <http://publishmydata.com/def/ontology/foi/>
    # ASK  
    # {
    # ?areaToBeChecked <http://publishmydata.com/def/ontology/foi/code> "%s" .
    # ?areaGiven <http://publishmydata.com/def/ontology/foi/code> "%s" .
    # ?areaToBeChecked foi:within ?areaGiven .
    # }
    # """%(str(LACode_toBeCheck), str(givenLACode))

    print('...query ifWithin condition...')
    res = json.loads(performQuery(endPointIRI, queryStr))  
    print('...queryifWithin is done...')
    res = res[0]['ASK']
    return res 

def queryEnglandAndWalesAndScotlandBounderies(ONS_Endpoint_label):
    if ONS_Endpoint_label == str(EndPointConfigAndBlazegraphRepoLabel.ONS['label']):
        endPointIRI = str(EndPointConfigAndBlazegraphRepoLabel.ONS['endpoint_iri'])
    elif parse(ONS_Endpoint_label, rule='IRI'):
        endPointIRI = ONS_Endpoint_label
    else:
        raiseExceptions("!!!!Please provide a valid endpoint IRI!!!!")

    queryStr = f"""
    SELECT DISTINCT ?area (GROUP_CONCAT(?areaBoundary;SEPARATOR = '***') AS ?Geo_InfoList)
    WHERE
    {{
    ?area <{RDF_TYPE}> <{ONS_STATISTICAL_GEOGRAPHY}> .
    {{ ?area <{FOI_CODE}> "K04000001" .}} UNION 
    {{ ?area <{FOI_CODE}> "E92000001" .}} UNION
    {{ ?area <{FOI_CODE}> "W92000004" .}} UNION
    {{ ?area <{FOI_CODE}> "S92000003" .}}    
    ?area <{ONS_GEOSPARQL_HASGEOMETRY}> ?geometry .
    ?geometry <{ONS_GEOSPARQL_ASWKT}> ?areaBoundary .
    }} GROUP BY ?area
    """

    # queryStr = """
    # PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    # PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    # PREFIX ons: <http://statistics.data.gov.uk/def/statistical-geography#>
    # PREFIX ons_entity: <http://statistics.data.gov.uk/def/statistical-entity#>
    # PREFIX ons_geosparql: <http://www.opengis.net/ont/geosparql#>
    # PREFIX ons_foi: <http://publishmydata.com/def/ontology/foi/>
    # SELECT DISTINCT ?area (GROUP_CONCAT(?areaBoundary;SEPARATOR = '***') AS ?Geo_InfoList)
    # WHERE
    # {
    # # ?area ons:status "live" .
    # ?area rdf:type ons:Statistical-Geography .
    # { ?area ons_foi:code "K04000001" .} UNION 
    # { ?area ons_foi:code "E92000001" .} UNION
    # { ?area ons_foi:code "W92000004" .} UNION
    # { ?area ons_foi:code "S92000003" .} 
    # ?area ons_geosparql:hasGeometry ?geometry .
    # ?geometry ons_geosparql:asWKT ?areaBoundary .
    # } GROUP BY ?area
    # """

    print('...query EnglandAndWalesAndScotlandBounderies...')
    res = json.loads(performQuery(endPointIRI, queryStr))  
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
  
# if __name__ == '__main__':
#     res = queryWithinRegion("E00000003", EndPointConfigAndBlazegraphRepoLabel.ONS['endpoint_iri'])
#     ## res = queryifWithin(LACode_toBeCheck, givenLACode, EndPointConfigAndBlazegraphRepoLabel.ONS['endpoint_iri'])
#     print(res)
   
   
   
   
   