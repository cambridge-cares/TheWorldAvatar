####################################################
# Author: Wanni Xie (wx243@cam.ac.uk)              #
# Extended from: Tom Savage (trs3@cam.ac.uk)       #
# Last Update Date: 18 August 2021                 #
####################################################

"""This script developed functuions for querying the data from remote triple store or SPARQL endpoints for data visualisation."""

from SPARQLWrapper import SPARQLWrapper, CSV, JSON
from tqdm import tqdm
import time
import numpy as np 

# import shapely
# from shapely.wkt import dumps, loads
# from shapely import wkt, geometry

from shapely.wkt import loads
from shapely.geometry import mapping
import geojson
import ast

"""query the COMO RDF4j triple store for the UK power plant data from DUKES"""
# The endpoint is: https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerPlantKG
def queryPowerPlantForVisualisation(powerPlantEndpoint):
    
  queryVar = ["?powerPlantIRI", "?numericalValue_x", "?numericalValue_y", "?Primary_Fuel_type", "?Plant_Generation_Technology", "?value_of_Designed_Capacity", "?Owner", "?Year_of_Build"]  
  selectClause = " ".join(queryVar)
    
  query_UKPowerPlant = """
          PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
          PREFIX ontopowsys_PowSysPerformance: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#>
          PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
          PREFIX ontocape_upper_level_system_v1: <http://www.theworldavatar.com/ontology/ontoeip/upper_level/system_v1.owl#>
          PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
          PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
          PREFIX meta_model_topology: <http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>
          PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
          PREFIX power_plant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
          
          SELECT DISTINCT %s
          
          WHERE
          {       
            %s space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem .
            ?CoordinateSystem space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate .
            ?CoordinateSystem space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate .
            ?x_coordinate ontocape_upper_level_system:hasValue ?GPS_x_coordinate .
            ?y_coordinate ontocape_upper_level_system:hasValue ?GPS_y_coordinate . 
            ?GPS_x_coordinate ontocape_upper_level_system:numericalValue %s . # longitude is east/west
            ?GPS_y_coordinate ontocape_upper_level_system:numericalValue %s . # latitude is north/south
        	
            %s ontoecape_technical_system:hasRealizationAspect ?PowerGenerator .
            ?PowerGenerator a ontoeip_powerplant:PowerGenerator . 
            ?PowerGenerator ontoecape_technical_system:realizes/ontoeip_powerplant:consumesPrimaryFuel %s .
            ?PowerGenerator ontoecape_technical_system:realizes/ontoeip_powerplant:usesGenerationTechnology %s .
            
            %s ontoecape_technical_system:hasRequirementsAspect/ontocape_upper_level_system:hasValue ?v_capa .
            ?v_capa ontocape_upper_level_system:numericalValue %s .
        	?v_capa ontocape_upper_level_system:hasUnitOfMeasure ?UnitOfCapacity .
            
            %s ontocape_upper_level_system_v1:isOwnedBy/ontocape_upper_level_system_v1:hasName %s .
            
            %s ontoeip_powerplant:hasYearOfBuilt/ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
  
        }""" % (selectClause, queryVar[0], queryVar[1], queryVar[2], queryVar[0], queryVar[3], queryVar[4], queryVar[0], queryVar[5], queryVar[0], queryVar[6], queryVar[0], queryVar[7])
  
  # performing SPARQL query  
  sparql = SPARQLWrapper(powerPlantEndpoint)
  sparql.setReturnFormat(JSON) 
  sparql.setQuery(query_UKPowerPlant)
  # print query time consumption
  start = time.time()
  print('Querying...')
  ret = sparql.queryAndConvert()
  end = time.time()
  print('Finished in ',np.round(end-start,2),' seconds')
  # parsing JSON SPARQL results into an array
  ret = ret['results']['bindings']
  #print(ret) #Unformatted
  num_ret = len(ret)
  num_query_var = len(queryVar) 
  # assigning memory to results array 
  ret_array = np.zeros((num_ret, num_query_var), dtype='object')
  # iterating over results and allocating properties from query
  for i in tqdm(range(num_ret)):
      power_plant_name = ret[i][queryVar[0].strip("?")]['value'].split('#')
      lon = ret[i][queryVar[1].strip("?")]['value']
      lat = ret[i][queryVar[2].strip("?")]['value']
      fuel = ret[i][queryVar[3].strip("?")]['value'].split('#')
      gen_tech = ret[i][queryVar[4].strip("?")]['value'].split('#')
      capacity = ret[i][queryVar[5].strip("?")]['value']
      owner = ret[i][queryVar[6].strip("?")]['value']
      built_year = ret[i][queryVar[7].strip("?")]['value']
      ret_array[i,:] = [power_plant_name[1], lon, lat, fuel[1], gen_tech[1], capacity, owner, built_year]
  return ret_array



"""Query the UK electricity consumption data (total, domestic, industrial&conmercial) and its associated geo information, i.e. the boundaries of the places"""
# electricity_consumption_endpoint: https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKEnergyConsumptionKG
# ONS_GEO_Info_endpoint: http://statistics.data.gov.uk/sparql.json
def queryUKElectricityConsumptionAndAssociatedGEOInfo(electricity_consumption_endpoint, ONS_GEO_Info_endpoint, regionOrArea):
    
  queryVar = ["?Location", "?Area_LACode", "?Area_id_url", "?Total_Electricity_Consumption", "?Domestic_Electricity_Consumption", "?Industrial_and_Commercial_Electricity_Consumption"]  
  selectClause = " ".join(queryVar)
  
  query_UKElectricityConsumption_region = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontoeip_system_function: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_function.owl#>
    PREFIX mathematical_relation: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/mathematical_relation/mathematical_relation.owl#>
    PREFIX db: <https://dbpedia.org/ontology/>
    PREFIX bibtex: <http://purl.org/net/nknouf/ns/bibtex#>
    SELECT DISTINCT %s
    
    WHERE
    {
    ?Area ontocape_upper_level_system:hasAddress/rdf:type <https://dbpedia.org/ontology/Region> .
    ?Area ontocape_upper_level_system:hasAddress %s .
    ?Area ontocape_upper_level_system:hasAddress/db:areaCode %s .
    ?Area ontocape_upper_level_system:hasAddress/bibtex:hasURL %s .
     
    ?Area ontoeip_system_function:consumes/ontocape_upper_level_system:hasValue ?v_TotalELecConsumption .   
    ?v_TotalELecConsumption ontocape_upper_level_system:numericalValue %s .

    ?Area ontoeip_system_function:consumes/mathematical_relation:ConsistsOfDemesticElectricityConsumption ?Domestic . 
    ?Domestic ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s . 
    
    ?Area ontoeip_system_function:consumes/mathematical_relation:ConsistsOfDemesticElectricityConsumption ?Industrial_and_Commercial . 
    ?Industrial_and_Commercial ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s . 
    }
    """ % (selectClause, queryVar[0], queryVar[1], queryVar[2], queryVar[3], queryVar[4], queryVar[5])
    
  query_UKElectricityConsumption_area = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontoeip_system_function: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_function.owl#>
    PREFIX mathematical_relation: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/mathematical_relation/mathematical_relation.owl#>
    PREFIX db: <https://dbpedia.org/ontology/>
    PREFIX bibtex: <http://purl.org/net/nknouf/ns/bibtex#>
    SELECT DISTINCT %s
    
    WHERE
    {
    ?Area ontocape_upper_level_system:hasAddress/rdf:type <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#AddressArea> .
    ?Area ontocape_upper_level_system:hasAddress %s .
    ?Area ontocape_upper_level_system:hasAddress/db:areaCode %s .
    ?Area ontocape_upper_level_system:hasAddress/bibtex:hasURL %s .
     
    ?Area ontoeip_system_function:consumes/ontocape_upper_level_system:hasValue ?v_TotalELecConsumption .   
    ?v_TotalELecConsumption ontocape_upper_level_system:numericalValue %s .

    ?Area ontoeip_system_function:consumes/mathematical_relation:ConsistsOfDemesticElectricityConsumption ?Domestic . 
    ?Domestic ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s . 
    
    ?Area ontoeip_system_function:consumes/mathematical_relation:ConsistsOfDemesticElectricityConsumption ?Industrial_and_Commercial . 
    ?Industrial_and_Commercial ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s . 
    }
    """ % (selectClause, queryVar[0], queryVar[1], queryVar[2], queryVar[3], queryVar[4], queryVar[5])

  # performing SPARQL query  
  sparql = SPARQLWrapper(electricity_consumption_endpoint)
  sparql.setReturnFormat(JSON) 
  if regionOrArea == True:    
      sparql.setQuery(query_UKElectricityConsumption_region)
  else:
      sparql.setQuery(query_UKElectricityConsumption_area)
  # print query time consumption
  start = time.time()
  print('Querying UK Electricity Consumption Data...')
  ret = sparql.queryAndConvert()
  end = time.time()
  print('Finished in ',np.round(end-start,2),' seconds')
  # parsing JSON SPARQL results into an array
  ret = ret['results']['bindings']
  num_ret = len(ret)
  num_query_var = len(queryVar) 
  # assigning memory to results array 
  ret_array = np.zeros((num_ret, num_query_var), dtype='object')
  # iterating over results and allocating properties from query
  counter = 0
  Num_no_geoInfoAreas = 0
  No_geoInfoAreas = []
     
  for i in tqdm(range(num_ret)):
      Location = ret[i][queryVar[0].strip("?")]['value'].split("resource/")[1]
      Area_LACode = ret[i][queryVar[1].strip("?")]['value']
      Area_id_url = ret[i][queryVar[2].strip("?")]['value']
      TotalELecConsumption = ret[i][queryVar[3].strip("?")]['value']
      DomesticConsumption = ret[i][queryVar[4].strip("?")]['value']
      Industrial_and_Commercial = ret[i][queryVar[5].strip("?")]['value']
     
      print(Location)
      print("Area_id_url is:", Area_id_url)
      
      ##############################################
      # print("###############Testing#################")
 
      # Area_id_url = "http://statistics.data.gov.uk/id/statistical-geography/E07000201"
      # # Area_id_url = "http://statistics.data.gov.uk/id/statistical-geography/E09000003" # has two polygons
      # # Area_id_url = "http://statistics.data.gov.uk/id/statistical-geography/E09000011" # has two multipolygons
      # print("Testing Area_id_url is: ", Area_id_url)
      ##############################################
      query_ONS = """
          PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
          PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
          PREFIX ontoeip_system_function: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_function.owl#>
          PREFIX db: <https://dbpedia.org/ontology/>
          PREFIX bibtex: <http://purl.org/net/nknouf/ns/bibtex#>
          PREFIX ont: <http://www.opengis.net/ont/geosparql#>
          PREFIX ont_sparql: <http://www.opengis.net/ont/geosparql#>
          SELECT DISTINCT ?Geo_Info 
          WHERE
          {
          <%s> a <http://statistics.data.gov.uk/def/statistical-geography#Statistical-Geography> .
          <%s> ont:hasGeometry ?geometry . 
          ?geometry ont_sparql:asWKT ?Geo_Info . 
        
          }
          """ % (Area_id_url, Area_id_url) 
         
      # performing SPARQL query  
      sparql = SPARQLWrapper(ONS_GEO_Info_endpoint)
      sparql.setReturnFormat(JSON) 
      sparql.setQuery(query_ONS)
      geo = sparql.queryAndConvert()
      # print("The results contains: ", geo)
      if str(geo['results']['bindings']) == "[]":
          print(Area_id_url, "does't have the geographical attributes.")
          Num_no_geoInfoAreas += 1
          No_geoInfoAreas.append(Area_id_url) #.split("geography/")[1])
          continue
      polygon_point_unformatted_string =str(geo['results']['bindings'][0]["Geo_Info"]['value']) # extract the elements of the original dict
      geojson_string = geojson.dumps(mapping(loads(polygon_point_unformatted_string)))
      geojson_dict = ast.literal_eval(geojson_string) 
      ret_array[i,:] = [Location, Area_LACode, TotalELecConsumption, DomesticConsumption, Industrial_and_Commercial, geojson_dict]            
      counter += 1
     
  print("******************The query results report******************")
  print("The total number of the areas are: ", counter)
  print("The number of the areas don't have the geo attibutes are: ", Num_no_geoInfoAreas, " which are listed as follow: ")
  print(No_geoInfoAreas)
  return ret_array    
       

def queryUKSDGIndicatorForVisualisation():

  query_sdg = """
        PREFIX ontosdg: <http://www.theworldavatar.com/ontology/ontosdg/OntoSDG.owl#>
        PREFIX ontospecies:<http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> 

        SELECT *
        WHERE
        {
        ?powerPlantIRI ontosdg:hasIndicator ?941 .
        ?941 ontospecies:value ?941Value .
        }
        """    
  return query_sdg


if __name__ == '__main__': 
    electricity_consumption_RDF4j_Endpoint = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKEnergyConsumptionKG"
    ONS = "http://statistics.data.gov.uk/sparql"
    ONS_json = "http://statistics.data.gov.uk/sparql.json"
    pp = 'https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerPlantKG'
    res = queryUKElectricityConsumptionAndAssociatedGEOInfo(electricity_consumption_RDF4j_Endpoint, ONS_json, False)
