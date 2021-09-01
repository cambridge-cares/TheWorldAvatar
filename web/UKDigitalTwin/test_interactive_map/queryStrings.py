####################################################
# Author: Wanni Xie (wx243@cam.ac.uk)              #
# Extended from: Tom Savage (trs3@cam.ac.uk)       #
# Last Update Date: 30 August 2021                 #
####################################################

"""This script developed functuions for querying the data from remote triple store or SPARQL endpoints for data visualisation."""

import os, sys, json
from SPARQLWrapper import SPARQLWrapper, JSON
from tqdm import tqdm
import time
import numpy as np 

from shapely.wkt import loads
from shapely.geometry import mapping
import geojson
import ast

# sys.path.insert(0, '\\TheWorldAvatar\\UK_Digital_Twin\\')
# from UK_Digital_Twin.UK_Digital_Twin_Package.queryInterface import performQuery, performUpdate, performFederatedQuery
from queryInterface import performQuery, performUpdate, performFederatedQuery

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
    
    ?Area ontoeip_system_function:consumes/mathematical_relation:ConsistsOfIndustrialAndCommercialConsumption ?Industrial_and_Commercial . 
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
    
    ?Area ontoeip_system_function:consumes/mathematical_relation:ConsistsOfIndustrialAndCommercialConsumption ?Industrial_and_Commercial . 
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
  
  print('Querying UK ONS geometry Data...')  
  
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
      
      if str(geo['results']['bindings']) == "[]":
          print(Area_id_url, "does't have the geographical attributes.")
          Num_no_geoInfoAreas += 1
          No_geoInfoAreas.append(Area_id_url)
          continue
      polygon_point_unformatted_string =str(geo['results']['bindings'][0]["Geo_Info"]['value']) #extract the elements of the original dict
      geojson_string = geojson.dumps(mapping(loads(polygon_point_unformatted_string)))
      geojson_dict = ast.literal_eval(geojson_string) 
      ret_array[i,:] = [Location, Area_LACode, TotalELecConsumption, DomesticConsumption, Industrial_and_Commercial, geojson_dict]            
      counter += 1
      
  end = time.time()
  
  print("******************The query results report******************")
  print('Finished in ',np.round(end-start,2),' seconds')  
  print("The total number of the areas are: ", counter)
  print("The number of the areas don't have the geo attibutes are: ", Num_no_geoInfoAreas, " which are listed as follow: ")
  print(No_geoInfoAreas)
  return ret_array 

"""This function is used for query the bus model parameters and input variables from the grid model endpoint and query the location of the bus from the grid topology endpoint"""
# topoEndpoint = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridTopology"
# busModelEndpoint = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridModel"
# the federated query interface takes the endpoint urls as the input arguments 
def queryGridModeltForVisualisation_Bus(topoEndpoint, busModelEndpoint):
    
  queryVar_1 = ["?Bus_num", "?numericalValue_x", "?numericalValue_y", "?Bus_type", "?para_Gs", "?para_Bs", "?para_area", "?para_basekV", \
                "?para_zone", "?para_Vmax", "?para_Vmin"] 
      
  queryVar_2 = ["?Bus_num", "?input_Pd", "?input_Gd", "?input_Vm", "?input_Va"] 
      
  selectClause_1 = " ".join(queryVar_1)
  selectClause_2 = " ".join(queryVar_2)
  
  queryBusModelParameter_federated = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
    PREFIX mathematical_model: <http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#>
    PREFIX ontopowsys_PowerSystemModel: <http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#>
    PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    SELECT DISTINCT %s
    WHERE
    {
    ?EquipmentConnection_EBus rdf:type ontopowsys_PowSysFunction:PowerEquipmentConnection .   
    ?EquipmentConnection_EBus space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem_Bus .    
    ?CoordinateSystem_Bus  space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate_Bus .
    ?CoordinateSystem_Bus  space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate_Bus .
    ?x_coordinate_Bus  ontocape_upper_level_system:hasValue ?GPS_x_coordinate_Bus .
    ?y_coordinate_Bus  ontocape_upper_level_system:hasValue ?GPS_y_coordinate_Bus . 
    ?GPS_x_coordinate_Bus  ontocape_upper_level_system:numericalValue %s .
    ?GPS_y_coordinate_Bus  ontocape_upper_level_system:numericalValue %s .
    
    ?EquipmentConnection_EBus ontoecape_technical_system:isRealizedBy ?EBus . 
    ?EBus rdf:type ontopowsys_PowSysRealization:BusNode .
    ?EBus ontocape_upper_level_system:isModeledBy ?Model_EBus . 
    
    ?Model_EBus mathematical_model:hasModelVariable ?Bus_Number . 
    ?Bus_Number rdf:type ontopowsys_PowerSystemModel:BusNumber . 
    ?Bus_Number rdf:type mathematical_model:Parameter . 
    ?Bus_Number ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_EBus mathematical_model:hasModelVariable ?Type . 
    ?Type rdf:type ontopowsys_PowerSystemModel:BusType . 
    ?Type rdf:type mathematical_model:Parameter . 
    ?Type ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_EBus mathematical_model:hasModelVariable ?Gsvar . 
    ?Gsvar rdf:type ontopowsys_PowerSystemModel:Gs . 
    ?Gsvar rdf:type mathematical_model:Parameter . 
    ?Gsvar ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_EBus mathematical_model:hasModelVariable ?Bsvar . 
    ?Bsvar rdf:type ontopowsys_PowerSystemModel:Bs . 
    ?Bsvar rdf:type mathematical_model:Parameter . 
    ?Bsvar ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_EBus mathematical_model:hasModelVariable ?areavar . 
    ?areavar rdf:type ontopowsys_PowerSystemModel:Area . 
    ?areavar rdf:type mathematical_model:Parameter . 
    ?areavar ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_EBus mathematical_model:hasModelVariable ?BKV . 
    ?BKV rdf:type ontopowsys_PowerSystemModel:baseKV . 
    ?BKV rdf:type mathematical_model:Parameter . 
    ?BKV ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_EBus mathematical_model:hasModelVariable ?zvar . 
    ?zvar rdf:type ontopowsys_PowerSystemModel:Zone . 
    ?zvar rdf:type mathematical_model:Parameter . 
    ?zvar ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_EBus mathematical_model:hasModelVariable ?vmaxvar . 
    ?vmaxvar rdf:type ontopowsys_PowerSystemModel:VmMax . 
    ?vmaxvar rdf:type mathematical_model:Parameter . 
    ?vmaxvar ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_EBus mathematical_model:hasModelVariable ?vminvar . 
    ?vminvar rdf:type ontopowsys_PowerSystemModel:VmMin . 
    ?vminvar rdf:type mathematical_model:Parameter . 
    ?vminvar ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
 
    }
    """ % (selectClause_1, queryVar_1[1], queryVar_1[2], queryVar_1[0], queryVar_1[3], queryVar_1[4], queryVar_1[5], queryVar_1[6], queryVar_1[7], \
        queryVar_1[8], queryVar_1[9], queryVar_1[10])
   
    
  queryBusModelInput = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
    PREFIX mathematical_model: <http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#>
    PREFIX ontopowsys_PowerSystemModel: <http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#>
    PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    SELECT DISTINCT %s
    WHERE
    {
    ?Model_EBus a mathematical_model:Submodel . 
    
    ?Model_EBus mathematical_model:hasModelVariable ?Bus_Number . 
    ?Bus_Number rdf:type ontopowsys_PowerSystemModel:BusNumber . 
    ?Bus_Number rdf:type mathematical_model:Parameter . 
    ?Bus_Number ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
     
    ?Model_EBus mathematical_model:hasModelVariable ?Pd . 
    ?Pd rdf:type ontopowsys_PowerSystemModel:PdBus . 
    ?Pd rdf:type mathematical_model:InputVariable . 
    ?Pd ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_EBus mathematical_model:hasModelVariable ?Gd . 
    ?Gd rdf:type ontopowsys_PowerSystemModel:GdBus . 
    ?Gd rdf:type mathematical_model:InputVariable . 
    ?Gd ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_EBus mathematical_model:hasModelVariable ?VM . 
    ?VM rdf:type ontopowsys_PowerSystemModel:Vm . 
    ?VM rdf:type mathematical_model:InputVariable . 
    ?VM ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_EBus mathematical_model:hasModelVariable ?VA . 
    ?VA rdf:type ontopowsys_PowerSystemModel:Va . 
    ?VA rdf:type mathematical_model:InputVariable . 
    ?VA ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    }
    """% (selectClause_2, queryVar_2[0], queryVar_2[1], queryVar_2[2], queryVar_2[3], queryVar_2[4])
  
  # print(queryBusModelInput)
  # query the Bus model parameters and its GPS location
  start = time.time()
  print('Federated Querying the Bus Model Parameters...')
  res_para = json.loads(performFederatedQuery(queryBusModelParameter_federated, topoEndpoint, busModelEndpoint))
  end = time.time()  
  print('Finished querying the Bus patameter in ',np.round(end-start,2),' seconds') 
  print("The result of the federated query is ")
  for r in res_para:
      for key in r.keys():
          r[key] = (r[key].split('\"^^')[0]).replace('\"','')         
  qres_para = [[ int(r['Bus_num']), float(r['numericalValue_x']), float(r['numericalValue_y']), int(r['Bus_type']), float(r['para_Gs']), float(r['para_Bs']), int(r['para_area']), \
                float(r['para_basekV']), int(r['para_zone']), float(r['para_Vmax']), float(r['para_Vmin'])] for r in res_para ]
  for q in qres_para: 
      if q[3] == 1:
          q[3] = "PQ Bus"
      elif q[3] == 2:  
          q[3] = "PV Bus"
      elif q[3] == 3:  
          q[3] = "Slack Bus" 
  
  # query the Bus model input variables
  start = time.time()
  print('Querying the Bus Model Input Variables...')
  # the performQuery interface needs the label of the endpoint as the function argument which should be registered in advanced in the Blazegraph look-up table
  res_input = json.loads(performQuery("ukpowergridmodel", queryBusModelInput)) 
  end = time.time()  
  print('Finished querying the Bus patameter in ',np.round(end-start,2),' seconds') 
  
  for r in res_input:
      for key in r.keys():
          r[key] = (r[key].split('\"^^')[0]).replace('\"','')
  qres_input = [[ int(r['Bus_num']), float(r['input_Pd']), float(r['input_Gd']), float(r['input_Vm']), float(r['input_Va'])] for r in res_input ]
         
  if len(res_input) == len(res_para) and len(res_input) != 0:
      return qres_para, qres_input
  elif len(res_para) == 0:
      print("The federated query of the bus model parameter is failed.")
      return None
  elif len(res_input) == 0:
      print("The query of the bus model varibles is failed.")
      return None

    
"""This function is used for query the branch model parameters and input variables, also there connectivity relationship with buses"""
# branchModelEndpoint = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridModel", lable: "ukpowergridmodel"
# topoEndpoint = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridTopology", lable: "ukpowergridtopology"
def queryGridModeltForVisualisation_Branch(branchModelEndpoint, topoEndpoint):
    
  queryVar = ["?ELine", "?From_Bus", "?To_Bus", "?para_R", "?para_X", "?para_B", "?para_RateA", "?para_RateB", "?para_RateC", "?para_RatioCoefficient", \
                "?para_Angle", "?para_Status", "?para_AngleMax", "?para_AngleMin"] 
      
  queryGPS = ["?PowerFlow_ELine", "?FromBus_latitude", "?FromBus_longitude", "?ToBus_latitude", "?ToBus_longitude"]
      
  selectClause = " ".join(queryVar)
  selectClause_queryGPS = " ".join(queryGPS)
  
  
  queryFromAndToBusGPSLocation = """
    PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX space_and_time_extended:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    SELECT DISTINCT %s
    WHERE
    {
     
    %s ontocape_network_system:leaves ?EquipmentConnection_FromEBus . 
    ?EquipmentConnection_FromEBus rdf:type ontopowsys_PowSysFunction:PowerEquipmentConnection .
    ?EquipmentConnection_FromEBus space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem_FromBus .
    ?CoordinateSystem_FromBus  space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate_FromBus .
    ?CoordinateSystem_FromBus  space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate_FromBus .
    ?x_coordinate_FromBus  system:hasValue ?GPS_x_coordinate_FromBus .
    ?y_coordinate_FromBus  system:hasValue ?GPS_y_coordinate_FromBus . 
    ?GPS_x_coordinate_FromBus  system:numericalValue %s .
    ?GPS_y_coordinate_FromBus  system:numericalValue %s .
    
    %s ontocape_network_system:enters ?EquipmentConnection_ToEBus . 
    ?EquipmentConnection_ToEBus rdf:type ontopowsys_PowSysFunction:PowerEquipmentConnection .
    ?EquipmentConnection_ToEBus space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem_ToBus .
    ?CoordinateSystem_ToBus  space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate_ToBus .
    ?CoordinateSystem_ToBus  space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate_ToBus .
    ?x_coordinate_ToBus  system:hasValue ?GPS_x_coordinate_ToBus .
    ?y_coordinate_ToBus  system:hasValue ?GPS_y_coordinate_ToBus . 
    ?GPS_x_coordinate_ToBus  system:numericalValue %s .
    ?GPS_y_coordinate_ToBus  system:numericalValue %s .
    }
    """ % (selectClause_queryGPS, queryGPS[0], queryGPS[1], queryGPS[2], queryGPS[0], queryGPS[3], queryGPS[4])
  
  
  queryBuranchModelParameter = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
    PREFIX mathematical_model: <http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#>
    PREFIX ontopowsys_PowerSystemModel: <http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#>
    PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    SELECT DISTINCT %s
    WHERE
    {
    %s ontocape_upper_level_system:isModeledBy ?Model_Eline . 
    
    ?Model_Eline mathematical_model:hasModelVariable ?FromBusNumber . 
    ?FromBusNumber rdf:type ontopowsys_PowerSystemModel:BusFrom . 
    ?FromBusNumber rdf:type mathematical_model:Parameter . 
    ?FromBusNumber ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_Eline mathematical_model:hasModelVariable ?ToBusNumber . 
    ?ToBusNumber rdf:type ontopowsys_PowerSystemModel:BusTo . 
    ?ToBusNumber rdf:type mathematical_model:Parameter . 
    ?ToBusNumber ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_Eline mathematical_model:hasModelVariable ?res . 
    ?res rdf:type ontopowsys_PowerSystemModel:R . 
    ?res rdf:type mathematical_model:Parameter . 
    ?res ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_Eline mathematical_model:hasModelVariable ?rea . 
    ?rea rdf:type ontopowsys_PowerSystemModel:X . 
    ?rea rdf:type mathematical_model:Parameter . 
    ?rea ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_Eline mathematical_model:hasModelVariable ?sus . 
    ?sus rdf:type ontopowsys_PowerSystemModel:B . 
    ?sus rdf:type mathematical_model:Parameter . 
    ?sus ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_Eline mathematical_model:hasModelVariable ?ratea . 
    ?ratea rdf:type ontopowsys_PowerSystemModel:RateA . 
    ?ratea rdf:type mathematical_model:Parameter . 
    ?ratea ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_Eline mathematical_model:hasModelVariable ?rateb . 
    ?rateb rdf:type ontopowsys_PowerSystemModel:RateB . 
    ?rateb rdf:type mathematical_model:Parameter . 
    ?rateb ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_Eline mathematical_model:hasModelVariable ?ratec . 
    ?ratec rdf:type ontopowsys_PowerSystemModel:RateC . 
    ?ratec rdf:type mathematical_model:Parameter . 
    ?ratec ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_Eline mathematical_model:hasModelVariable ?ratio . 
    ?ratio rdf:type ontopowsys_PowerSystemModel:RatioCoefficient . 
    ?ratio rdf:type mathematical_model:Parameter . 
    ?ratio ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .

    ?Model_Eline mathematical_model:hasModelVariable ?ang . 
    ?ang rdf:type ontopowsys_PowerSystemModel:Angle . 
    ?ang rdf:type mathematical_model:Parameter . 
    ?ang ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_Eline mathematical_model:hasModelVariable ?stat . 
    ?stat rdf:type ontopowsys_PowerSystemModel:BranchStatus . 
    ?stat rdf:type mathematical_model:Parameter . 
    ?stat ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_Eline mathematical_model:hasModelVariable ?angmin . 
    ?angmin rdf:type ontopowsys_PowerSystemModel:AngleMin . 
    ?angmin rdf:type mathematical_model:Parameter . 
    ?angmin ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    ?Model_Eline mathematical_model:hasModelVariable ?angmax . 
    ?angmax rdf:type ontopowsys_PowerSystemModel:AngleMax . 
    ?angmax rdf:type mathematical_model:Parameter . 
    ?angmax ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue %s .
    
    }
    """ % (selectClause, queryVar[0], queryVar[1], queryVar[2], queryVar[3], queryVar[4], queryVar[5], queryVar[6], queryVar[7], \
        queryVar[8], queryVar[9], queryVar[10], queryVar[11], queryVar[12], queryVar[13])
  
  # print(queryFromAndToBusGPSLocation)
  # query the Bus model parameters and its GPS location
  
  test = """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
    PREFIX mathematical_model: <http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#>
    PREFIX ontopowsys_PowerSystemModel: <http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#>
    PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    SELECT DISTINCT ?ELine ?From_Bus ?To_Bus ?para_R ?para_X ?para_B
    WHERE
    {
    ?ELine ontocape_upper_level_system:isModeledBy ?Model_Eline . 
    
    ?Model_Eline mathematical_model:hasModelVariable ?FromBusNumber . 
    ?FromBusNumber rdf:type ontopowsys_PowerSystemModel:BusFrom . 
    ?FromBusNumber rdf:type mathematical_model:Parameter . 
    ?FromBusNumber ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?From_Bus .
    
    ?Model_Eline mathematical_model:hasModelVariable ?ToBusNumber . 
    ?ToBusNumber rdf:type ontopowsys_PowerSystemModel:BusTo . 
    ?ToBusNumber rdf:type mathematical_model:Parameter . 
    ?ToBusNumber ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?To_Bus .
    
    ?Model_Eline mathematical_model:hasModelVariable ?res . 
    ?res rdf:type ontopowsys_PowerSystemModel:R . 
    ?res rdf:type mathematical_model:Parameter . 
    ?res ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?para_R .
    
    ?Model_Eline mathematical_model:hasModelVariable ?rea . 
    ?rea rdf:type ontopowsys_PowerSystemModel:X . 
    ?rea rdf:type mathematical_model:Parameter . 
    ?rea ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?para_X .
    
    ?Model_Eline mathematical_model:hasModelVariable ?sus . 
    ?sus rdf:type ontopowsys_PowerSystemModel:B . 
    ?sus rdf:type mathematical_model:Parameter . 
    ?sus ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?para_B .
    
    
    }
    """
  start = time.time()
  print('Querying the Branch Model Parameters (model input)...')
  # res_para = json.loads(performQuery(branchModelEndpoint, queryBuranchModelParameter))
  res_GPS = json.loads(performQuery(topoEndpoint, queryFromAndToBusGPSLocation))
  end = time.time()  
  print('Finished querying in ',np.round(end-start,2),' seconds')
  
  # print(res_GPS)
  # for r in res_GPS:
  #     for key in r.keys():
  #         r[key] = (r[key].split('\"^^')[0]).replace('\"','')

  qres_GPS = [[ str(r['PowerFlow_ELine'].split('PowerFlow_')[1]), float(r['FromBus_latitude']), float(r['FromBus_longitude']), float(r['ToBus_latitude']), float(r['ToBus_longitude'])] for r in res_GPS ]
       
  # qres_para = [[ int(r['Bus_num']), float(r['numericalValue_x']), float(r['numericalValue_y']), int(r['Bus_type']), float(r['para_Gs']), float(r['para_Bs']), int(r['para_area']), \
  #               float(r['para_basekV']), int(r['para_zone']), float(r['para_Vmax']), float(r['para_Vmin'])] for r in res_para ]
  return qres_GPS

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
    topoEndpoint = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridTopology"
    busModelEndpoint = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerGridModel"
    # res = queryUKElectricityConsumptionAndAssociatedGEOInfo(electricity_consumption_RDF4j_Endpoint, ONS_json, False)
    
    # res = queryGridModeltForVisualisation_Bus(topoEndpoint, busModelEndpoint)
    res  = queryGridModeltForVisualisation_Branch("ukpowergridmodel", "ukpowergridtopology")
    # for r in res:
    #     print(r)
    print(type(res))
