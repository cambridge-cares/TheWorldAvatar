##############################################
# Author: Wanni Xie (wx243@cam.ac.uk)        #
# Last Update Date: 11 Oct 2021              #
##############################################

"""This script developed functuions for querying the data from remote triple store or SPARQL endpoints for data visualisation."""

import os, sys, json
# from SPARQLWrapper import SPARQLWrapper, JSON
from tqdm import tqdm
import time
import numpy as np 

from shapely.wkt import loads
from shapely.geometry import mapping
import geojson
import ast
from queryInterface import performQuery, performUpdate, performFederatedQuery

"""query the COMO RDF4j triple store for the UK power plant data from DUKES"""
# The endpoint is: http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin/sparql
# the endpoint label used in the blazegraph is "ukdigitaltwin"
def queryPowerPlantForVisualisation(ukdigitaltwin_label):
    
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
  
  start = time.time()
  print('Querying...')
  ret = json.loads(performQuery(ukdigitaltwin_label, query_UKPowerPlant))
  end = time.time()
  print('Finished in ',np.round(end-start,2),' seconds')
  
  for r in ret:
      for key in r.keys():
          if '\"^^' in  r[key] :
            r[key] = (r[key].split('\"^^')[0]).replace('\"','')    
  
  ret_array = [[ str(r['powerPlantIRI'].split('#')[1]), str(r['numericalValue_x']), str(r['numericalValue_y']), str(r['Primary_Fuel_type'].split('#')[1]), \
                 str(r['Plant_Generation_Technology'].split('#')[1]), float(r['value_of_Designed_Capacity']), str(r['Owner']), int(r['Year_of_Build'])] for r in ret ]
  
  return ret_array

"""Query the UK electricity consumption data (total, domestic, industrial&conmercial) and its associated geo information, i.e. the boundaries of the places"""
# ukdigitaltwin: http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin/sparql
# ONS_GEO_Info_endpoint: http://statistics.data.gov.uk/sparql.json
# TODO:check the dbpedia, can use the centroid of the place from the dbpedia 
def queryUKElectricityConsumptionAndAssociatedGEOInfo(ukdigitaltwin, ONS, startTime_of_EnergyConsumption, regionOrArea):

  # the query string for querying the electricity consumption of 11 official regions
  query_region = """  
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontoeip_system_function: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_function.owl#>
    PREFIX mathematical_relation: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/mathematical_relation/mathematical_relation.owl#>
    PREFIX db: <https://dbpedia.org/ontology/>
    PREFIX bibtex: <https://zeitkunst.org/bibtex/0.2/bibtex.owl#> 
    # PREFIX bibtex: <http://purl.org/net/nknouf/ns/bibtex#>
    PREFIX ont: <http://www.opengis.net/ont/geosparql#>
    PREFIX ont_sparql: <http://www.opengis.net/ont/geosparql#>
    PREFIX ontocape_derived_SI_units: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#>
    PREFIX ontoecape_space_and_time: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#>
    PREFIX ontocape_coordinate_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#>
    SELECT DISTINCT ?Location ?Area_LACode ?Area_id_url ?Total_Electricity_Consumption ?Domestic_Electricity_Consumption ?Industrial_and_Commercial_Electricity_Consumption (GROUP_CONCAT(?Geo_Info;SEPARATOR = '***') AS ?Geo_InfoList)
    WHERE
    {
    ?Area ontocape_upper_level_system:hasAddress ?Location .
    ?Location rdf:type <https://dbpedia.org/ontology/Region> . 
    ?Location db:areaCode ?Area_LACode .
    ?Location bibtex:hasURL ?Area_id_url .
    
    ?Area ontoeip_system_function:consumes ?Total_ele_consumption . 
    ?Total_ele_consumption ontocape_derived_SI_units:hasTimePeriod/ontocape_upper_level_system:hasValue ?TimePeriod .
    ?TimePeriod ontoecape_space_and_time:hasStartingTime ?startTime .
    ?startTime rdf:type ontocape_coordinate_system:CoordinateValue . 
    ?startTime ontocape_upper_level_system:numericalValue "%s"^^xsd:dateTime .
     
    ?Area ontoeip_system_function:consumes/ontocape_upper_level_system:hasValue ?v_TotalELecConsumption .   
    ?v_TotalELecConsumption ontocape_upper_level_system:numericalValue ?Total_Electricity_Consumption .

    ?Area ontoeip_system_function:consumes/mathematical_relation:ConsistsOfDemesticElectricityConsumption ?Domestic . 
    ?Domestic ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Domestic_Electricity_Consumption . 
    
    ?Area ontoeip_system_function:consumes/mathematical_relation:ConsistsOfIndustrialAndCommercialConsumption ?Industrial_and_Commercial . 
    ?Industrial_and_Commercial ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Industrial_and_Commercial_Electricity_Consumption . 
    
    OPTIONAL { ?Area_id_url a <http://statistics.data.gov.uk/def/statistical-geography#Statistical-Geography> .
    ?Area_id_url ont:hasGeometry ?geometry . 
    ?geometry ont_sparql:asWKT ?Geo_Info . }      
    
    } GROUP BY ?Location ?Area_LACode ?Area_id_url ?Total_Electricity_Consumption ?Domestic_Electricity_Consumption ?Industrial_and_Commercial_Electricity_Consumption

    """ %startTime_of_EnergyConsumption
    
  # the query string for querying the electricity consumption of areas
  query_area = """   
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontoeip_system_function: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_function.owl#>
    PREFIX mathematical_relation: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/mathematical_relation/mathematical_relation.owl#>
    PREFIX db: <https://dbpedia.org/ontology/>
    PREFIX bibtex: <https://zeitkunst.org/bibtex/0.2/bibtex.owl#> 
    # PREFIX bibtex: <http://purl.org/net/nknouf/ns/bibtex#>
    PREFIX ont: <http://www.opengis.net/ont/geosparql#>
    PREFIX ont_sparql: <http://www.opengis.net/ont/geosparql#>
    PREFIX ontocape_derived_SI_units: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#>
    PREFIX ontoecape_space_and_time: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#>
    PREFIX ontocape_coordinate_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#>
    SELECT DISTINCT ?Location ?Area_LACode ?Area_id_url ?Total_Electricity_Consumption ?Domestic_Electricity_Consumption ?Industrial_and_Commercial_Electricity_Consumption (GROUP_CONCAT(?Geo_Info;SEPARATOR = '***') AS ?Geo_InfoList) 
    WHERE
    {
    ?Area ontocape_upper_level_system:hasAddress ?Location .
    ?Location rdf:type <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#AddressArea> . 
    ?Location db:areaCode ?Area_LACode .
    ?Location bibtex:hasURL ?Area_id_url .
    
    ?Area ontoeip_system_function:consumes ?Total_ele_consumption . 
    ?Total_ele_consumption ontocape_derived_SI_units:hasTimePeriod/ontocape_upper_level_system:hasValue ?TimePeriod .
    ?TimePeriod ontoecape_space_and_time:hasStartingTime ?startTime .
    ?startTime rdf:type ontocape_coordinate_system:CoordinateValue . 
    ?startTime ontocape_upper_level_system:numericalValue "%s"^^xsd:dateTime .
     
    ?Area ontoeip_system_function:consumes/ontocape_upper_level_system:hasValue ?v_TotalELecConsumption .   
    ?v_TotalELecConsumption ontocape_upper_level_system:numericalValue ?Total_Electricity_Consumption .

    ?Area ontoeip_system_function:consumes/mathematical_relation:ConsistsOfDemesticElectricityConsumption ?Domestic . 
    ?Domestic ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Domestic_Electricity_Consumption . 
    
    ?Area ontoeip_system_function:consumes/mathematical_relation:ConsistsOfIndustrialAndCommercialConsumption ?Industrial_and_Commercial . 
    ?Industrial_and_Commercial ontocape_upper_level_system:hasValue/ontocape_upper_level_system:numericalValue ?Industrial_and_Commercial_Electricity_Consumption . 
    
    OPTIONAL { ?Area_id_url a <http://statistics.data.gov.uk/def/statistical-geography#Statistical-Geography> .
    ?Area_id_url ont:hasGeometry ?geometry . 
    ?geometry ont_sparql:asWKT ?Geo_Info . }      
    
    } GROUP BY ?Location ?Area_LACode ?Area_id_url ?Total_Electricity_Consumption ?Domestic_Electricity_Consumption ?Industrial_and_Commercial_Electricity_Consumption

    """ %startTime_of_EnergyConsumption
    
  start = time.time()
  print("Federated Querying ONS and UK Digital Twin...")
  if regionOrArea == True:    
      ret = json.loads(performFederatedQuery(query_region, ukdigitaltwin, ONS))    
  else:
      ret = json.loads(performFederatedQuery(query_area, ukdigitaltwin, ONS))         
  end = time.time()
  if len(ret) == 0:
      raise Exception("The query is failed.")
  else:
      print("Query is done. The length of query results is: ", len(ret))

  counter = 0
  Num_no_geoInfoAreas = 0
  No_geoInfoAreas = []
  ret_array = np.zeros((len(ret), 6), dtype='object')
      
  for r in ret:
      for key in r.keys():
          if '\"^^' in  r[key] :
            r[key] = (r[key].split('\"^^')[0]).replace('\"','') 
             
  for i in tqdm(range(len(ret))):
      print(counter, ":", ret[i]["Area_id_url"])
      if len(ret[i]["Geo_InfoList"]) == 0:
          print(ret[i]["Area_id_url"], "does't have the geographical attributes.")
          Num_no_geoInfoAreas += 1
          No_geoInfoAreas.append(ret[i]["Area_id_url"])
          continue
      if "***" in ret[i]['Geo_InfoList']:
        ret[i]['Geo_InfoList'] = ret[i]['Geo_InfoList'].split("***")[0]
      geojson_string = geojson.dumps(mapping(loads(ret[i]['Geo_InfoList'])))
      ret[i]['Geo_InfoList'] = ast.literal_eval(geojson_string)  
      ret_array[i,:] =[ str(ret[i]['Location'].split("page/")[1]), str(ret[i]['Area_LACode']), float(ret[i]['Total_Electricity_Consumption']), float(ret[i]['Domestic_Electricity_Consumption']), \
                 float(ret[i]['Industrial_and_Commercial_Electricity_Consumption']), ret[i]['Geo_InfoList']] 
      counter += 1
  
  print("******************The query results report******************")
  print('Finished in ',np.round(end-start,2),' seconds')  
  print("The total number of the areas are: ", counter)
  print("The number of the areas don't have the geometry attibutes are: ", Num_no_geoInfoAreas, " which are listed as follow: ")
  print(No_geoInfoAreas)
  
  return ret_array 

# TODO: a temporary function only used for visualise the bus location
def queryBusGPSLocation(ukdigitaltwin_label, numOfBus):
    label = "_" + str(numOfBus) + "_"
    queryStr = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
    PREFIX mathematical_model: <http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#>
    PREFIX ontopowsys_PowerSystemModel: <http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#>
    PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    SELECT DISTINCT ?EquipmentConnection_EBus ?numericalValue_y ?numericalValue_x 
    WHERE
    {    
    ?Topology rdf:type ontocape_network_system:NetworkSystem .
    ?Topology rdfs:label ?label .
    FILTER regex(?label, "%s") .
    ?Topology ontocape_upper_level_system:isComposedOfSubsystem ?EquipmentConnection_EBus .
    
    ?EquipmentConnection_EBus rdf:type ontopowsys_PowSysFunction:PowerEquipmentConnection .      
    ?EquipmentConnection_EBus space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem_Bus .    
    ?CoordinateSystem_Bus  space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate_Bus .
    ?CoordinateSystem_Bus  space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate_Bus .
    ?x_coordinate_Bus  ontocape_upper_level_system:hasValue ?GPS_x_coordinate_Bus .
    ?y_coordinate_Bus  ontocape_upper_level_system:hasValue ?GPS_y_coordinate_Bus . 
    ?GPS_x_coordinate_Bus  ontocape_upper_level_system:numericalValue ?numericalValue_x .
    ?GPS_y_coordinate_Bus  ontocape_upper_level_system:numericalValue ?numericalValue_y .
    }
    """% label
          
    start = time.time()
    print('Querying the Bus location...')
    res = json.loads(performQuery(ukdigitaltwin_label, queryStr))
    # print(res_para)
    end = time.time()  
    print('Finished querying the Bus location in ',np.round(end-start,2),' seconds') 
    for r in res:
        for key in r.keys():
            r[key] = (r[key].split('\"^^')[0]).replace('\"','')         
    qres_busLocation = [[ int(r['EquipmentConnection_EBus'].split('EBus-')[1]), float(r['numericalValue_x']), float(r['numericalValue_y']) ] for r in res ]
    
    return qres_busLocation

# TODO: a temporary function only used for visualise the branches
def queryBranchConnectedGPSLocation(ukdigitaltwin_label, numOfBus):
    label = "_" + str(numOfBus) + "_"
    queryStr = """
    PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX space_and_time_extended:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    SELECT DISTINCT ?PowerFlow_ELine ?FromBus_latitude ?FromBus_longitude ?ToBus_latitude ?ToBus_longitude 
    WHERE
    {
    ?Topology rdf:type ontocape_network_system:NetworkSystem .
    ?Topology rdfs:label ?label .
    FILTER regex(?label, "%s") .
    ?Topology ontocape_upper_level_system:isComposedOfSubsystem ?PowerFlow_ELine .
    ?PowerFlow_ELine rdf:type ontopowsys_PowSysFunction:PowerFlow .  
    
    ?PowerFlow_ELine ontocape_network_system:leaves ?From_EquipmentConnection_EBus .    
    ?From_EquipmentConnection_EBus space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem_FromBus .
    ?From_EquipmentConnection_EBus rdf:type ontopowsys_PowSysFunction:PowerEquipmentConnection .
    ?CoordinateSystem_FromBus  space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate_FromBus .
    ?CoordinateSystem_FromBus  space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate_FromBus .
    ?x_coordinate_FromBus  system:hasValue ?GPS_x_coordinate_FromBus .
    ?y_coordinate_FromBus  system:hasValue ?GPS_y_coordinate_FromBus . 
    ?GPS_x_coordinate_FromBus  system:numericalValue ?FromBus_longitude .
    ?GPS_y_coordinate_FromBus  system:numericalValue ?FromBus_latitude .
    
    ?PowerFlow_ELine ontocape_network_system:enters ?To_EquipmentConnection_EBus .    
    ?To_EquipmentConnection_EBus space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem_ToBus .
    ?To_EquipmentConnection_EBus rdf:type ontopowsys_PowSysFunction:PowerEquipmentConnection .
    ?CoordinateSystem_ToBus  space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate_ToBus .
    ?CoordinateSystem_ToBus  space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate_ToBus .
    ?x_coordinate_ToBus  system:hasValue ?GPS_x_coordinate_ToBus .
    ?y_coordinate_ToBus  system:hasValue ?GPS_y_coordinate_ToBus . 
    ?GPS_x_coordinate_ToBus  system:numericalValue ?ToBus_longitude .
    ?GPS_y_coordinate_ToBus  system:numericalValue ?ToBus_latitude .
    }
    """ % label
          
    start = time.time()
    print('Querying branch connected GPS location...')
    res = json.loads(performQuery(ukdigitaltwin_label, queryStr))
    # print(res_para)
    end = time.time()  
    print('Finished querying branch connected GPS location in ',np.round(end-start,2),' seconds') 
    for r in res:
        for key in r.keys():
            r[key] = (r[key].split('\"^^')[0]).replace('\"','')         
    qres_branchLocation = [[ int(r['PowerFlow_ELine'].split('ELine-')[1]), float(r['FromBus_longitude']), float(r['FromBus_latitude']), float(r['ToBus_longitude']), float(r['ToBus_latitude']) ] for r in res ]   
    return qres_branchLocation
      
# TODO: add numOfBus and merge the query strings; needs to modify the 10 bus visualisation html
"""This function is used for query the bus model parameters and input variables from the grid model endpoint and query the location of the bus from the grid topology endpoint"""
# ukdigitaltwin_label = "ukdigitaltwin"
def queryGridModeltForVisualisation_Bus(ukdigitaltwin_label):
    
  queryVar_1 = ["?Bus_num", "?Located_City", "?numericalValue_x", "?numericalValue_y", "?Bus_type", "?para_Gs", "?para_Bs", "?para_area", "?para_basekV", \
                "?para_zone", "?para_Vmax", "?para_Vmin"] 
      
  queryVar_2 = ["?Bus_num", "?input_Pd", "?input_Gd", "?input_Vm", "?input_Va"] 
  
      
  selectClause_1 = " ".join(queryVar_1)
  selectClause_2 = " ".join(queryVar_2)
  
  queryBusModelParameter = """
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
    
    ?EquipmentConnection_EBus ontocape_upper_level_system:hasAddress %s .
    %s rdf:type <https://dbpedia.org/ontology/Region> .
    
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
    """ % (selectClause_1, queryVar_1[1], queryVar_1[1], queryVar_1[2], queryVar_1[3], queryVar_1[0], queryVar_1[4], queryVar_1[5], queryVar_1[6], queryVar_1[7], \
        queryVar_1[8], queryVar_1[9], queryVar_1[10], queryVar_1[11])
   
  #TODO: to be discard  
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
   

  print(queryBusModelParameter)       
  start = time.time()
  print('Querying the Bus Model Parameters...')
  res_para = json.loads(performQuery(ukdigitaltwin_label, queryBusModelParameter))
  print(res_para)
  end = time.time()  
  print('Finished querying the Bus patameter in ',np.round(end-start,2),' seconds') 
  for r in res_para:
      for key in r.keys():
          r[key] = (r[key].split('\"^^')[0]).replace('\"','')         
  qres_para = [[ int(r['Bus_num']), str(r["Located_City"]), float(r['numericalValue_x']), float(r['numericalValue_y']), int(r['Bus_type']), float(r['para_Gs']), float(r['para_Bs']), int(r['para_area']), \
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
  res_input = json.loads(performQuery(ukdigitaltwin_label, queryBusModelInput)) 
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
  

"""This function is designed for query the geo information of the bus located regions"""
def queryBusLocatedRegionGeoInfo(ukdigitaltwin, ONS):
  query_BusLocatedRegion = """
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>    
    PREFIX db: <https://dbpedia.org/ontology/>
    PREFIX bibtex: <http://purl.org/net/nknouf/ns/bibtex#>
    PREFIX ont: <http://www.opengis.net/ont/geosparql#>
    PREFIX ont_sparql: <http://www.opengis.net/ont/geosparql#>
    SELECT DISTINCT ?EquipmentConnection_EBus ?Area_id_url ?Geo_Info
    WHERE
    {
    ?EquipmentConnection_EBus a ontopowsys_PowSysFunction:PowerEquipmentConnection . 
    ?EquipmentConnection_EBus ontocape_upper_level_system:hasAddress/rdf:type <https://dbpedia.org/ontology/Region> .
    ?EquipmentConnection_EBus ontocape_upper_level_system:hasAddress/bibtex:hasURL ?Area_id_url .
    
    OPTIONAL { ?Area_id_url a <http://statistics.data.gov.uk/def/statistical-geography#Statistical-Geography> .
    ?Area_id_url ont:hasGeometry ?geometry . 
    ?geometry ont_sparql:asWKT ?Geo_Info . }      
    }  
    """
    
  start = time.time()
  print("Federated Querying ONS and bus location...") 
  ret = json.loads(performFederatedQuery(query_BusLocatedRegion, ukdigitaltwin, ONS))            
  end = time.time()
  print("Query is done.")
  print('Finished querying Bus Located Region in ',np.round(end-start,2),' seconds')
  
  for r in ret:
      for key in r.keys():
          if '\"^^' in  r[key] :
            r[key] = (r[key].split('\"^^')[0]).replace('\"','')            
  
  counter = 0
  Num_no_geoInfoAreas = 0
  No_geoInfoAreas = []
  ret_array = np.zeros((len(ret), 2), dtype='object')
      
  for i in tqdm(range(len(ret))):
      print(counter, ' ', ret[i]["Area_id_url"])
      # print(ret[i]['Geo_Info'])
      if len(ret[i]["Geo_Info"]) == 0:
          print(ret[i]["Area_id_url"], "does't have the geographical attributes.")
          Num_no_geoInfoAreas += 1
          No_geoInfoAreas.append(ret[i]["Area_id_url"])
          continue
     
      geojson_string = geojson.dumps(mapping(loads(ret[i]['Geo_Info'])))
      ret[i]['Geo_Info'] = ast.literal_eval(geojson_string)  
      ret_array[i,:] =[ int(ret[i]['EquipmentConnection_EBus'].split("EBus-")[1]), ret[i]['Geo_Info']] 
      counter += 1
  return ret_array
    

"""This function is used for query the branch model parameters and input variables, also there connectivity relationship with buses"""
# ukdigitaltwin_label = "ukdigitaltwin"
def queryGridModeltForVisualisation_Branch(ukdigitaltwin_label):
  
  queryVar = ["?PowerFlow_ELine", "?FromBus_latitude", "?FromBus_longitude", "?ToBus_latitude", "?ToBus_longitude", "?From_Bus", "?To_Bus", "?para_R", "?para_X", "?para_B", "?para_RateA", "?para_RateB", "?para_RateC", "?para_RatioCoefficient", \
                "?para_Angle", "?para_Status", "?para_AngleMin", "?para_AngleMax"] 
      
  selectClause = " ".join(queryVar)
    
  queryBranch = """
    PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX space_and_time_extended:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>   
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>   
    PREFIX mathematical_model: <http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#>
    PREFIX ontopowsys_PowerSystemModel: <http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#>
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
    
    %s ontoecape_technical_system:isRealizedBy ?ELine . 
    ?ELine rdf:type ontopowsys_PowSysRealization:OverheadLine .
    ?ELine ontocape_upper_level_system:isModeledBy ?Model_Eline . 
    
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
    """ % (selectClause, queryVar[0], queryVar[1], queryVar[2], queryVar[0], queryVar[3], queryVar[4], queryVar[0], queryVar[5], queryVar[6], queryVar[7], \
        queryVar[8], queryVar[9], queryVar[10], queryVar[11], queryVar[12], queryVar[13], queryVar[14], queryVar[15], queryVar[16], queryVar[17], )
 
  start = time.time()
  print('Querying the Branch GPS information ...')
  res = json.loads(performQuery(ukdigitaltwin_label, queryBranch))
  end = time.time()  
  print('Finished querying in ',np.round(end-start,2),' seconds')
        
  qres = [[ str(r['PowerFlow_ELine'].split('PowerFlow_')[1]), float(r['FromBus_latitude']), float(r['FromBus_longitude']), float(r['ToBus_latitude']), \
           float(r['ToBus_longitude']),  int(r['From_Bus']), int(r['To_Bus']), float(format(float(r['para_R']), ".7f")), float(format(float(r['para_X']), ".5f")), \
           float(format(float(r['para_B']), ".5f")), float(format(float(r['para_RateA']), ".5f")), float(r['para_RateB']), float(r['para_RateC']), float(r['para_RatioCoefficient']), float(r['para_Angle']), int(r['para_Status']), float(r['para_AngleMax']), float(r['para_AngleMin'])] for r in res ]

  print("The length of the query result is", len(qres), ". The length should be same as the number of the branches in the model.")

  return qres

"""This function is used for query the generator model parameters and input variables, also there connectivity relationship with buses"""
# ukdigitaltwin_label = "ukdigitaltwin"
def queryGridModeltForVisualisation_Generator(ukdigitaltwin_label, numOfBus):
  
  queryVar = ["?PowerFlow_ELine", "?FromBus_latitude", "?FromBus_longitude", "?ToBus_latitude", "?ToBus_longitude", "?From_Bus", "?To_Bus", "?para_R", "?para_X", "?para_B", "?para_RateA", "?para_RateB", "?para_RateC", "?para_RatioCoefficient", \
                "?para_Angle", "?para_Status", "?para_AngleMin", "?para_AngleMax"] 
      
  selectClause = " ".join(queryVar)
    
  queryBranch = """
    PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX space_and_time_extended:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>   
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>   
    PREFIX mathematical_model: <http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#>
    PREFIX ontopowsys_PowerSystemModel: <http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#>
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
    
    %s ontoecape_technical_system:isRealizedBy ?ELine . 
    ?ELine rdf:type ontopowsys_PowSysRealization:OverheadLine .
    ?ELine ontocape_upper_level_system:isModeledBy ?Model_Eline . 
    
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
    """ % (selectClause, queryVar[0], queryVar[1], queryVar[2], queryVar[0], queryVar[3], queryVar[4], queryVar[0], queryVar[5], queryVar[6], queryVar[7], \
        queryVar[8], queryVar[9], queryVar[10], queryVar[11], queryVar[12], queryVar[13], queryVar[14], queryVar[15], queryVar[16], queryVar[17], )
 
  start = time.time()
  print('Querying the Branch GPS information ...')
  res = json.loads(performQuery(ukdigitaltwin_label, queryBranch))
  end = time.time()  
  print('Finished querying in ',np.round(end-start,2),' seconds')
        
  qres = [[ str(r['PowerFlow_ELine'].split('PowerFlow_')[1]), float(r['FromBus_latitude']), float(r['FromBus_longitude']), float(r['ToBus_latitude']), \
           float(r['ToBus_longitude']),  int(r['From_Bus']), int(r['To_Bus']), float(format(float(r['para_R']), ".7f")), float(format(float(r['para_X']), ".5f")), \
           float(format(float(r['para_B']), ".5f")), float(format(float(r['para_RateA']), ".5f")), float(r['para_RateB']), float(r['para_RateC']), float(r['para_RatioCoefficient']), float(r['para_Angle']), int(r['para_Status']), float(r['para_AngleMax']), float(r['para_AngleMin'])] for r in res ]

  print("The length of the query result is", len(qres), ". The length should be same as the number of the branches in the model.")

  return qres

#TODO: temporary for visualising the gen clustering of 29_bus model
def queryGeneratorLocation(ukdigitaltwin_label, numOfBus):
    label = "_" + str(numOfBus) + "_"
    queryStr = """
    PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX ontocape_network_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#>
    PREFIX ontopowsys_PowSysFunction: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX space_and_time_extended:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
    PREFIX meta_model_topology: <http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
    SELECT DISTINCT ?PowerGeneration_EGen ?EquipmentConnection_EBus ?numericalValue_x ?numericalValue_y
    WHERE
    {
    ?Topology rdf:type ontocape_network_system:NetworkSystem .
    ?Topology rdfs:label ?label .
    FILTER regex(?label, "%s") .
    ?Topology ontocape_upper_level_system:isComposedOfSubsystem ?PowerGeneration_EGen .
    ?PowerGeneration_EGen rdf:type ontopowsys_PowSysFunction:PowerGeneration .  
    
    ?PowerGeneration_EGen meta_model_topology:isConnectedTo ?EquipmentConnection_EBus .
    ?EquipmentConnection_EBus a ontopowsys_PowSysFunction:PowerEquipmentConnection . 
    
    ?PowerGeneration_EGen ontoecape_technical_system:isRealizedBy/ontocape_upper_level_system:isExclusivelySubsystemOf ?pg_plant . 
    ?pg_plant a ontoeip_powerplant:PowerGenerator .
    ?PowerPlant ontoecape_technical_system:hasRealizationAspect ?pg_plant .
    ?PowerPlant a ontoeip_powerplant:PowerPlant .
    
    ?PowerPlant space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem .
    ?CoordinateSystem space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate .
    ?CoordinateSystem space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate .
    ?x_coordinate ontocape_upper_level_system:hasValue ?GPS_x_coordinate .
    ?y_coordinate ontocape_upper_level_system:hasValue ?GPS_y_coordinate . 
    ?GPS_x_coordinate ontocape_upper_level_system:numericalValue ?numericalValue_x . # longitude is east/west
    ?GPS_y_coordinate ontocape_upper_level_system:numericalValue ?numericalValue_y . # latitude is north/south
    }
    """ % label
          
    start = time.time()
    print('Querying Generator Location...')
    res = json.loads(performQuery(ukdigitaltwin_label, queryStr))
    # print(res_para)
    end = time.time()  
    print('Finished querying Generator Location in ',np.round(end-start,2),' seconds') 
    for r in res:
        for key in r.keys():
            r[key] = (r[key].split('\"^^')[0]).replace('\"','')         
    qres_genLocation = [[ int(r['PowerGeneration_EGen'].split('EGen-')[1]), int(r['EquipmentConnection_EBus'].split('EBus-')[1]), float(r['numericalValue_x']), float(r['numericalValue_y'])] for r in res ]
    
    return qres_genLocation


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



def test_queryLACode(lacode, ukdigitaltwin, startTime_of_EnergyConsumption):
    query_area = """   
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
    PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
    PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
    PREFIX ontoeip_system_function: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_function.owl#>
    PREFIX mathematical_relation: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/mathematical_relation/mathematical_relation.owl#>
    PREFIX db: <https://dbpedia.org/ontology/>
    PREFIX bibtex: <https://zeitkunst.org/bibtex/0.2/bibtex.owl#> 
    PREFIX ont: <http://www.opengis.net/ont/geosparql#>
    PREFIX ont_sparql: <http://www.opengis.net/ont/geosparql#>
    PREFIX ontocape_derived_SI_units: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#>
    PREFIX ontoecape_space_and_time: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#>
    PREFIX ontocape_coordinate_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#>
    SELECT DISTINCT ?Area_LACode 
    WHERE
    {
    ?Area ontocape_upper_level_system:hasAddress ?Location .
    ?Location db:areaCode ?Area_LACode .
    ?Location bibtex:hasURL ?Area_id_url .
    
    ?Area ontoeip_system_function:consumes ?Total_ele_consumption . 
    ?Total_ele_consumption ontocape_derived_SI_units:hasTimePeriod/ontocape_upper_level_system:hasValue ?TimePeriod .
    ?TimePeriod ontoecape_space_and_time:hasStartingTime ?startTime .
    ?startTime rdf:type ontocape_coordinate_system:CoordinateValue . 
    ?startTime ontocape_upper_level_system:numericalValue "%s"^^xsd:dateTime .
    } 
    """ %startTime_of_EnergyConsumption
    ret = json.loads(performFederatedQuery(query_area, ukdigitaltwin)) 
    print('The length of the query results is: ', len(ret))   
    for r in ret:
     for key in r.keys():
         if '\"^^' in  r[key] :
           r[key] = (r[key].split('\"^^')[0]).replace('\"','') 
           
    res = [ str(r['Area_LACode']) for r in ret ]       
    
    for r in res:
        if r in lacode:
            lacode.remove(r)
    
    return lacode

# LA code of year of 2019
lacode = ['E06000001', 'E06000002', 'E06000003', 'E06000004', 'E06000005', 'E06000047', 'E06000057', 'E08000021', 'E08000022', 'E08000023', 'E08000024', 'E08000037', 'E12000001', 'E06000006', 'E06000007', 'E06000008', 'E06000009', 'E06000049', 'E06000050', 'E07000026', 'E07000027', 'E07000028', 'E07000029', 'E07000030', 'E07000031', 'E07000117', 'E07000118', 'E07000119', 'E07000120', 'E07000121', 'E07000122', 'E07000123', 'E07000124', 'E07000125', 'E07000126', 'E07000127', 'E07000128', 'E08000001', 'E08000002', 'E08000003', 'E08000004', 'E08000005', 'E08000006', 'E08000007', 'E08000008', 'E08000009', 'E08000010', 'E08000011', 'E08000012', 'E08000013', 'E08000014', 'E08000015', 'E12000002', 'E06000010', 'E06000011', 'E06000012', 'E06000013', 'E06000014', 'E07000163', 'E07000164', 'E07000165', 'E07000166', 'E07000167', 'E07000168', 'E07000169', 'E08000016', 'E08000017', 'E08000018', 'E08000019', 'E08000032', 'E08000033', 'E08000034', 'E08000035', 'E08000036', 'E12000003', 'E06000015', 'E06000016', 'E06000017', 'E06000018', 'E07000032', 'E07000033', 'E07000034', 'E07000035', 'E07000036', 'E07000037', 'E07000038', 'E07000039', 'E07000129', 'E07000130', 'E07000131', 'E07000132', 'E07000133', 'E07000134', 'E07000135', 'E07000136', 'E07000137', 'E07000138', 'E07000139', 'E07000140', 'E07000141', 'E07000142', 'E07000150', 'E07000151', 'E07000152', 'E07000153', 'E07000154', 'E07000155', 'E07000156', 'E07000170', 'E07000171', 'E07000172', 'E07000173', 'E07000174', 'E07000175', 'E07000176', 'E12000004', 'E06000019', 'E06000020', 'E06000021', 'E06000051', 'E07000192', 'E07000193', 'E07000194', 'E07000195', 'E07000196', 'E07000197', 'E07000198', 'E07000199', 'E07000218', 'E07000219', 'E07000220', 'E07000221', 'E07000222', 'E07000234', 'E07000235', 'E07000236', 'E07000237', 'E07000238', 'E07000239', 'E08000025', 'E08000026', 'E08000027', 'E08000028', 'E08000029', 'E08000030', 'E08000031', 'E12000005', 'E06000031', 'E06000032', 'E06000033', 'E06000034', 'E06000055', 'E06000056', 'E07000008', 'E07000009', 'E07000010', 'E07000011', 'E07000012', 'E07000066', 'E07000067', 'E07000068', 'E07000069', 'E07000070', 'E07000071', 'E07000072', 'E07000073', 'E07000074', 'E07000075', 'E07000076', 'E07000077', 'E07000095', 'E07000096', 'E07000098', 'E07000099', 'E07000102', 'E07000103', 'E07000143', 'E07000144', 'E07000145', 'E07000146', 'E07000147', 'E07000148', 'E07000149', 'E07000200', 'E07000202', 'E07000203', 'E07000240', 'E07000241', 'E07000242', 'E07000243', 'E07000244', 'E07000245', 'E12000006', 'E09000001', 'E09000007', 'E09000012', 'E09000013', 'E09000014', 'E09000019', 'E09000020', 'E09000022', 'E09000023', 'E09000025', 'E09000028', 'E09000030', 'E09000032', 'E09000033', 'E13000001', 'E09000002', 'E09000003', 'E09000004', 'E09000005', 'E09000006', 'E09000008', 'E09000009', 'E09000010', 'E09000011', 'E09000015', 'E09000016', 'E09000017', 'E09000018', 'E09000021', 'E09000024', 'E09000026', 'E09000027', 'E09000029', 'E09000031', 'E13000002', 'E12000007', 'E06000035', 'E06000036', 'E06000037', 'E06000038', 'E06000039', 'E06000040', 'E06000041', 'E06000042', 'E06000043', 'E06000044', 'E06000045', 'E06000046', 'E06000060', 'E07000061', 'E07000062', 'E07000063', 'E07000064', 'E07000065', 'E07000084', 'E07000085', 'E07000086', 'E07000087', 'E07000088', 'E07000089', 'E07000090', 'E07000091', 'E07000092', 'E07000093', 'E07000094', 'E07000105', 'E07000106', 'E07000107', 'E07000108', 'E07000109', 'E07000110', 'E07000111', 'E07000112', 'E07000113', 'E07000114', 'E07000115', 'E07000116', 'E07000177', 'E07000178', 'E07000179', 'E07000180', 'E07000181', 'E07000207', 'E07000208', 'E07000209', 'E07000210', 'E07000211', 'E07000212', 'E07000213', 'E07000214', 'E07000215', 'E07000216', 'E07000217', 'E07000223', 'E07000224', 'E07000225', 'E07000226', 'E07000227', 'E07000228', 'E07000229', 'E12000008', 'E06000022', 'E06000023', 'E06000024', 'E06000025', 'E06000026', 'E06000027', 'E06000030', 'E06000052', 'E06000054', 'E06000058', 'E06000059', 'E07000040', 'E07000041', 'E07000042', 'E07000043', 'E07000044', 'E07000045', 'E07000046', 'E07000047', 'E07000078', 'E07000079', 'E07000080', 'E07000081', 'E07000082', 'E07000083', 'E07000187', 'E07000188', 'E07000189', 'E06000053', 'E07000246', 'E12000009', 'W06000001', 'W06000002', 'W06000003', 'W06000004', 'W06000005', 'W06000006', 'W06000008', 'W06000009', 'W06000010', 'W06000011', 'W06000012', 'W06000013', 'W06000014', 'W06000015', 'W06000016', 'W06000018', 'W06000019', 'W06000020', 'W06000021', 'W06000022', 'W06000023', 'W06000024', 'W92000004', 'S12000005', 'S12000006', 'S12000008', 'S12000010', 'S12000011', 'S12000013', 'S12000014', 'S12000017', 'S12000018', 'S12000019', 'S12000020', 'S12000021', 'S12000023', 'S12000026', 'S12000027', 'S12000028', 'S12000029', 'S12000030', 'S12000033', 'S12000034', 'S12000035', 'S12000036', 'S12000038', 'S12000039', 'S12000040', 'S12000041', 'S12000042', 'S12000045', 'S12000047', 'S12000048', 'S12000049', 'S12000050', 'S92000003']

# 2017 missing areas
missing_area_2017 = ['http://statistics.data.gov.uk/id/statistical-geography/E07000007', 'http://statistics.data.gov.uk/id/statistical-geography/E07000206', 'http://statistics.data.gov.uk/id/statistical-geography/E07000005', 'http://statistics.data.gov.uk/id/statistical-geography/E06000028', 'http://statistics.data.gov.uk/id/statistical-geography/E07000052', 'http://statistics.data.gov.uk/id/statistical-geography/E07000191', 'http://statistics.data.gov.uk/id/statistical-geography/E07000050', 'http://statistics.data.gov.uk/id/statistical-geography/E07000049', 'http://statistics.data.gov.uk/id/statistical-geography/E07000205', 'http://statistics.data.gov.uk/id/statistical-geography/E07000053', 'http://statistics.data.gov.uk/id/statistical-geography/S12000044', 'http://statistics.data.gov.uk/id/statistical-geography/E07000048', 'http://statistics.data.gov.uk/id/statistical-geography/E07000204', 'http://statistics.data.gov.uk/id/statistical-geography/E07000004', 'http://statistics.data.gov.uk/id/statistical-geography/E07000006', 'http://statistics.data.gov.uk/id/statistical-geography/S12000046', 'http://statistics.data.gov.uk/id/statistical-geography/E06000029', 'http://statistics.data.gov.uk/id/statistical-geography/E07000051', 'http://statistics.data.gov.uk/id/statistical-geography/E07000190', 'http://statistics.data.gov.uk/id/statistical-geography/E07000201']

if __name__ == '__main__': 
    
    ONS_json = "http://statistics.data.gov.uk/sparql.json"
    ukdigitaltwinendpoint = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin/sparql"
    ukdigitaltwin_label = "ukdigitaltwin"
    # res = queryPowerPlantForVisualisation(ukdigitaltwin_label)
    res = queryUKElectricityConsumptionAndAssociatedGEOInfo(ukdigitaltwinendpoint, ONS_json, "2017-01-31", False)   
    # res = test_queryLACode(lacode, ukdigitaltwinendpoint, "2017-01-31")
    # res = queryGridModeltForVisualisation_Bus(ukdigitaltwin_label)
    # res = queryBranchConnectedGPSLocation(ukdigitaltwin_label, 29)
    # res= queryGeneratorLocation(ukdigitaltwin_label, 29)
    # queryGridModeltForVisualisation_Branch(ukdigitaltwin_label)
    # res = queryBusLocatedRegionGeoInfo(ukdigitaltwinendpoint, ONS_json)
    # print(res) 
    # sprint(len(res))
