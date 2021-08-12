from SPARQLWrapper import SPARQLWrapper, CSV, JSON
import json
from matplotlib.pyplot import colormaps 
from tqdm import tqdm
import time
import numpy as np 
import pandas as pd
import matplotlib.cm 
import matplotlib.colors

from GPSLocationChecker import check_GPS_char, GPS_special_chars, check_GPS
from colourLayers import gen_fuel_col

def query_to_geoJSON(endpoint, class_label):
  '''
  DESCRIPTION: 
  Produces a .geoJSON file by querying a triple-store for a specific class
  with the attribute lat-lon from the namespace http://www.bigdata.com/rdf/geospatial/literals/v1#
  If you encode your locations using alternative methods you will have to change this.
  The .geoJSON file produced encodes single point locations and can include their attributes.

  INPUTS:
  class_URI:      The URI of the class you want to produce a geoJSON file of (with namespace)
  class_label:    The name of the class you are querying, used to name the final geoJSON file

  OUTPUTS:
  A file of the form class_label.geoJSON in the working directory. 
  '''
  # defining the query string given the class URI
  query_pow = """
        PREFIX powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX coordinate: <http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#>
        PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
        PREFIX space_and_time_extended:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
        PREFIX technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
        PREFIX system_realization: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#>
        PREFIX ontosdg: <http://www.theworldavatar.com/ontology/ontosdg/OntoSDG.owl#>
        PREFIX ontospecies:<http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> 

        SELECT DISTINCT ?numericalValue_y ?numericalValue_x  ?powerPlantIRI ?Primary_Fuel_type ?value_of_Designed_Capacity
        WHERE
        {
        ?powerPlantIRI rdf:type powerplant:PowerPlant .
        ?powerPlantIRI space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem .
        ?CoordinateSystem  space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate .
        ?CoordinateSystem  space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate .
        ?y_coordinate  system:hasValue ?value_y_coordinate .
        ?x_coordinate  system:hasValue ?value_x_coordinate .
        ?value_y_coordinate  system:numericalValue ?numericalValue_y .
        ?value_x_coordinate  system:numericalValue ?numericalValue_x .
        ?powerPlantIRI rdf:type powerplant:PowerPlant .
        ?powerPlantIRI technical_system:realizes ?Generation_Type .
        ?Generation_Type powerplant:consumesPrimaryFuel ?Primary_Fuel_type .
        ?powerPlantIRI system_realization:designCapacity ?Capacity .
        ?Capacity system:hasValue ?Capacity_value .
        ?Capacity_value system:numericalValue ?value_of_Designed_Capacity .
        ?Capacity_value system:hasUnitOfMeasure ?unit_of_Designed_Capacity .

        


        } LIMIT 1200
        """
  query_pow_new = """
          PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          PREFIX ontopowsys_PowSysRealization: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#>
          PREFIX ontopowsys_PowSysPerformance: <http://www.theworldavatar.com/ontology/ontopowsys/PowSysPerformance.owl#>
          PREFIX ontocape_upper_level_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
          PREFIX ontoeip_powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
          PREFIX ontoecape_technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
          PREFIX meta_model_topology: <http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#>
          PREFIX space_and_time_extended: <http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
          PREFIX power_plant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
          
          SELECT DISTINCT ?numericalValue_y ?numericalValue_x ?powerPlantIRI ?Primary_Fuel_type ?value_of_Designed_Capacity
          WHERE
          {       
            ?powerPlantIRI space_and_time_extended:hasGISCoordinateSystem ?CoordinateSystem .
            ?CoordinateSystem space_and_time_extended:hasProjectedCoordinate_x ?x_coordinate .
            ?CoordinateSystem space_and_time_extended:hasProjectedCoordinate_y ?y_coordinate .
            ?x_coordinate ontocape_upper_level_system:hasValue ?GPS_x_coordinate .
            ?y_coordinate ontocape_upper_level_system:hasValue ?GPS_y_coordinate . 
            ?GPS_x_coordinate ontocape_upper_level_system:numericalValue ?numericalValue_x. # longitude is east/west
            ?GPS_y_coordinate ontocape_upper_level_system:numericalValue ?numericalValue_y . # latitude is north/south
        	
            ?powerPlantIRI ontoecape_technical_system:hasRealizationAspect ?PowerGenerator .
            ?PowerGenerator a ontoeip_powerplant:PowerGenerator . 
            ?powerPlantIRI ontoecape_technical_system:hasRequirementsAspect/ontocape_upper_level_system:hasValue ?v_capa .
            ?v_capa ontocape_upper_level_system:numericalValue ?value_of_Designed_Capacity .
        	?v_capa ontocape_upper_level_system:hasUnitOfMeasure ?UnitOfCapacity .
            ?PowerGenerator ontoecape_technical_system:realizes/ontoeip_powerplant:consumesPrimaryFuel ?Primary_Fuel_type .
            ?PowerGenerator ontoecape_technical_system:realizes/ontoeip_powerplant:usesGenerationTechnology ?GenerationTechnology .
  
        } LIMIT 2
 """

  query_sdg = """
        PREFIX ontosdg: <http://www.theworldavatar.com/ontology/ontosdg/OntoSDG.owl#>
        PREFIX ontospecies:<http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#> 

        SELECT *
        WHERE
        {
        ?powerPlantIRI ontosdg:hasIndicator ?941 .
        ?941 ontospecies:value ?941Value .
        } LIMIT 1200
        """
        
 # performing SPARQL query  
  # sparql_sdg= SPARQLWrapper('http://www.theworldavatar.com/blazegraph/namespace/ontosdg/sparql')
  # TODO: Modify the endpoint 
  sparql_pow= SPARQLWrapper('https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerPlantKG_demo')
  # sparql_pow= SPARQLWrapper('https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerPlantKG')
  sparql_pow.setReturnFormat(JSON) 
  
  # TODO: query string passing  
  sparql_pow.setQuery(query_pow_new)
  
  start = time.time()
  print('Querying...')
  ret = sparql_pow.queryAndConvert()
  end = time.time()
  # parsing JSON SPARQL results into an array
  print('Finished in ',np.round(end-start,2),' seconds')
  # print("The query return type is: ", type(ret))
  # print(list(ret.keys())) #Unformatted
  # print(ret["head"])  
  
  ret = ret['results']['bindings'] # extract the elements of the original dict
  print("The query return is ", ret) #Unformatted
  num_ret = len(ret)
  # assigning memory to results array 
  ret_array = np.zeros((num_ret,5),dtype='object')
  header = ['name', 'lon','lat', 'fuel', 'capacity']
  # iterating over results and allocating properties from query
  for i in tqdm(range(num_ret)):
      name = ret[i]['powerPlantIRI']['value'].split('#')
      lon = ret[i]['numericalValue_x']['value']
      lat = ret[i]['numericalValue_y']['value']
      fuel = ret[i]['Primary_Fuel_type']['value'].split('#')
      capacity = ret[i]['value_of_Designed_Capacity']['value']
      ret_array[i,:] = [name[1], lon, lat, fuel[1], capacity]
  ret_pow = ret_array

  # sparql_sdg.setReturnFormat(JSON) 
  # sparql_sdg.setQuery(query_sdg) 
  # start = time.time()
  # print('Querying...')
  # ret = sparql_sdg.queryAndConvert()
  # end = time.time()
  # # parsing JSON SPARQL results into an array
  # print('Finished in ',np.round(end-start,2),' seconds')
  # ret = ret['results']['bindings']
  # #print(ret) #Unformatted
  # num_ret = len(ret)
  # # assigning memory to results array 
  # ret_array = np.zeros((num_ret,2),dtype='object')
  # header = ['name', '941']
  # # iterating over results and allocating properties from query
  # for i in tqdm(range(num_ret)):
  #     name = ret[i]['powerPlantIRI']['value'].split('/')
  #     ind_val = ret[i]['941Value']['value']
  #     ret_array[i,:] = [name[-1], float(ind_val)]
  # ret_sdg = ret_array

  #print(ret) #Formatted
  ret_pow = check_GPS(ret_pow)
  print("power plant", ret_pow)
  # print("power plant", ret_pow, "SDG", ret_sdg) #GPS compatibility fixed (if required)

  # power_plant_name = []
  # SDG_powplant = []
  # sdg_addition = [] 
  # for i in range(len(ret_pow)):
  #     power_plant_name.append(str(ret_pow[i,0]))
  # print("power_plant_name length is", len(power_plant_name))
  # print("power_plant_name is", power_plant_name)
  
  # for j in range(len(ret_sdg)):
  #     SDG_powplant.append(str(ret_sdg[j,0]))
  # print("sdg_powplant is", len(SDG_powplant))
  # print(SDG_powplant)
  
  # for i in range(len(ret_sdg)):
  #     power_plant_name.append(str(ret_pow[i,0]))
  # print("power_plant_name length is", len(power_plant_name))
  # print("power_plant_name is", power_plant_name)
  
  # for j in range(len(ret_sdg)):
  #     SDG_powplant.append(str(ret_sdg[j,1]))
  # print("sdg_powplant is", len(SDG_powplant), print(SDG_powplant))
  
  # for j in range(len(ret_sdg)):
  #   if ret_pow[i,0] in power_plant_name:    
  #     power_plant_name.remove(str(ret_sdg[j,1]))
  #   else: 
  #     print(ret_pow[i,0])
    
  # sdg_addition_col = np.array([sdg_addition]).T
  # colors_ar = []
  # cmap = matplotlib.cm.get_cmap('RdYlGn')
  # scaled_sdg = sdg_addition / np.max(sdg_addition)
  # for i in range(len(sdg_addition)):
  #   rgba = cmap(scaled_sdg[i])
  #   colors_ar.append(matplotlib.colors.rgb2hex(rgba))

  # colors_ar = np.array([colors_ar]).T

  # ret = np.concatenate((ret_pow,sdg_addition_col,colors_ar),axis=1)
  ret = ret_pow

  # allocating start of .geoJSON file 
  
  # geojson_file = """
  # {
  #   "type": "FeatureCollection",
  #   "features": ["""
  # # iterating over features (rows in results array)
  # for i in range(len(ret)):
  #     # creating point feature 
  #     feature = """{
  #       "type": "Feature",
  #       "properties": {
  #         "marker-color": "%s",
  #         "marker-size": "medium",
  #         "marker-symbol": "",
  #         "name": "%s",
  #         "fuel": "%s",
  #         "capacity": "%s",
  #         "sdg941": "%s",
  #         "sdgcolor": "%s"
  #       },
  #       "geometry": {
  #         "type": "Point",
  #         "coordinates": [
  #           %s,
  #           %s
  #         ]
  #       }
  #     },"""%(gen_fuel_col(ret[i, 3]), ret[i,0], ret[i,3], ret[i,4],str(ret[i,5]),ret[i,6], ret[i,1], ret[i,2])
  
  geojson_file = """
  {
    "type": "FeatureCollection",
    "features": ["""
  # iterating over features (rows in results array)
  for i in range(len(ret)):
      # creating point feature 
      feature = """{
        "type": "Feature",
        "properties": {
          "marker-color": "%s",
          "marker-size": "medium",
          "marker-symbol": "",
          "name": "%s",
          "fuel": "%s",
          "capacity": "%s"
        },
        "geometry": {
          "type": "Point",
          "coordinates": [
            %s,
            %s
          ]
        }
      },"""%(gen_fuel_col(ret[i, 3]), ret[i,0], ret[i,3], ret[i,4], ret[i,1], ret[i,2])
      # adding new line 
      geojson_file += '\n'+feature

  # removing last comma as is last line
  geojson_file = geojson_file[:-1]
  # finishing file end 
  end_geojson = """
    ]
  }
  """
  geojson_file += end_geojson
  # saving as geoJSON
  geojson_written = open(class_label+'.geojson','w')
  geojson_written.write(geojson_file)
  geojson_written.close() 
  return 

# querying terminals from ontogasgrid
endpoint = ""
class_label = 'UK_PowerPlants'
query_to_geoJSON(endpoint, class_label)
