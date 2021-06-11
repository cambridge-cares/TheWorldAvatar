from SPARQLWrapper import SPARQLWrapper, CSV, JSON
import json 
from tqdm import tqdm
import time
import numpy as np 
import pandas as pd

def gen_fuel_col(gen_fuel):
  #returns a colour code for each generator fuel type.
  #https://htmlcolorcodes.com/
  map_fuel_dict = {
        "BiomassGeneration": "#1E8449",
        "CoalBiomass": "#1E8449",
        "WindGeneration": "#13bef2",
        "Wind": "#13bef2",
        "HydroGeneration": "#1F618D",
        "Hydro": "#1F618D",
        "SolarGeneration": "#ffd21f",
        "Solar": "#ffd21f",
        "CoalGeneration": "#99A3A4",
        "Coal": "#99A3A4",
        "OilGeneration": "#1B2631",
        "Oil": "#1B2631",
        "NaturalGasGeneration": "#eb8500",
        "NaturalGas": "#eb8500",
        "NuclearGeneration": "#ed2400",
        "Nuclear": "#ed2400",
        "PumpHydroStorage": "#1F618D",
        "PumpHydro": "#1F618D",
        "SourGasGeneration": "#eb8500",
        "SourGas": "#eb8500",
        "Waste_anaerobicdigestion": "#1E8449",
        "Waste_municipalsolidwaste": "#bc1fff",
        "Waste": "#873600"
    }
  return map_fuel_dict[gen_fuel]

def check_GPS_char(c):
  #Checks to provided character is acceptable in GPS format
  acceptable = "-.0123456789"
  if c in acceptable:
    return True
  return False

def GPS_special_chars(coordinate):
  #Checks that if there's a '-' it's only in the first position.
  #Checks that there is only one '.' decimal point.
  acceptable = ".0123456789"
  tick = 0
  point = 0
  for i in coordinate[1]:
    if (tick > 0) and not (i in acceptable):
      coordinate[1] = coordinate[1][:tick] + coordinate[1][tick+1:]
      tick -= 1
      print("Warning: " + coordinate[0] + " contains (automatically removed) misplaced '-' in longitude.")
    if i == '.':
      if point > 0:
        coordinate[1] = coordinate[1][:tick] + coordinate[1][tick+1:]
        tick -= 1
        print("Warning: " + coordinate[0] + " contains (automatically removed) additional '.' in longitude.")
      point += 1
    tick += 1
  tick = 0
  point = 0
  for i in coordinate[2]:
    if (tick > 0) and not (i in acceptable):
      coordinate[2] = coordinate[2][:tick] + coordinate[2][tick+1:]
      tick -= 1
      print("Warning: " + coordinate[0] + " contains (automatically removed) misplaced '-' in latitude.")
    if i == '.':
      if point > 0:
        coordinate[2] = coordinate[2][:tick] + coordinate[2][tick+1:]
        tick -= 1
        print("Warning: " + coordinate[0] + " contains (automatically removed) additional '.' in latitude.")
      point += 1
    tick += 1
  return coordinate

def check_GPS(ret):
  #Checks the GPS coordinates are of the correct format.
  #the 'ret' input array should have the first three columns as:
  #0: The name.
  #1: The longitude.
  #2: The latitude.
  #3+: (Optional) Doesn't matter, it just uses the first three columns, if you have more that's fine. 
  #Note that the 'automatic replacement' is not in the knowledge graph, just on the output side.
  for i in ret:
    #Check the longitude and latitude don't have unexpected characters and have at least some permitted characters. 
    for j in i[1]:
      if not check_GPS_char(j):
        print("Warning: " + i[0] + " contains (automatically removed) longitude (GPS) incompatible character: " + j)
        i[1] = i[1].replace(j, '')
    if len(i[1]) == 0:
      print("Warning: " + i[0] + " contains no valid longitude, 0.0000000 substituted")
      i[1] = "0.0000000"
    for j in i[2]:
      if not check_GPS_char(j):
        print("Warning: " + i[0] + " contains (automatically removed) latitude (GPS) incompatible character: " + j)
        i[2] = i[2].replace(j, '')
    if len(i[2]) == 0:
      print("Warning: " + i[0] + " contains no valid latitude, 0.0000000 substituted")
      i[2] = "0.0000000"
    #Check the longitude and latitude have special characters ('-' and '.') used correctly.
    i = GPS_special_chars(i)
    #Check the longitude and latitude are now valid numbers.
    try:
        float(i[1])
    except ValueError:
        print("Warning: " + i[0] + " contains longitude that is not a number, 0.0000000 substituted")
        i[1] = "0.0000000"
    try:
        float(i[2])
    except ValueError:
        print("Warning: " + i[0] + " contains latitude that is not a number, 0.0000000 substituted")
        i[2] = "0.0000000"
    #Check the longitude and latitude are in the valid range (-180 to 180 for longitude, -90 to 90 for latitude).
    if (float(i[1]) < -180.0) or (float(i[1]) > 180.0):
      print("Warning: " + i[0] + " contains longitude that is out of the valid -180 to 180 range, 0.0000000 substituted")
      i[1] = "0.0000000"
    if (float(i[2]) < -90.0) or (float(i[2]) > 90.0):
      print("Warning: " + i[0] + " contains latitude that is out of the valid -90 to 90 range, 0.0000000 substituted")
      i[2] = "0.0000000"
  print("GPS check completed. If there was a 'Warning' in the output above then the 'automatic replacement / substitution' mentioned does not effect the knowledge graph, just the created JSON file. ")
  return ret

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
  query = """
        PREFIX powerplant: <http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX coordinate: <http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#>
        PREFIX system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>
        PREFIX space_and_time_extended:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#>
        PREFIX technical_system: <http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#>
        PREFIX system_realization: <http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#>
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
 # performing SPARQL query  
  sparql = SPARQLWrapper(endpoint)
  sparql.setReturnFormat(JSON) 
  sparql.setQuery(query) 
  start = time.time()
  print('Querying...')
  ret = sparql.queryAndConvert()
  end = time.time()
  # parsing JSON SPARQL results into an array
  print('Finished in ',np.round(end-start,2),' seconds')
  ret = ret['results']['bindings']
  #print(ret) #Unformatted
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
  ret = ret_array
  #print(ret) #Formatted
  ret = check_GPS(ret)
  print(ret) #GPS compatibility fixed (if required)
  # allocating start of .geoJSON file 
  geojson_file = """
  {
    "type": "FeatureCollection",
    "features": ["""
  # iterating over features (rows in results array)
  for i in range(num_ret):
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
endpoint = "https://como.ceb.cam.ac.uk/rdf4j-server/repositories/UKPowerPlant"
class_label = 'UK_PowerPlants'
query_to_geoJSON(endpoint, class_label)
