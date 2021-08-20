####################################################
# Author: Wanni Xie (wx243@cam.ac.uk)              #
# Extended from: Tom Savage (trs3@cam.ac.uk)       #
# Last Update Date: 18 August 2021                 #
####################################################

import matplotlib.cm 
from colourLayers import gen_fuel_col, getChoropleth

# def SDGgeoJSONCreator():
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
  
  
def powerPlantgeoJSONCreator(ret_pow, class_label): 
    geojson_file = """
      {
        "type": "FeatureCollection",
        "features": ["""
      # iterating over features (rows in results array)
    for i in range(len(ret_pow)):
          # creating point feature 
          feature = """{
            "type": "Feature",
            "properties": {
              "marker-color": "%s",
              "marker-size": "medium",
              "marker-symbol": "",
              "name": "%s",
              "fuel": "%s",
              "gen_tech": "%s",
              "capacity": "%s",
              "owner": "%s",
              "built_year": "%s"
            },
            "geometry": {
              "type": "Point",
              "coordinates": [
                %s,
                %s
              ]
            }
          },"""%(gen_fuel_col(ret_pow[i, 3]), ret_pow[i,0], ret_pow[i,3], ret_pow[i,4], ret_pow[i,5], ret_pow[i,6], ret_pow[i,7], ret_pow[i,1], ret_pow[i,2])
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
 
def elecConsAndGEOInfogeoJSONCreator(ret_elec, class_label): 
    geojson_file = """
      {
        "type": "FeatureCollection",
        "features": ["""
      # iterating over features (rows in results array)
    for i in range(len(ret_elec)):
          # creating point feature 
          feature = """{
            "type": "Feature",
            "properties": {
              "Location": "%s",
              "Area_LACode": "%s",
              "TotalELecConsumption": "%s",
              "DomesticConsumption": "%s",
              "Industrial_and_Commercial": "%s",
              "area-color": "%s"
            },
            "geometry":  %s             
          },"""%(ret_elec[i,0], ret_elec[i,1], ret_elec[i,2], ret_elec[i,3], ret_elec[i,4], getChoropleth(float(ret_elec[i,2])), str(ret_elec[i,5]).replace("\'", "\""))         
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
