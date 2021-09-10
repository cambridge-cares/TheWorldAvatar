####################################################
# Author: Wanni Xie (wx243@cam.ac.uk)              #
# Extended from: Tom Savage (trs3@cam.ac.uk)       #
# Last Update Date: 18 August 2021                 #
####################################################

import matplotlib.cm 
from colourLayers import gen_fuel_col, getChoropleth, getBusColour

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

def busModelJSONCreator(ret_bus, class_label_busPara, class_label_busInputVar): 
    bus_gps_para = ret_bus[0]
    bus_input = ret_bus[1]
    
    geojson_file_bus_gps_para = """
      {
        "type": "FeatureCollection",
        "features": ["""
      # iterating over features (rows in results array)
    for i in range(len(bus_gps_para)):
        
          # creating point feature 
          feature = """{
            "type": "Feature",
            "properties": {
              "Data_type": "Bus model parameters",
              "Bus_num": "%s",
              "Bus_type": "%s",
              "para_Gs": "%s",
              "para_Bs": "%s",
              "para_area": "%s",
              "para_basekV": "%s",
              "para_zone": "%s",
              "para_Vmax": "%s",
              "para_Vmin": "%s",
              "bus-color": "%s"
            },
            "geometry": {
              "type": "Point",
              "coordinates": [
                %s,
                %s
              ]
            }          
          },"""%(bus_gps_para[i][0], bus_gps_para[i][3], bus_gps_para[i][4], bus_gps_para[i][5], bus_gps_para[i][6], bus_gps_para[i][7], bus_gps_para[i][8], bus_gps_para[i][9], \
                     bus_gps_para[i][10], getBusColour(i + 1), bus_gps_para[i][1], bus_gps_para[i][2])         
          # adding new line 
          geojson_file_bus_gps_para += '\n'+feature   
    # removing last comma as is last line
    geojson_file_bus_gps_para = geojson_file_bus_gps_para[:-1]
    # finishing file end 
    end_geojson = """
        ]
      }
      """
    geojson_file_bus_gps_para += end_geojson
    # saving as geoJSON
    geojson_written = open(class_label_busPara+'.geojson','w')
    geojson_written.write(geojson_file_bus_gps_para)
    geojson_written.close() 
    
    # creat the geojson file for 
    geojson_file_bus_input = """
      {
        "type": "FeatureCollection",
        "features": ["""
      # iterating over features (rows in results array)
    for i in range(len(bus_input)):
          # creating point feature 
          feature = """{
            "type": "Feature",
            "properties": {
              "Data_type": "Bus model input variables",
              "Bus_num": "%s",
              "input_Pd": "%s",
              "input_Gd": "%s",
              "input_Vm": "%s",
              "input_Va": "%s"
            },
            "geometry": {
              "type": "Point",
              "coordinates": [
                %s,
                %s
              ]
            }     
          },"""%(bus_input[i][0], bus_input[i][1], bus_gps_para[i][2], bus_gps_para[i][3], bus_gps_para[i][4], bus_gps_para[i][1], bus_gps_para[i][2])         
          geojson_file_bus_input += '\n'+feature   
    # removing last comma as is last line
    geojson_file_bus_input = geojson_file_bus_input[:-1]
    # finishing file end 
    end_geojson = """
        ]
      }
      """
    geojson_file_bus_input += end_geojson
    # saving as geoJSON
    geojson_written = open(class_label_busInputVar + '.geojson','w')
    geojson_written.write(geojson_file_bus_input)
    geojson_written.close() 
    return 

def BranchAndBusConnectionGPSLocationJSONCreator(ret_branchGPS, ret_branchPara, class_label_lineGPS, class_label_FromBus, class_label_linePara): 
    geojson_file = """
      {
        "type": "FeatureCollection",
        "features": ["""
      # iterating over features (rows in results array)
    for i in range(len(ret_branchGPS)):
          # creating point feature 
          feature = """{
            "type": "Feature",
            "properties": {
              "Name": "%s",
              "From_Bus": %s,
              "To_Bus": %s,
              "para_R":  %s,
              "para_X":  %s,
              "para_B":  %s,
              "para_RateA":  %s,
              "para_RateB":  %s,
              "para_RateC":  %s, 
              "para_RatioCoefficient":  %s, 
              "para_Angle":  %s, 
              "para_Status":  %s,
              "para_AngleMax":  %s,
              "para_AngleMin":  %s       
            },
            "geometry": {
                "type": "LineString",
                "coordinates": [[%s, %s], 
                                [%s, %s]]
                }            
          },""" %(ret_branchPara[i][0], ret_branchPara[i][1], ret_branchPara[i][2], ret_branchPara[i][3], ret_branchPara[i][4], ret_branchPara[i][5], \
                  ret_branchPara[i][6], ret_branchPara[i][7], ret_branchPara[i][8], ret_branchPara[i][9], ret_branchPara[i][10], ret_branchPara[i][11], \
                  ret_branchPara[i][12], ret_branchPara[i][13], ret_branchGPS[i][1], ret_branchGPS[i][2], ret_branchGPS[i][3], ret_branchGPS[i][4])         
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
    geojson_written = open(class_label_lineGPS + '.geojson','w')
    geojson_written.write(geojson_file)
    geojson_written.close()
    
    geojson_file = """
      {
        "type": "FeatureCollection",
        "features": ["""
      # iterating over features (rows in results array)
    for i in range(len(ret_branchGPS)):
          # creating point feature 
          feature = """{
            "type": "Feature",
            "properties": {
              "Name": "%s"
            },
            "geometry": {
                "type": "Point",
                "coordinates": [%s, %s]
                }            
          },""" %(ret_branchGPS[i][0], ret_branchGPS[i][1], ret_branchGPS[i][2])         
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
    geojson_written = open(class_label_FromBus + '.geojson','w')
    geojson_written.write(geojson_file)
    geojson_written.close() 
    
    geojson_file = """
      {
        "type": "FeatureCollection",
        "features": ["""
      # iterating over features (rows in results array)
    for i in range(len(ret_branchPara)):
          # creating point feature 
          feature = """{
            "type": "Feature",
            "properties": {
              "Name": "%s",
              "From_Bus": %s,
              "To_Bus": %s,
              "para_R":  %s,
              "para_X":  %s,
              "para_B":  %s,
              "para_RateA":  %s,
              "para_RateB":  %s,
              "para_RateC":  %s, 
              "para_RatioCoefficient":  %s, 
              "para_Angle":  %s, 
              "para_Status":  %s,
              "para_AngleMax":  %s,
              "para_AngleMin":  %s             
            }
          },""" %(ret_branchPara[i][0], ret_branchPara[i][1], ret_branchPara[i][2], ret_branchPara[i][3], ret_branchPara[i][4], ret_branchPara[i][5], \
                  ret_branchPara[i][6], ret_branchPara[i][7], ret_branchPara[i][8], ret_branchPara[i][9], ret_branchPara[i][10], ret_branchPara[i][11], \
                  ret_branchPara[i][12], ret_branchPara[i][13])         
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
    geojson_written = open(class_label_linePara + '.geojson','w')
    geojson_written.write(geojson_file)
    geojson_written.close() 
    return 
