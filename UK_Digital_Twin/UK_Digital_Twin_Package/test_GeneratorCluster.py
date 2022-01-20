###################################################
# Author: Wanni Xie (wx243@cam.ac.uk)             #
# Last Update Date: 18 Jan 2022                   #
###################################################
import sys, os
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.DistanceCalculator import DistanceBasedOnGPSLocation as GPS_distance
import UK_Power_Grid_Topology_Generator.SPARQLQueriesUsedInTopologyABox as query_topo
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel as endpointList
import shapely.geometry
import time

def closestBus_withinMethod(res_queryBusTopologicalInformation, res_queryPowerPlantAttributes, aggregatedBusList):  
      print('****The cluster principle is closestBus_withinMethod****')
      time.gmtime(0)
      # res_queryBusTopologicalInformation = [Bus_node, EBus, Bus_lat_lon (LA code region) ]
      # res_queryPowerPlantAttributes = [PowerGenerator, LACode_PP, PP_lat_lon, PrimaryFuel, GenerationTechnology]
      ons_label = endpointList.ONS['lable']
      # detect the location of the bus, in GB or in NI
      busInGB, busInNorthernIreland, countryBoundaryDict = busLocationFinderForGBOrNI(res_queryBusTopologicalInformation, ons_label)
      
      powerPlantAndBusPairList = [] 
      busNumberArray = list(range(1, len(res_queryBusTopologicalInformation) + 1))     
      for pp in res_queryPowerPlantAttributes: 
           powerPlantAndBusPair = {}
           if len(busInGB) > 0:
               pp_within_flag = query_topo.queryifWithin(pp['LACode_PP'], 'K03000001', ons_label)
               j = 0
               distances = [65534]*len(busInGB) # the large number is the earth's circumference              
               if pp_within_flag == True or pp['LACode_PP'] == 'K03000001': # power plant located in GB
                  for bus in busInGB:
                      GPSLocationPair = [pp['PP_lat_lon'][0], pp['PP_lat_lon'][1], bus['Bus_lat_lon'][0], bus['Bus_lat_lon'][1]]    
                      distances[j] = GPS_distance(GPSLocationPair)
                      j += 1
                  
               if min(distances) == 65534: # The power plant is not located in the area which the buses located in, like the pp in NI
                   print('######', pp['PowerGenerator'], pp['LACode_PP'])
                   print('######The power plant is not located in GB.')
               else:
                   bus_index = distances.index(min(distances))    
                   powerPlantAndBusPair = {**busInGB[bus_index], **pp} 
                   powerPlantAndBusPairList.append(powerPlantAndBusPair) 
                   # check if all buses are connected with generators 
                   busNo = int(powerPlantAndBusPair['Bus_node'].split("EBus-")[1])
                   if busNo in busNumberArray:
                      busNumberArray.remove(busNo)
                   continue # if the power plant is within GB, then it is not necessary to check its distance from the buses located in NI, jump from the current loop
           if len(busInNorthernIreland) > 0:
               j = 0
               distances = [65534]*len(busInNorthernIreland) # the large number is the earth's circumference
               pp_within_flag = query_topo.queryifWithin(pp['LACode_PP'], 'N07000001', ons_label)
               if pp_within_flag == True or pp['LACode_PP'] == 'N92000002' or pp['LACode_PP'] == 'N07000001': # power plant located in NI
                  for bus in busInNorthernIreland:
                      GPSLocationPair = [pp['PP_lat_lon'][0], pp['PP_lat_lon'][1], bus['Bus_lat_lon'][0], bus['Bus_lat_lon'][1]]    
                      distances[j] = GPS_distance(GPSLocationPair)
                      j += 1
               if min(distances) == 65534: # The power plant is not located in the area which the buses located in, like the pp in NI
                   print('######', pp['PowerGenerator'], pp['LACode_PP'])
                   print('######The power plant is not located in Northern Ireland, or the power plant is the type of offshore wind farm.')
               else:
                   bus_index = distances.index(min(distances))    
                   powerPlantAndBusPair = {**busInNorthernIreland[bus_index], **pp}                    
                   powerPlantAndBusPairList.append(powerPlantAndBusPair) 
                   #check if all buses are connected with generators busLocatedCountryFinder
                   busNo = int(powerPlantAndBusPair['Bus_node'].split("EBus-")[1])
                   if busNo in busNumberArray:
                      busNumberArray.remove(busNo)
      if len(busNumberArray) != 0:
          print("WARNING: There are buses not being connected by the generators, which are number:", busNumberArray)
      else:
          print("************All buses are connected with generators************") 
      final_time = time.time_ns()
      print('####################The total running time is', final_time, 'ns.')
      return powerPlantAndBusPairList 
  
def closestBus_shapelyMethod(res_queryBusTopologicalInformation, res_queryPowerPlantAttributes, aggregatedBusList):  
      print('****The cluster principle is closestBus_shapelyMethod****')
      time.gmtime(0) 
      # res_queryBusTopologicalInformation = [Bus_node, EBus, Bus_lat_lon (LA code region) ]
      # res_queryPowerPlantAttributes = [PowerGenerator, LACode_PP, PP_lat_lon, PrimaryFuel, GenerationTechnology]
      ons_label = endpointList.ONS['lable']
      # detect the location of the bus, in GB or in NI
      busInGB, busInNorthernIreland, countryBoundaryDict = busLocationFinderForGBOrNI(res_queryBusTopologicalInformation, ons_label)
      
      powerPlantAndBusPairList = [] 
      busNumberArray = list(range(1, len(res_queryBusTopologicalInformation) + 1))     
      for pp in res_queryPowerPlantAttributes: 
           powerPlantAndBusPair = {}
           if len(busInGB) > 0:
                j = 0
                distances = [65534]*len(busInGB) # the large number is the earth's circumference
                pp_lonlat_point = shapely.geometry.Point(pp['PP_lat_lon'][1], pp['PP_lat_lon'][0])
                interior_pp = countryBoundaryDict['K03000001'].intersects(pp_lonlat_point) # check if the power plant locates in GB
                if interior_pp == True:
                    for bus in busInGB:
                        GPSLocationPair = [pp['PP_lat_lon'][0], pp['PP_lat_lon'][1], bus['Bus_lat_lon'][0], bus['Bus_lat_lon'][1]]    
                        distances[j] = GPS_distance(GPSLocationPair)
                        j += 1
                  
                if min(distances) == 65534: # The power plant is not located in the area which the buses located in
                    print('######', pp['PowerGenerator'], pp['LACode_PP'])
                    print('######The power plant is not located in GB, or the power plant is the type of offshore wind farm.')
                else:
                   bus_index = distances.index(min(distances))    
                   powerPlantAndBusPair = {**busInGB[bus_index], **pp} 
                   powerPlantAndBusPairList.append(powerPlantAndBusPair) 
                   # check if all buses are connected with generators 
                   busNo = int(powerPlantAndBusPair['Bus_node'].split("EBus-")[1])
                   if busNo in busNumberArray:
                      busNumberArray.remove(busNo)
                   continue # if the power plant is within GB, then it is not necessary to check its distance from the buses located in NI, jump from the current loop
           if len(busInNorthernIreland) > 0:
                j = 0
                distances = [65534]*len(busInNorthernIreland) # the large number is the earth's circumference
                pp_lonlat_point = shapely.geometry.Point(pp['PP_lat_lon'][1], pp['PP_lat_lon'][0])
                interior_pp = countryBoundaryDict['N92000002'].intersects(pp_lonlat_point)
                if interior_pp == True:
                    for bus in busInNorthernIreland:
                        GPSLocationPair = [pp['PP_lat_lon'][0], pp['PP_lat_lon'][1], bus['Bus_lat_lon'][0], bus['Bus_lat_lon'][1]]    
                        distances[j] = GPS_distance(GPSLocationPair)
                        j += 1
                  
                if min(distances) == 65534: # The power plant is not located in the area which the buses located in, like the pp in NI
                    print('######', pp['PowerGenerator'], pp['LACode_PP'])
                    print('######The power plant is not located in Northern Ireland, or the power plant is the type of offshore wind farm.')
                else:
                   bus_index = distances.index(min(distances))    
                   powerPlantAndBusPair = {**busInNorthernIreland[bus_index], **pp}                    
                   powerPlantAndBusPairList.append(powerPlantAndBusPair) 
                   #check if all buses are connected with generators busLocatedCountryFinder
                   busNo = int(powerPlantAndBusPair['Bus_node'].split("EBus-")[1])
                   if busNo in busNumberArray:
                      busNumberArray.remove(busNo)
      if len(busNumberArray) != 0:
          print("WARNING: There are buses not being connected by the generators, which are number:", busNumberArray)
      else:
          print("************All buses are connected with generators************") 
      final_time = time.time_ns()
      print('####################The total running time is', final_time, 'ns.')
      return powerPlantAndBusPairList 
  
def busLocationFinderForGBOrNI(res_queryBusTopologicalInformation, ons_label):
    # declare the lists
    busInGB = []
    busInNorthernIreland = []
    countryBoundaryDict = {}
    # query the boundary of GB and NI
    res_GBOrNIBoundary = list(query_topo.queryGBOrNIBoundary(ons_label)) # [LACode_area, Geo_InfoList]       
    for bus in res_queryBusTopologicalInformation:
      # bus['Bus_lat_lon'] = [float(bus['Bus_lat_lon'].split('#')[0]), float(bus['Bus_lat_lon'].split('#')[1])]
      bus_lonlat_point = shapely.geometry.Point(bus['Bus_lat_lon'][1], bus['Bus_lat_lon'][0])
      for country in res_GBOrNIBoundary:
          countryBoundaryDict.update({country['LACode_area']: country['Geo_InfoList']})
          countryBoundary = country['Geo_InfoList']
          interior = countryBoundary.intersects(bus_lonlat_point)
          if interior == True:
              # bus.update({'Bus_LocatedCountry':str(country['LACode_area'])})
              if str(country['LACode_area']) == 'K03000001':
                  busInGB.append(bus)
              elif str(country['LACode_area']) == 'N92000002' or str(country['LACode_area']) == 'N07000001' :
                  busInNorthernIreland.append(bus) 
    return busInGB, busInNorthernIreland, countryBoundaryDict


if __name__ == '__main__':
    # 29-bus
    res_queryBusTopologicalInformation = [{'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-001', 'Bus_lat_lon': [57.46987, -4.49067], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-001_Scotland.owl#EBus-001_Scotland'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-002', 'Bus_lat_lon': [57.47452, -1.79982], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-002_Scotland.owl#EBus-002_Scotland'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-003', 'Bus_lat_lon': [56.707, -4.01079], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-003_Scotland.owl#EBus-003_Scotland'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-004', 'Bus_lat_lon': [56.03863, -3.88907], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-004_Scotland.owl#EBus-004_Scotland'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-005', 'Bus_lat_lon': [55.80952, -4.47682], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-005_Scotland.owl#EBus-005_Scotland'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-006', 'Bus_lat_lon': [55.75094, -4.08051], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-006_Scotland.owl#EBus-006_Scotland'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-007', 'Bus_lat_lon': [55.96636, -2.40824], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-007_Scotland.owl#EBus-007_Scotland'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-008', 'Bus_lat_lon': [55.66849, -2.32998], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-008_Scotland.owl#EBus-008_Scotland'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-009', 'Bus_lat_lon': [54.94193, -2.9618], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-009_North_West_England.owl#EBus-009_North_West_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-010', 'Bus_lat_lon': [54.97442, -1.73299], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-010_North_East_England.owl#EBus-010_North_East_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-011', 'Bus_lat_lon': [53.74435, -2.75499], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-011_North_West_England.owl#EBus-011_North_West_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-012', 'Bus_lat_lon': [53.22924, -3.03174], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-012_Wales.owl#EBus-012_Wales'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-013', 'Bus_lat_lon': [53.42696, -2.37878], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-013_North_West_England.owl#EBus-013_North_West_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-014', 'Bus_lat_lon': [53.48778, -1.60162], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-014_Yorkshire_and_the_Humber.owl#EBus-014_Yorkshire_and_the_Humber'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-015', 'Bus_lat_lon': [53.90023, -0.82358], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-015_Yorkshire_and_the_Humber.owl#EBus-015_Yorkshire_and_the_Humber'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-016', 'Bus_lat_lon': [53.5973, -0.7558], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-016_East_Midlands.owl#EBus-016_East_Midlands'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-017', 'Bus_lat_lon': [52.86291, -1.25763], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-017_East_Midlands.owl#EBus-017_East_Midlands'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-018', 'Bus_lat_lon': [52.25124, -1.97351], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-018_West_Midlands_(county).owl#EBus-018_West_Midlands_(county)'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-019', 'Bus_lat_lon': [52.72692, 0.19812], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-019_East_of_England.owl#EBus-019_East_of_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-020', 'Bus_lat_lon': [52.07165, 1.06316], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-020_East_of_England.owl#EBus-020_East_of_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-021', 'Bus_lat_lon': [51.93513, 0.11679], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-021_East_of_England.owl#EBus-021_East_of_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-022', 'Bus_lat_lon': [51.92706, -0.90993], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-022_South_East_England.owl#EBus-022_South_East_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-023', 'Bus_lat_lon': [51.37497, -2.14415], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-023_South_West_England.owl#EBus-023_South_West_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-024', 'Bus_lat_lon': [51.33589, -1.07755], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-024_South_East_England.owl#EBus-024_South_East_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-025', 'Bus_lat_lon': [51.50774, -0.12715], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-025_London.owl#EBus-025_London'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-026', 'Bus_lat_lon': [51.36846, 0.74141], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-026_South_East_England.owl#EBus-026_South_East_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-027', 'Bus_lat_lon': [51.10502, 0.97611], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-027_South_East_England.owl#EBus-027_South_East_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-028', 'Bus_lat_lon': [50.91637, -1.03831], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-028_South_East_England.owl#EBus-028_South_East_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-029', 'Bus_lat_lon': [50.76746, -3.40616], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-029_South_West_England.owl#EBus-029_South_West_England'}]    
    res_queryPowerPlantAttributes = query_topo.queryPowerPlantAttributes(None, False, 'ukdigitaltwin')
    aggragatedBusList = [[8, 'E12000001']]
    
    # ONS
    res1 = closestBus_withinMethod(res_queryBusTopologicalInformation, res_queryPowerPlantAttributes, aggragatedBusList)
    print(res1, len(res1))
    
    # Shapely
    # res2 = closestBus_shapelyMethod(res_queryBusTopologicalInformation, res_queryPowerPlantAttributes, aggragatedBusList)
    # print(res2, len(res2))
    
    