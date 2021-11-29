###################################################
# Author: Wanni Xie (wx243@cam.ac.uk)             #
# Last Update Date: 29 Nov 2021                   #
###################################################

import UK_Power_Grid_Topology_Generator.SPARQLQueriesUsedInTopologyABox as query_topo
import shapely.geometry

"""
This function is developed to find the region where the bus located in.
Arguments of this function are described below:
busInfoList(list[{}]): the query results which is formated as a list with the element as a dictionary. This list needs to provide bus uri and bus location in lat-lon sequernce.
            example: [{'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-001', 'Bus_lat_lon': [57.46987, -4.49067]}]  
ons_label(str): the ONS database query endpoint, http://statistics.data.gov.uk/sparql.json
Bus_lat_lon_key (str): the ket of the dictionary whose the value is the lat-lon pair  
"""
def busLocatedRegionFinder(busInfoList, ons_label, Bus_lat_lon_key):
    res_regionalBoundary = list(query_topo.queryRegionBoundaries(ons_label)) # [LACode_area, Geo_InfoList]       
    for bus in busInfoList:
        lengthBusDict = len(bus)
        bus_lonlat_point = shapely.geometry.Point(float(bus[Bus_lat_lon_key][1]), float(bus[Bus_lat_lon_key][0]))
        for region in res_regionalBoundary:
            regionBoundary = region['Geo_InfoList']
            interior = regionBoundary.intersects(bus_lonlat_point)
            if interior == True:
                bus.update({'Bus_LACode': str(region['LACode_area'])})
                bus.update({'Bus_LocatedRegionBoundary': region['Geo_InfoList']})
        if len(bus) == lengthBusDict:
            raise Exception('Cannot find the located region of the given bus node', bus, 'please check the lat-lon attribute of the bus.')      
    return busInfoList

"""
This function is developed to find the countrt (GB or Northern Ireland) where the bus located in.
Arguments of this function are described below:
busInfoList(list[{}]): the query results which is formated as a list with the element as a dictionary. This list needs to provide bus uri and bus location in lat-lon sequernce.
            example: [{'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-001', 'Bus_lat_lon': [57.46987, -4.49067]}]  
ons_label(str): the ONS database query endpoint, http://statistics.data.gov.uk/sparql.json
Bus_lat_lon_key (str): the ket of the dictionary whose the value is the lat-lon pair  
"""
def busLocatedCountryFinder(busInfoList, ons_label, Bus_lat_lon_key):
    res_GBOrNIBoundary = list(query_topo.queryGBOrNIBoundary(ons_label)) # [LACode_area, Geo_InfoList]       
    for bus in busInfoList: 
      bus_lonlat_point = shapely.geometry.Point(float(bus[Bus_lat_lon_key][1]), float(bus[Bus_lat_lon_key][0]))
      for country in res_GBOrNIBoundary:
          countryBoundary = country['Geo_InfoList']
          interior = countryBoundary.intersects(bus_lonlat_point)
          if interior == True:
              bus.update({'Bus_LocatedCountry':str(country['LACode_area'])})   
    return busInfoList
    