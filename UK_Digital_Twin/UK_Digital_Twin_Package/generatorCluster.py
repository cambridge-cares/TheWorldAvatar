###################################################
# Author: Wanni Xie (wx243@cam.ac.uk)             #
# Last Update Date: 10 Jan 2022                   #
###################################################
import sys, os
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.DistanceCalculator import DistanceBasedOnGPSLocation as GPS_distance
import UK_Power_Grid_Topology_Generator.SPARQLQueriesUsedInTopologyABox as query_topo
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel as endpointList
from UK_Digital_Twin_Package.busLocatedRangeFinder import busLocatedRegionFinder, busLocatedCountryFinder
from UK_Digital_Twin_Package.polygonCoversEdinburghChannel import EdinburghChannelNorthShapely, EdinburghChannelSouthShapely, complementaryBorderShapely
from collections import Counter
import shapely.geometry
from shapely.validation import make_valid
import time
import matplotlib.pyplot as plt


"""This class is designed to allow a more flexible way for selecting a suitable generator cluster 
Find the closest bus of a given GPS location (of a power plant or generator)"""
class generatorCluster(object):
    
    """"This cluster is firstly appoled in the 10-bus model (UK) as a simplest generator allocation principle, 
    which is to assign the genenrators to the bus who locatates in the same region. 
    However, when the same region has more than one bus, this cluster method may not be suitable anymore."""   
    def sameRegionWithBus(self, res_queryBusTopologicalInformation, res_queryPowerPlantAttributes, aggregatedBusList):     
        # res_queryBusTopologicalInformation = [Bus_node, EBus, Bus_lat_lon[], Bus_LACode ]
        # res_queryPowerPlantAttributes = [PowerGenerator, LACode_PP, PP_lat_lon, PrimaryFuel, GenerationTechnology]
        ons_label = endpointList.ONS['lable']
        # Find the located region of each bus
        res_queryBusTopologicalInformation = busLocatedRegionFinder(res_queryBusTopologicalInformation, ons_label, 'Bus_lat_lon')
        
        # identify the aggragatedBus: same bus but represents different areas (regions)
        if len(aggregatedBusList) != 0:
            for aggregatedBus in aggregatedBusList:               
                for bus in res_queryBusTopologicalInformation:
                    if int(bus['Bus_node'].split('_EBus-')[1]) == int(aggregatedBus[0]):
                        aggregatedBusDict = {}
                        aggregatedBusDict = {**bus, **aggregatedBusDict} # dictionary cannot be renamed, merge a blank dict with an exisiting one equals to rename the dict
                        aggregatedBusDict['Bus_LACode'] = str(aggregatedBus[1])
                        res_queryBusTopologicalInformation.append(aggregatedBusDict)
                        break
   
        # check one region has at most one bus as this cluster method can only be used in this occasion 
        region = [ str(r['Bus_LACode']) for r in res_queryBusTopologicalInformation ]
        duplicatesOfRegion = [k for k, v in Counter(region).items() if v > 1]
        if len(duplicatesOfRegion) > 0:
            print("The duplicatesOfRegion are: ", duplicatesOfRegion, 'the method sameRegionWithBus cannot deal with this situation, please choose another demand load allocater.')
            return None
            # raise Exception("More than one buses located in the same region. This cluster principle cannot deal with this situation.")
        elif len(region) > 12:
            raise Exception('The total number of the region exceeds 12.')
        # region = sorted(set(region),key=region.index) # remove the duplicated item and keep the order of the list as original
        
        print('****The cluster principle is sameRegionWithBus****')
        powerPlantAndBusPairList = []   
        for pp in res_queryPowerPlantAttributes:
            # pp['PP_lat_lon'] = [float(pp['PP_lat_lon'].split('#')[0]), float(pp['PP_lat_lon'].split('#')[1])]
            powerPlantAndBusPair = {}
            for bus in res_queryBusTopologicalInformation:
                if str(pp['LACode_PP']) == str(bus['Bus_LACode']):
                    powerPlantAndBusPair = {**pp, **bus}
                    del powerPlantAndBusPair['Bus_LocatedRegionBoundary']
                    powerPlantAndBusPairList.append(powerPlantAndBusPair) 
                    break
            if len(powerPlantAndBusPair) == 0: # if the length of pp has not changed which means no region being found in the bus node from the above loop
                pp_within_region = str(query_topo.queryWithinRegion(pp['LACode_PP'], ons_label)) # if the LA code attached to the pp is under the region level in hierarchy
                if str(pp_within_region) != str(None): 
                    print('###########The LA code attached to the power plant is under the region level in hierarchy###########')
                    for bus in res_queryBusTopologicalInformation:
                          if bus['Bus_LACode'] in pp_within_region:
                              powerPlantAndBusPair = {**pp, **bus}
                              del powerPlantAndBusPair['Bus_LocatedRegionBoundary']
                              powerPlantAndBusPairList.append(powerPlantAndBusPair)  
                              continue
                else: 
                    print('###########The attached LA code of power plant is higher than the level of region###########')
                    pp_lonlat_point = shapely.geometry.Point(pp['PP_lat_lon'][1], pp['PP_lat_lon'][0])
                    for bus in res_queryBusTopologicalInformation:
                        busLocatedRegionBoundary = bus['Bus_LocatedRegionBoundary']
                        interior_pp = busLocatedRegionBoundary.intersects(pp_lonlat_point)
                        if interior_pp == True:
                            powerPlantAndBusPair = {**pp, **bus}
                            del powerPlantAndBusPair['Bus_LocatedRegionBoundary']
                            powerPlantAndBusPairList.append(powerPlantAndBusPair) 
                            break
                    if len(powerPlantAndBusPair) == 0:
                        print('###########The point cannot be found in the boundaries of the given buses###########')
                        if str(pp['GenerationTechnology'].split('#')[1]) == 'WindOffshore': # the offshore wind turbine is outside the landmass boundaries
                            raise Exception('This is a WindOffshore power plant, ', pp['PowerGenerator'], 'which may not being included in any landmass boundaries.')
                        elif len(region) < 12: 
                            print('This power plant may located in the other regions which are not be served by the given bus list, mostly likely located in Northern Ireland.')
                        elif len(region) == 12:
                            print('This power plant may not locate in the UK or its lat-lon/GenerationTechnology is wrong.')
                        else:
                            raise Exception('UNKNOWN REASON, please debug the function sameRegionWithBus.')
      
        return powerPlantAndBusPairList  
  
    
  
    """This cluster algorithm is firstly employed in the 29-bus model (UK) which assign the generators to its closest bus on the straight line.
    However, it may generate some unpractical design. For example, a generator located in Walse will be allocated to a bus across the Bristol channel,
    which is aginst the reality."""
    # This function is modified from: John Atherton (ja685@cam.ac.uk) #   
    def closestBus_longestRunTime(self, res_queryBusTopologicalInformation, res_queryPowerPlantAttributes, aggregatedBusList):  
      print('****The cluster principle is closestBus****')
      # res_queryBusTopologicalInformation = [Bus_node, EBus, Bus_lat_lon (LA code region) ]
      # res_queryPowerPlantAttributes = [PowerGenerator, LACode_PP, PP_lat_lon, PrimaryFuel, GenerationTechnology]
      ons_label = endpointList.ONS['lable']
      # Find which country (landmass) the bus locates in, GB or Norther Ireland
      res_queryBusTopologicalInformation = busLocatedCountryFinder(res_queryBusTopologicalInformation, ons_label, 'Bus_lat_lon')
      
      powerPlantAndBusPairList = [] 
      busNumberArray = list(range(1, len(res_queryBusTopologicalInformation) + 1))     
      for pp in res_queryPowerPlantAttributes: 
           powerPlantAndBusPair = {}
           distances = [65534]*len(res_queryBusTopologicalInformation) # the large number is the earth's circumference
           # pp['PP_lat_lon'] = [float(pp['PP_lat_lon'].split('#')[0]), float(pp['PP_lat_lon'].split('#')[1])]
           j = 0
           for bus in res_queryBusTopologicalInformation:
              pp_within_flag = query_topo.queryifWithin(pp['LACode_PP'], bus['Bus_LocatedCountry'], ons_label)
              if pp_within_flag == True or pp['LACode_PP'] == bus['Bus_LocatedCountry']:  
                  GPSLocationPair = [pp['PP_lat_lon'][0], pp['PP_lat_lon'][1], bus['Bus_lat_lon'][0], bus['Bus_lat_lon'][1]]    
                  distances[j] = GPS_distance(GPSLocationPair)
                  j += 1
              elif pp['LACode_PP'][0] == 'K02000001': # incase the power plant being attached with a LA code which represents UK
                  pp_lonlat_point = shapely.geometry.Point(pp['PP_lat_lon'][1], pp['PP_lat_lon'][0])
                  interior_pp = bus['Bus_LocatedCountry'].intersects(pp_lonlat_point)
                  if interior_pp == True:
                      GPSLocationPair = [pp['PP_lat_lon'][0], pp['PP_lat_lon'][1], bus['Bus_lat_lon'][0], bus['Bus_lat_lon'][1]]    
                      distances[j] = GPS_distance(GPSLocationPair)
                      j += 1
                  
           if min(distances) == 65534: # The power plant is not located in any area which the buses located in, like the pp in NI
               print('######', pp['PowerGenerator'], pp['LACode_PP'])
               print('######The power plant is not located in any area which the buses located in.')
           else:
               bus_index = distances.index(min(distances))    
               powerPlantAndBusPair = {**res_queryBusTopologicalInformation[bus_index], **pp} 
               del powerPlantAndBusPair['Bus_LocatedCountry']
               powerPlantAndBusPairList.append(powerPlantAndBusPair) 
               #check if all buses are connected with generators 
               busNo = int(powerPlantAndBusPair['Bus_node'].split("EBus-")[1])
               if busNo in busNumberArray:
                  busNumberArray.remove(busNo)
      if len(busNumberArray) != 0:
          print("WARNING: There are buses not being connected by the generators, which are number:", busNumberArray)
      else:
          print("************All buses are connected with generators************") 
      return powerPlantAndBusPairList 
    ###########################################################
    ##### use some pre known to avoid the multiple queries ####
    def closestBus_withPreKnown(self, res_queryBusTopologicalInformation, res_queryPowerPlantAttributes, aggregatedBusList):  
      print('****The cluster principle is closestBus****')
      # res_queryBusTopologicalInformation = [Bus_node, EBus, Bus_lat_lon (LA code region) ]
      # res_queryPowerPlantAttributes = [PowerGenerator, LACode_PP, PP_lat_lon, PrimaryFuel, GenerationTechnology]
      ons_label = endpointList.ONS['lable']
      # Find which country (landmass) the bus locates in, GB or Norther Ireland
      res_queryBusTopologicalInformation = busLocatedCountryFinder(res_queryBusTopologicalInformation, ons_label, 'Bus_lat_lon')
    
      powerPlantAndBusPairList = [] 
      busNumberArray = list(range(1, len(res_queryBusTopologicalInformation) + 1)) 
      print('The total number of power plants is:', len(res_queryPowerPlantAttributes))
      for pp in res_queryPowerPlantAttributes: 
           # print(pp['PowerGenerator'])
           powerPlantAndBusPair = {}
           distances = [65534]*len(res_queryBusTopologicalInformation) # the large number is the earth's circumference
           #pp['PP_lat_lon'] = [float(pp['PP_lat_lon'].split('#')[0]), float(pp['PP_lat_lon'].split('#')[1])]
           j = 0
           for bus in res_queryBusTopologicalInformation:
              if str(bus['Bus_LocatedCountry']) == 'K03000001': # bus in GB
                  if str(pp['LACode_PP'][0]) in ['S', 'W', 'E'] or str(pp['LACode_PP']) in ['K03000001', 'K04000001']:
                      GPSLocationPair = [pp['PP_lat_lon'][0], pp['PP_lat_lon'][1], bus['Bus_lat_lon'][0], bus['Bus_lat_lon'][1]]    
                      distances[j] = GPS_distance(GPSLocationPair)
                      j += 1
              elif str(bus['Bus_LocatedCountry']) == 'N92000002': #bus in NI   
                  if str(pp['LACode_PP'][0]) =='N':
                      GPSLocationPair = [pp['PP_lat_lon'][0], pp['PP_lat_lon'][1], bus['Bus_lat_lon'][0], bus['Bus_lat_lon'][1]]    
                      distances[j] = GPS_distance(GPSLocationPair)
                      j += 1
              elif pp['LACode_PP'][0] == 'K02000001': # in case the power plant being attached with a LA code which represents UK
                  pp_lonlat_point = shapely.geometry.Point(pp['PP_lat_lon'][1], pp['PP_lat_lon'][0])
                  interior_pp = bus['Bus_LocatedCountry'].intersects(pp_lonlat_point)
                  if interior_pp == True:
                      GPSLocationPair = [pp['PP_lat_lon'][0], pp['PP_lat_lon'][1], bus['Bus_lat_lon'][0], bus['Bus_lat_lon'][1]]    
                      distances[j] = GPS_distance(GPSLocationPair)
                      j += 1
     
           if min(distances) == 65534: # The power plant is not located in any area which the buses located in, like the pp in NI
               print('######', pp['PowerGenerator'], pp['LACode_PP'])
               print('######The power plant is not located in any area where the buses located in.')
           else:
               bus_index = distances.index(min(distances))    
               powerPlantAndBusPair = {**res_queryBusTopologicalInformation[bus_index], **pp} 
               del powerPlantAndBusPair['Bus_LocatedCountry']
               powerPlantAndBusPairList.append(powerPlantAndBusPair) 
               #check if all buses are connected with generators 
               busNo = int(powerPlantAndBusPair['Bus_node'].split("EBus-")[1])
               if busNo in busNumberArray:
                  busNumberArray.remove(busNo)
      if len(busNumberArray) != 0:
          print("WARNING: There are buses not being connected by the generators, which are number:", busNumberArray)
      else:
          print("************All buses are connected with generators************") 
      return powerPlantAndBusPairList
  
    #######################################################################################################################################
    def closestBus(self, res_queryBusTopologicalInformation, res_queryPowerPlantAttributes, aggregatedBusList):  
      print('****The cluster principle is closestBus****')
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
               BusPowerPlantGPSPairList = []
               if pp_within_flag == True or pp['LACode_PP'] == 'K03000001': # power plant located in GB
                  for bus in busInGB:
                      GPSLocationPair = [pp['PP_lat_lon'][0], pp['PP_lat_lon'][1], bus['Bus_lat_lon'][0], bus['Bus_lat_lon'][1]]    
                      distances[j] = GPS_distance(GPSLocationPair)
                      BusPowerPlantGPSPairList.append(GPSLocationPair)
                      j += 1
               elif pp['LACode_PP'] == 'K02000001': # in case the power plant being attached with a LA code which represents UK
                      pp_lonlat_point = shapely.geometry.Point(pp['PP_lat_lon'][1], pp['PP_lat_lon'][0])
                      interior_pp = countryBoundaryDict['K03000001'].intersects(pp_lonlat_point)
                      if interior_pp == True:
                          for bus in busInGB:
                              GPSLocationPair = [pp['PP_lat_lon'][0], pp['PP_lat_lon'][1], bus['Bus_lat_lon'][0], bus['Bus_lat_lon'][1]]    
                              distances[j] = GPS_distance(GPSLocationPair)
                              j += 1
                  
               if min(distances) == 65534: # The power plant is not located in the area which the buses located in, like the pp in NI
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
               pp_within_flag = query_topo.queryifWithin(pp['LACode_PP'], 'N07000001', ons_label)
               if pp_within_flag == True or pp['LACode_PP'] == 'N92000002' or pp['LACode_PP'] == 'N07000001': # power plant located in NI
                  for bus in busInNorthernIreland:
                      GPSLocationPair = [pp['PP_lat_lon'][0], pp['PP_lat_lon'][1], bus['Bus_lat_lon'][0], bus['Bus_lat_lon'][1]]    
                      distances[j] = GPS_distance(GPSLocationPair)
                      j += 1
               elif pp['LACode_PP'] == 'K02000001': # in case the power plant being attached with a LA code which represents UK
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
                   # del powerPlantAndBusPair['Bus_LocatedCountry']
                   powerPlantAndBusPairList.append(powerPlantAndBusPair) 
                   #check if all buses are connected with generators busLocatedCountryFinder
                   busNo = int(powerPlantAndBusPair['Bus_node'].split("EBus-")[1])
                   if busNo in busNumberArray:
                      busNumberArray.remove(busNo)
      if len(busNumberArray) != 0:
          print("WARNING: There are buses not being connected by the generators, which are number:", busNumberArray)
      else:
          print("************All buses are connected with generators************") 
      return powerPlantAndBusPairList 
  ##########################################################################################################################################
    """Check the Bristol channel"""
    def closestBus_withBoundCheck(self, res_queryBusTopologicalInformation, res_queryPowerPlantAttributes, aggregatedBusList):  
        print('****The cluster principle is closestBus_withBoundCheck****')
        time.gmtime(0) 
        # res_queryBusTopologicalInformation = [Bus_node, EBus, Bus_lat_lon (LA code region) ]
        # res_queryPowerPlantAttributes = [PowerGenerator, LACode_PP, PP_lat_lon, PrimaryFuel, GenerationTechnology]
        ons_label = endpointList.ONS['lable']
        # detect the location of the bus, in GB or in NI
        busInGB, busInNorthernIreland, countryBoundaryDict = busLocationFinderForGBOrNI(res_queryBusTopologicalInformation, ons_label)
        # query the bounderies of England&Wales, England, Wales
        EngAndWalesBound, EngBound, WalesBound, ScotlandBound = query_topo.queryEnglandAndWalesAndScotlandBounderies(ons_label)
        # the border between England and Wales
        BorderOfEnglandAndWales = make_valid(EngBound).intersection(make_valid(WalesBound))
        
        powerPlantAndBusPairList = [] 
        busNumberArray = list(range(1, len(res_queryBusTopologicalInformation) + 1)) 
        mismatch = 0
        for pp in res_queryPowerPlantAttributes: 
             powerPlantAndBusPair = {}
             if len(busInGB) > 0:
                 pp_within_flag = query_topo.queryifWithin(pp['LACode_PP'], 'K03000001', ons_label)
                 j = 0
                 distances = [65534]*len(busInGB) # the large number is the earth's circumference
                 BusPowerPlantGPSPairList = []
                 if pp_within_flag == True or pp['LACode_PP'] == 'K03000001': # power plant located in GB
                    for bus in busInGB:
                        GPSLocationPair = [pp['PP_lat_lon'][0], pp['PP_lat_lon'][1], bus['Bus_lat_lon'][0], bus['Bus_lat_lon'][1]]    
                        distances[j] = GPS_distance(GPSLocationPair)
                        BusPowerPlantGPSPairList.append(GPSLocationPair)
                        j += 1
                 elif pp['LACode_PP'] == 'K02000001': # in case the power plant being attached with a LA code which represents UK
                        pp_lonlat_point = shapely.geometry.Point(pp['PP_lat_lon'][1], pp['PP_lat_lon'][0])
                        interior_pp = countryBoundaryDict['K03000001'].intersects(pp_lonlat_point)
                        if interior_pp == True:
                            for bus in busInGB:
                                GPSLocationPair = [pp['PP_lat_lon'][0], pp['PP_lat_lon'][1], bus['Bus_lat_lon'][0], bus['Bus_lat_lon'][1]]    
                                distances[j] = GPS_distance(GPSLocationPair)
                                BusPowerPlantGPSPairList.append(GPSLocationPair)
                                j += 1
                    
                 if min(distances) == 65534: # The power plant is not located in the area which the buses located in, like the pp in NI
                     print('######', pp['PowerGenerator'], pp['LACode_PP'])
                     print('######The power plant is not located in GB, or the power plant is the type of offshore wind farm.')
                 else:
                     bus_index = distances.index(min(distances)) 
                     BusPowerPlantGPSPair = BusPowerPlantGPSPairList[bus_index] 
                     BusPowerPlantGPSTie = shapely.geometry.LineString([(BusPowerPlantGPSPair[1], BusPowerPlantGPSPair[0]), \
                                                                        (BusPowerPlantGPSPair[3], BusPowerPlantGPSPair[2])])  
                     
                
                     # check the mismatch which is to avoid the situation like a power plant being connected to a bus located across the Bristol channel
                     while make_valid(EngAndWalesBound).crosses(BusPowerPlantGPSTie) == True and BorderOfEnglandAndWales.crosses(BusPowerPlantGPSTie) == False \
                         and complementaryBorderShapely.crosses(BusPowerPlantGPSTie) == False:
                         print('%%%%%%%This pp-bus tie crosses the E&W', busInGB[bus_index]['Bus_node'], \
                                   busInGB[bus_index]['Bus_lat_lon'], pp['PowerGenerator'], pp['PP_lat_lon'], min(distances))
                         print('If crosses the E&W Border', BorderOfEnglandAndWales.crosses(BusPowerPlantGPSTie), BusPowerPlantGPSTie)
                         print(BusPowerPlantGPSPair)
                         if make_valid(EngBound).disjoint(BusPowerPlantGPSTie) == False and make_valid(WalesBound).disjoint(BusPowerPlantGPSTie) == False:
                             print('!!!!!!This pp is not allowed to connected the bus', busInGB[bus_index]['Bus_node'], \
                                   busInGB[bus_index]['Bus_lat_lon'], pp['PowerGenerator'], pp['PP_lat_lon'], min(distances))
                             print(BusPowerPlantGPSPair)
                             distances[bus_index] = 65534
                             # del distances[bus_index] 
                             # del BusPowerPlantGPSPairList[bus_index]
                             bus_index = distances.index(min(distances)) 
                             BusPowerPlantGPSPair = BusPowerPlantGPSPairList[bus_index] 
                             BusPowerPlantGPSTie = shapely.geometry.LineString([(BusPowerPlantGPSPair[1], BusPowerPlantGPSPair[0]), \
                                                                                (BusPowerPlantGPSPair[3], BusPowerPlantGPSPair[2])])
                             print(BusPowerPlantGPSPair)
                             mismatch +=1
                             print('^^^^^^^^The number of the mismatch is:', mismatch)
                         else:
                             break
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
                 elif pp['LACode_PP'] == 'K02000001': # in case the power plant being attached with a LA code which represents UK
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
                     # del powerPlantAndBusPair['Bus_LocatedCountry']
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
        print('********************The number of the mismatch is', mismatch)
        return powerPlantAndBusPairList 


    """This method considers both the boundaries of Bristol channel and Edinburgh sea"""    
    def closestBus_withBristolAndEdinburghBoundCheck(self, res_queryBusTopologicalInformation, res_queryPowerPlantAttributes, aggregatedBusList):  
        print('****The cluster principle is closestBus_withBristolAndEdinburghBoundCheck****')
        time.gmtime(0) 
        # res_queryBusTopologicalInformation = [Bus_node, EBus, Bus_lat_lon (LA code region) ]
        # res_queryPowerPlantAttributes = [PowerGenerator, LACode_PP, PP_lat_lon, PrimaryFuel, GenerationTechnology]
        ons_label = endpointList.ONS['lable']
        # detect the location of the bus, in GB or in NI
        busInGB, busInNorthernIreland, countryBoundaryDict = busLocationFinderForGBOrNI(res_queryBusTopologicalInformation, ons_label)
        # query the bounderies of England&Wales, England, Wales
        EngAndWalesBound, EngBound, WalesBound, ScotlandBound = query_topo.queryEnglandAndWalesAndScotlandBounderies(ons_label)
        # the border between England and Wales
        BorderOfEnglandAndWales = make_valid(EngBound).intersection(make_valid(WalesBound))
        # the south coast and North coast of Edinburgh sea channel
        EdinburghChannelNorthCoast = make_valid(ScotlandBound).intersection(make_valid(EdinburghChannelNorthShapely))
        EdinburghChannelSouthCoast = make_valid(ScotlandBound).intersection(make_valid(EdinburghChannelSouthShapely))
        
        # # Plot each polygon shape directly
        # for geom in EdinburghChannelNorthCoast.geoms:
        #     plt.plot(*geom.exterior.xy)
        # for geom in EdinburghChannelSouthCoast.geoms:
        #     plt.plot(*geom.exterior.xy)
        
        powerPlantAndBusPairList = [] 
        busNumberArray = list(range(1, len(res_queryBusTopologicalInformation) + 1)) 
        mismatch = 0
        for pp in res_queryPowerPlantAttributes: 
             powerPlantAndBusPair = {}
             if len(busInGB) > 0:
                 pp_within_flag = query_topo.queryifWithin(pp['LACode_PP'], 'K04000001', ons_label)
                 j = 0
                 distances = [65534]*len(busInGB) # the large number is the earth's circumference
                 BusPowerPlantGPSPairList = []
                 if pp_within_flag == True or pp['LACode_PP'] == 'E92000001' or pp['LACode_PP'] == 'W08000001': # power plant located in England and Wales
                      pp.update({'EWS_LACode': 'K04000001'})
                      for bus in busInGB:
                         GPSLocationPair = [pp['PP_lat_lon'][0], pp['PP_lat_lon'][1], bus['Bus_lat_lon'][0], bus['Bus_lat_lon'][1]]    
                         distances[j] = GPS_distance(GPSLocationPair)
                         BusPowerPlantGPSPairList.append(GPSLocationPair)
                         j += 1 
                         
                 elif query_topo.queryifWithin(pp['LACode_PP'], 'S04000001', ons_label) == True or pp['LACode_PP'] == 'S92000003': # power plant located in Scotland
                        pp.update({'EWS_LACode': 'S04000001'})
                        for bus in busInGB:
                            GPSLocationPair = [pp['PP_lat_lon'][0], pp['PP_lat_lon'][1], bus['Bus_lat_lon'][0], bus['Bus_lat_lon'][1]]    
                            distances[j] = GPS_distance(GPSLocationPair)
                            BusPowerPlantGPSPairList.append(GPSLocationPair)
                            j += 1
                  
                 elif pp['LACode_PP'] == 'K03000001' or pp['LACode_PP'] == 'K02000001': # in case the power plant being attached with a LA code which represents GB or UK
                        pp_lonlat_point = shapely.geometry.Point(pp['PP_lat_lon'][1], pp['PP_lat_lon'][0])
                        interior_pp_EW = EngAndWalesBound.intersects(pp_lonlat_point)
                        if interior_pp_EW == True:
                            pp.update({'EWS_LACode': 'K04000001'})
                            for bus in busInGB:
                                GPSLocationPair = [pp['PP_lat_lon'][0], pp['PP_lat_lon'][1], bus['Bus_lat_lon'][0], bus['Bus_lat_lon'][1]]    
                                distances[j] = GPS_distance(GPSLocationPair)
                                BusPowerPlantGPSPairList.append(GPSLocationPair)
                                j += 1
                        elif ScotlandBound.intersects(pp_lonlat_point) == True:
                            pp.update({'EWS_LACode': 'S04000001'})
                            for bus in busInGB:
                                GPSLocationPair = [pp['PP_lat_lon'][0], pp['PP_lat_lon'][1], bus['Bus_lat_lon'][0], bus['Bus_lat_lon'][1]]    
                                distances[j] = GPS_distance(GPSLocationPair)
                                BusPowerPlantGPSPairList.append(GPSLocationPair)
                                j += 1
          
                 if min(distances) == 65534: # The power plant is not located in the area which the buses located in, like the pp in NI
                     pp.update({'EWS_LACode': None})
                     print('######', pp['PowerGenerator'], pp['LACode_PP'])
                     print('######The power plant is not located in GB, or the power plant is the type of offshore wind farm.')
                 else:
                     bus_index = distances.index(min(distances)) 
                     BusPowerPlantGPSPair = BusPowerPlantGPSPairList[bus_index] 
                     BusPowerPlantGPSTie = shapely.geometry.LineString([(BusPowerPlantGPSPair[1], BusPowerPlantGPSPair[0]), \
                                                                        (BusPowerPlantGPSPair[3], BusPowerPlantGPSPair[2])])  
                     
                
                     # check the mismatch which is to avoid the situation like a power plant being connected to a bus located across the Bristol channel
                     if pp['EWS_LACode'] == 'K04000001':
                         while make_valid(EngAndWalesBound).crosses(BusPowerPlantGPSTie) == True and BorderOfEnglandAndWales.crosses(BusPowerPlantGPSTie) == False \
                             and complementaryBorderShapely.crosses(BusPowerPlantGPSTie) == False:
                             print('%%%%%%%This pp-bus tie crosses the E&W', busInGB[bus_index]['Bus_node'], \
                                       busInGB[bus_index]['Bus_lat_lon'], pp['PowerGenerator'], pp['PP_lat_lon'], min(distances))
                             # print('If crosses the E&W Border', BorderOfEnglandAndWales.crosses(BusPowerPlantGPSTie), BusPowerPlantGPSTie)
                             print(BusPowerPlantGPSPair)
                             if make_valid(EngBound).disjoint(BusPowerPlantGPSTie) == False and make_valid(WalesBound).disjoint(BusPowerPlantGPSTie) == False:
                                 print('!!!!!!This pp is not allowed to connected the bus', busInGB[bus_index]['Bus_node'], \
                                       busInGB[bus_index]['Bus_lat_lon'], pp['PowerGenerator'], pp['PP_lat_lon'], min(distances))
                                 print(BusPowerPlantGPSPair)
                                 distances[bus_index] = 65534
                                 # del distances[bus_index] 
                                 # del BusPowerPlantGPSPairList[bus_index]
                                 bus_index = distances.index(min(distances)) 
                                 BusPowerPlantGPSPair = BusPowerPlantGPSPairList[bus_index] 
                                 BusPowerPlantGPSTie = shapely.geometry.LineString([(BusPowerPlantGPSPair[1], BusPowerPlantGPSPair[0]), \
                                                                                    (BusPowerPlantGPSPair[3], BusPowerPlantGPSPair[2])])
                                 print(BusPowerPlantGPSPair)
                                 mismatch +=1
                                 print('^^^^^^^^The number of the mismatch is:', mismatch)
                             else:
                                 break
                     elif pp['EWS_LACode'] == 'S04000001':    
                         while make_valid(EdinburghChannelNorthCoast).crosses(BusPowerPlantGPSTie) == True and make_valid(EdinburghChannelSouthCoast).crosses(BusPowerPlantGPSTie) == True:
                             print('%%%%%%%This pp-bus tie crosses the Edinburgh channel', busInGB[bus_index]['Bus_node'], \
                                       busInGB[bus_index]['Bus_lat_lon'], pp['PowerGenerator'], pp['PP_lat_lon'], min(distances))
                             distances[bus_index] = 65534
                            
                             bus_index = distances.index(min(distances)) 
                             BusPowerPlantGPSPair = BusPowerPlantGPSPairList[bus_index] 
                             BusPowerPlantGPSTie = shapely.geometry.LineString([(BusPowerPlantGPSPair[1], BusPowerPlantGPSPair[0]), \
                                                                               (BusPowerPlantGPSPair[3], BusPowerPlantGPSPair[2])])
                             print('@@@@@@@@@The new pair is', BusPowerPlantGPSPair)
                             mismatch +=1
                             print('^^^^^^^^The number of the mismatch is:', mismatch)
                    
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
                 elif pp['LACode_PP'] == 'K02000001': # in case the power plant being attached with a LA code which represents UK
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
                     # del powerPlantAndBusPair['Bus_LocatedCountry']
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
        print('********************The number of the mismatch is', mismatch)
        return powerPlantAndBusPairList 
    
"""This function is used to check where does each bus locates in, GB or Nortern Ireland (NI)"""    
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

def generatorClusteringColour(gen_bus):
  #https://htmlcolorcodes.com/
  map_bus_dict = {
        0: "#ffffff",
        1: "#AED6F1",
        2: "#1F618D",
        3: "#F9E79F",
        4: "#99A3A4",
        5: "#1B2631",
        6: "#DC7633",
        7: "#F1C40F",
        8: "#1F618D",
        9: "#873600",
        10: "#c0c0c0",
        11: "#800000",
        12: "#808000",
        13: "#00ff00",
        14: "#ff00ff",
        15: "#5f5fff",
        16: "#5fd787",
        17: "#875f5f",
        18: "#af5f00",
        19: "#d75f5f",
        20: "#afffff",
        21: "#d7af00",
        22: "#3a3a3a",
        23: "#0000af",
        24: "#ffff00",
        25: "#5f00ff",
        26: "#5fd700",
        27: "#ffd7ff",
        28: "#080808",
        29: "#1E8449"
    }
  return map_bus_dict[(gen_bus%30)]

def genLocationJSONCreator(ret_genLocation, class_label_29_gen_GPS): 
    geojson_file = """
      {
        "type": "FeatureCollection",
        "features": ["""
      # iterating over features (rows in results array)
    for r in ret_genLocation:
          # creating point feature 
          feature = """{
            "type": "Feature",
            "properties": {
              "Name": "%s",
              "marker-color": "%s",
              "marker-size": "small",
              "marker-symbol": "circle",
              "Connected_bus": "%s",
              "PP_LAcode": "%s"
            },
            "geometry": {
              "type": "Point",
              "coordinates": [
                %s,
                %s
              ]
            }                     
          },""" %(r['PowerGenerator'].split('#')[1], generatorClusteringColour(r['Bus_node']), \
                     r['Bus_node'], str(r['LACode_PP']), r['PP_lat_lon'][1], r['PP_lat_lon'][0])         
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
    geojson_written = open(class_label_29_gen_GPS + '.geojson','w')
    geojson_written.write(geojson_file)
    geojson_written.close()
    return

if __name__ == '__main__':
    # 10-bus
    # res_queryBusTopologicalInformation = [{'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-001', 'Bus_lat_lon': '50.82237#-0.13736', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EBus-001_South_East_England.owl#EBus-001_South_East_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-002', 'Bus_lat_lon': '51.4545#-2.58796', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EBus-002_South_West_England.owl#EBus-002_South_West_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-003', 'Bus_lat_lon': '51.50733#-0.12789', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EBus-003_London.owl#EBus-003_London'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-004', 'Bus_lat_lon': '52.63089#1.29725', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EBus-004_East_of_England.owl#EBus-004_East_of_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-005', 'Bus_lat_lon': '52.63658#-1.13956', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EBus-005_East_Midlands.owl#EBus-005_East_Midlands'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-006', 'Bus_lat_lon': '52.48622#-1.89051', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EBus-006_West_Midlands_(county).owl#EBus-006_West_Midlands_(county)'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-007', 'Bus_lat_lon': '53.48075#-2.24276', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EBus-007_North_West_England.owl#EBus-007_North_West_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-008', 'Bus_lat_lon': '53.80073#-1.54924', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EBus-008_Yorkshire_and_the_Humber.owl#EBus-008_Yorkshire_and_the_Humber'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-009', 'Bus_lat_lon': '51.48158#-3.17917', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EBus-009_Wales.owl#EBus-009_Wales'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-010', 'Bus_lat_lon': '55.86423#-4.2519', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EBus-010_Scotland.owl#EBus-010_Scotland'}]
    # 29-bus
    # res_queryBusTopologicalInformation = [{'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-001', 'Bus_lat_lon': [50.82237, -0.13736], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EBus-001_South_East_England.owl#EBus-001_South_East_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-002', 'Bus_lat_lon': [51.4545, -2.58796], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EBus-002_South_West_England.owl#EBus-002_South_West_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-003', 'Bus_lat_lon': [51.50733, -0.12789], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EBus-003_London.owl#EBus-003_London'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-004', 'Bus_lat_lon': [52.63089, 1.29725], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EBus-004_East_of_England.owl#EBus-004_East_of_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-005', 'Bus_lat_lon': [52.63658, -1.13956], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EBus-005_East_Midlands.owl#EBus-005_East_Midlands'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-006', 'Bus_lat_lon': [52.48622, -1.89051], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EBus-006_West_Midlands_(county).owl#EBus-006_West_Midlands_(county)'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-007', 'Bus_lat_lon': [53.48075, -2.24276], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EBus-007_North_West_England.owl#EBus-007_North_West_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-008', 'Bus_lat_lon': [53.80073, -1.54924], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EBus-008_Yorkshire_and_the_Humber.owl#EBus-008_Yorkshire_and_the_Humber'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-009', 'Bus_lat_lon': [51.48158, -3.17917], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EBus-009_Wales.owl#EBus-009_Wales'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-010', 'Bus_lat_lon': [55.86423, -4.2519], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/10_bus_model/Model_EBus-010_Scotland.owl#EBus-010_Scotland'}]#  res_queryPowerPlantAttributes = [{'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Newlands_Farm.owl#PowerGenerator_Newlands_Farm', 'PP_lat_lon': '50.8313#-1.1961', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Walland_Farm.owl#PowerGenerator_Walland_Farm', 'PP_lat_lon': '50.97536#-4.34893', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Cloford_Common_Farm.owl#PowerGenerator_Cloford_Common_Farm', 'PP_lat_lon': '51.19769#-2.38749', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Causilgey.owl#PowerGenerator_Causilgey', 'PP_lat_lon': '50.28811#-5.11702', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Molland_Farm.owl#PowerGenerator_Molland_Farm', 'PP_lat_lon': '51.01826#-3.68641', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Park_Wall.owl#PowerGenerator_Park_Wall', 'PP_lat_lon': '51.05559#-2.45602', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Chilton_Cantelo.owl#PowerGenerator_Chilton_Cantelo', 'PP_lat_lon': '50.99387#-2.60347', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Crossways.owl#PowerGenerator_Crossways', 'PP_lat_lon': '50.6949#-2.3204', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Shaftesbury.owl#PowerGenerator_Shaftesbury', 'PP_lat_lon': '51.02141#-2.18388', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Hornacott.owl#PowerGenerator_Hornacott', 'PP_lat_lon': '50.59408#-4.40153', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Huntspill.owl#PowerGenerator_Huntspill', 'PP_lat_lon': '51.19502#-2.99749', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Bratton_Fleming.owl#PowerGenerator_Bratton_Fleming', 'PP_lat_lon': '51.1252#-3.9249', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Bedborough.owl#PowerGenerator_Bedborough', 'PP_lat_lon': '50.81785#-1.92664', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Home_Farm.owl#PowerGenerator_Home_Farm', 'PP_lat_lon': '51.06402#-4.11108', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Lee_Moor.owl#PowerGenerator_Lee_Moor', 'PP_lat_lon': '50.43031#-4.02891', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Butleigh.owl#PowerGenerator_Butleigh', 'PP_lat_lon': '51.10258#-2.67671', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Higher_Hill.owl#PowerGenerator_Higher_Hill', 'PP_lat_lon': '51.12838#-2.79477', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Bodmin.owl#PowerGenerator_Bodmin', 'PP_lat_lon': '50.48193#-4.74511', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Long_Ash_Lane.owl#PowerGenerator_Long_Ash_Lane', 'PP_lat_lon': '50.82344#-2.55279', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Wick_Red_Farm.owl#PowerGenerator_Wick_Red_Farm', 'PP_lat_lon': '51.28034#-2.98787', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Springhill.owl#PowerGenerator_Springhill', 'PP_lat_lon': '52.01991#-1.80347', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Manor_Farm_Dorset.owl#PowerGenerator_Manor_Farm_Dorset', 'PP_lat_lon': '51.0541#-2.3179', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Northmoor.owl#PowerGenerator_Northmoor', 'PP_lat_lon': '50.75296#-4.44471', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Strete.owl#PowerGenerator_Strete', 'PP_lat_lon': '50.7459#-3.3587', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Wyld_Meadow_Farm.owl#PowerGenerator_Wyld_Meadow_Farm', 'PP_lat_lon': '50.77436#-2.9518', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Ashton.owl#PowerGenerator_Ashton', 'PP_lat_lon': '51.32343#-2.16765', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/RedHill_Farm.owl#PowerGenerator_RedHill_Farm', 'PP_lat_lon': '50.95456#-3.30552', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Rainbow.owl#PowerGenerator_Rainbow', 'PP_lat_lon': '52.06971#-1.85876', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Wick_Road.owl#PowerGenerator_Wick_Road', 'PP_lat_lon': '51.28155#-2.98846', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Brook_hall.owl#PowerGenerator_Brook_hall', 'PP_lat_lon': '51.28276#-2.2138', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Crapnell_Farm.owl#PowerGenerator_Crapnell_Farm', 'PP_lat_lon': '51.20924#-2.58613', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Knockworthy_Farm.owl#PowerGenerator_Knockworthy_Farm', 'PP_lat_lon': '50.98419#-4.12456', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Oakham.owl#PowerGenerator_Oakham', 'PP_lat_lon': '52.66325#-0.7507', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Trickey_Warren.owl#PowerGenerator_Trickey_Warren', 'PP_lat_lon': '50.93045#-3.13751', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Barton_Close.owl#PowerGenerator_Barton_Close', 'PP_lat_lon': '51.15271#-4.17851', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Puriton.owl#PowerGenerator_Puriton', 'PP_lat_lon': '51.18494#-2.96882', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Cleave_Farm.owl#PowerGenerator_Cleave_Farm', 'PP_lat_lon': '51.0039#-4.1408', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Goonhilly_Downs_1.owl#PowerGenerator_Goonhilly_Downs_1', 'PP_lat_lon': '50.04132#-5.21158', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Winnards.owl#PowerGenerator_Winnards', 'PP_lat_lon': '50.46356#-4.92869', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/New_Row_Farm.owl#PowerGenerator_New_Row_Farm', 'PP_lat_lon': '51.19875#-2.5352', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Trefinnick.owl#PowerGenerator_Trefinnick', 'PP_lat_lon': '50.54207#-4.35143', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Hellrigg.owl#PowerGenerator_Hellrigg', 'PP_lat_lon': '54.84686#-3.34899', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Flimby.owl#PowerGenerator_Flimby', 'PP_lat_lon': '54.68473#-3.49499', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Potato_Pot.owl#PowerGenerator_Potato_Pot', 'PP_lat_lon': '54.64219#-3.4991', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Winscales_II.owl#PowerGenerator_Winscales_II', 'PP_lat_lon': '54.62956#-3.50162', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Fairfield.owl#PowerGenerator_Fairfield', 'PP_lat_lon': '54.57397#-3.53377', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Lambrigg.owl#PowerGenerator_Lambrigg', 'PP_lat_lon': '54.3349#-2.63798', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Hameldon_Hill.owl#PowerGenerator_Hameldon_Hill', 'PP_lat_lon': '53.76728#-2.29758', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Hameldon_Hill_Extension.owl#PowerGenerator_Hameldon_Hill_Extension', 'PP_lat_lon': '53.76728#-2.29758', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Kirkby_Moor.owl#PowerGenerator_Kirkby_Moor', 'PP_lat_lon': '54.24746#-3.14525', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Orchard_End.owl#PowerGenerator_Orchard_End', 'PP_lat_lon': '53.8965#-2.84954', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Workington_(Voridian).owl#PowerGenerator_Workington_(Voridian)', 'PP_lat_lon': '54.63883#-3.49861', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Great_Orton.owl#PowerGenerator_Great_Orton', 'PP_lat_lon': '54.87056#-3.06881', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/High_Pow.owl#PowerGenerator_High_Pow', 'PP_lat_lon': '54.77945#-3.17127', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Haverigg_III.owl#PowerGenerator_Haverigg_III', 'PP_lat_lon': '54.19985#-3.32871', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Seaforth.owl#PowerGenerator_Seaforth', 'PP_lat_lon': '53.46577#-3.02182', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Dewlay_(Garstang).owl#PowerGenerator_Dewlay_(Garstang)', 'PP_lat_lon': '53.88852#-2.78708', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Winscales_I.owl#PowerGenerator_Winscales_I', 'PP_lat_lon': '54.62743#-3.50326', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Wythegill.owl#PowerGenerator_Wythegill', 'PP_lat_lon': '54.67056#-3.54959', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Rocksavage.owl#PowerGenerator_Rocksavage', 'PP_lat_lon': '53.31472#-2.72323', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#CombinedCycleGasTurbine'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Fiddlers_Ferry.owl#PowerGenerator_Fiddlers_Ferry', 'PP_lat_lon': '53.37234#-2.68912', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#ConventionalSteam'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Carrington.owl#PowerGenerator_Carrington', 'PP_lat_lon': '53.43653#-2.40956', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#CombinedCycleGasTurbine'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Fellside_CHP.owl#PowerGenerator_Fellside_CHP', 'PP_lat_lon': '54.4152#-3.49252', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#CombinedCycleGasTurbine'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Blackburn.owl#PowerGenerator_Blackburn', 'PP_lat_lon': '53.71812#-2.53922', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#CombinedCycleGasTurbine'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Sandbach.owl#PowerGenerator_Sandbach', 'PP_lat_lon': '53.16542#-2.40665', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#CombinedCycleGasTurbine'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Pilkington_-_Greengate.owl#PowerGenerator_Pilkington_-_Greengate', 'PP_lat_lon': '53.44219#-2.74238', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#OpenCycleGasTurbine'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Heysham_2.owl#PowerGenerator_Heysham_2', 'PP_lat_lon': '54.03015#-2.91749', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Nuclear', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#AGR'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Heysham_1.owl#PowerGenerator_Heysham_1', 'PP_lat_lon': '54.03015#-2.91749', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Nuclear', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#AGR'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Fiddlers_Ferry_GT.owl#PowerGenerator_Fiddlers_Ferry_GT', 'PP_lat_lon': '53.37234#-2.68912', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Oil', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#OpenCycleGasTurbine'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Bentley.owl#PowerGenerator_Bentley', 'PP_lat_lon': '53.10553#-2.47885', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Arna_Wood.owl#PowerGenerator_Arna_Wood', 'PP_lat_lon': '54.02747#-2.82523', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Bent_Spur.owl#PowerGenerator_Bent_Spur', 'PP_lat_lon': '53.5331#-2.37818', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Safeguard_Bradwall.owl#PowerGenerator_Safeguard_Bradwall', 'PP_lat_lon': '53.16466#-2.39243', 'LACode_PP': 'E12000002', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Newlands.owl#PowerGenerator_Newlands', 'PP_lat_lon': '50.77256#-2.9579', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Francis_Court.owl#PowerGenerator_Francis_Court', 'PP_lat_lon': '50.77248#-3.46779', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Slade_Farm.owl#PowerGenerator_Slade_Farm', 'PP_lat_lon': '50.2933#-3.7671', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Bystock_Farm.owl#PowerGenerator_Bystock_Farm', 'PP_lat_lon': '50.6473#-3.38468', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Portworthy.owl#PowerGenerator_Portworthy', 'PP_lat_lon': '50.4303#-4.01809', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Severn_Beach.owl#PowerGenerator_Severn_Beach', 'PP_lat_lon': '51.55863#-2.65744', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Ninnis.owl#PowerGenerator_Ninnis', 'PP_lat_lon': '50.32172#-4.84497', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Willersey.owl#PowerGenerator_Willersey', 'PP_lat_lon': '52.08901#-1.92453', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Brent_Broad.owl#PowerGenerator_Brent_Broad', 'PP_lat_lon': '56.16735#-3.17232', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Week_Farm_2.owl#PowerGenerator_Week_Farm_2', 'PP_lat_lon': '50.83671#-3.78238', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Henbury_Quarry.owl#PowerGenerator_Henbury_Quarry', 'PP_lat_lon': '50.77173#-2.05308', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Hale.owl#PowerGenerator_Hale', 'PP_lat_lon': '50.7784#-1.864', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Eastacombe.owl#PowerGenerator_Eastacombe', 'PP_lat_lon': '50.77557#-4.33295', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Penhale.owl#PowerGenerator_Penhale', 'PP_lat_lon': '50.52256#-4.80422', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Margate.owl#PowerGenerator_Margate', 'PP_lat_lon': '50.47039#-4.69711', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Common_Farm.owl#PowerGenerator_Common_Farm', 'PP_lat_lon': '51.59693#-1.71487', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Tope_Farm.owl#PowerGenerator_Tope_Farm', 'PP_lat_lon': '50.35143#-3.66482', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Willsland.owl#PowerGenerator_Willsland', 'PP_lat_lon': '50.81723#-4.11923', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Pitts_Farm.owl#PowerGenerator_Pitts_Farm', 'PP_lat_lon': '51.20942#-2.58549', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Rolls_Royce.owl#PowerGenerator_Rolls_Royce', 'PP_lat_lon': '51.52574#-2.5703', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Palmersford.owl#PowerGenerator_Palmersford', 'PP_lat_lon': '50.80272#-1.86276', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Higher_Trenhayle.owl#PowerGenerator_Higher_Trenhayle', 'PP_lat_lon': '50.21294#-5.37046', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Great_Knowles.owl#PowerGenerator_Great_Knowles', 'PP_lat_lon': '50.80542#-4.37358', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Sandys_Moor.owl#PowerGenerator_Sandys_Moor', 'PP_lat_lon': '51.03861#-3.30755', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Dunkeswell.owl#PowerGenerator_Dunkeswell', 'PP_lat_lon': '50.86618#-3.23113', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Sharland_Farm.owl#PowerGenerator_Sharland_Farm', 'PP_lat_lon': '50.79554#-4.54534', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Woodtown.owl#PowerGenerator_Woodtown', 'PP_lat_lon': '50.77639#-1.87505', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Mendennick.owl#PowerGenerator_Mendennick', 'PP_lat_lon': '52.87688#0.52106', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Bourne_Park_2.owl#PowerGenerator_Bourne_Park_2', 'PP_lat_lon': '50.77606#-2.40583', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Hollamoor.owl#PowerGenerator_Hollamoor', 'PP_lat_lon': '51.05843#-4.0713', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Wick_Road_CIC.owl#PowerGenerator_Wick_Road_CIC', 'PP_lat_lon': '51.39666#-2.8824', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Luson.owl#PowerGenerator_Luson', 'PP_lat_lon': '50.36059#-3.93954', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Sandhill.owl#PowerGenerator_Sandhill', 'PP_lat_lon': '51.06013#-3.1991', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Slaughtergate.owl#PowerGenerator_Slaughtergate', 'PP_lat_lon': '51.0459#-2.3119', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Craymarsh.owl#PowerGenerator_Craymarsh', 'PP_lat_lon': '51.36418#-2.09505', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Kia-ora_Farm.owl#PowerGenerator_Kia-ora_Farm', 'PP_lat_lon': '50.85014#-3.42462', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Benbole.owl#PowerGenerator_Benbole', 'PP_lat_lon': '50.39666#-5.04756', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Trevemper.owl#PowerGenerator_Trevemper', 'PP_lat_lon': '50.39807#-5.08319', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Higher_Knapp_Farm.owl#PowerGenerator_Higher_Knapp_Farm', 'PP_lat_lon': '51.01937#-2.99823', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Copley_Wood.owl#PowerGenerator_Copley_Wood', 'PP_lat_lon': '53.02271#-0.72776', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Clann_Farm.owl#PowerGenerator_Clann_Farm', 'PP_lat_lon': '50.44001#-4.77838', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Tredown_Farm.owl#PowerGenerator_Tredown_Farm', 'PP_lat_lon': '50.61106#-4.26668', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Wheal_Jane.owl#PowerGenerator_Wheal_Jane', 'PP_lat_lon': '50.24132#-5.12352', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Blatchworthy.owl#PowerGenerator_Blatchworthy', 'PP_lat_lon': '51.1461#-4.13988', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Ellicombe.owl#PowerGenerator_Ellicombe', 'PP_lat_lon': '50.83341#-3.78917', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Yeowood.owl#PowerGenerator_Yeowood', 'PP_lat_lon': '51.36066#-2.79341', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Higher_Pirzwell.owl#PowerGenerator_Higher_Pirzwell', 'PP_lat_lon': '50.8787#-3.31663', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Southcombe_Farm.owl#PowerGenerator_Southcombe_Farm', 'PP_lat_lon': '50.88752#-4.20783', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Barton.owl#PowerGenerator_Barton', 'PP_lat_lon': '50.95041#-1.30955', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Bristol_Water.owl#PowerGenerator_Bristol_Water', 'PP_lat_lon': '51.39299#-2.60853', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Broadgate.owl#PowerGenerator_Broadgate', 'PP_lat_lon': '50.84891#-3.73462', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/West_Bradley.owl#PowerGenerator_West_Bradley', 'PP_lat_lon': '50.91646#-3.57257', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Rudge_Hill_Farm.owl#PowerGenerator_Rudge_Hill_Farm', 'PP_lat_lon': '50.91367#-2.31493', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Palfreys_Barton.owl#PowerGenerator_Palfreys_Barton', 'PP_lat_lon': '50.95236#-3.47657', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Whiddon_Farm.owl#PowerGenerator_Whiddon_Farm', 'PP_lat_lon': '50.75395#-4.30049', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/North_Perrott_Fruit_Farm.owl#PowerGenerator_North_Perrott_Fruit_Farm', 'PP_lat_lon': '50.88807#-2.76411', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Summerlands.owl#PowerGenerator_Summerlands', 'PP_lat_lon': '50.8627#-3.64413', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Manor_Farm_2.owl#PowerGenerator_Manor_Farm_2', 'PP_lat_lon': '52.19201#-1.77329', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Slepe_Farm.owl#PowerGenerator_Slepe_Farm', 'PP_lat_lon': '50.73707#-2.09924', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Bourne_Park_1.owl#PowerGenerator_Bourne_Park_1', 'PP_lat_lon': '50.77329#-2.37874', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Lakeside_EfW.owl#PowerGenerator_Lakeside_EfW', 'PP_lat_lon': '51.51963#-0.53445', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Waste_municipalsolidwaste', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#ConventionalSteam'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Exeter_EfW.owl#PowerGenerator_Exeter_EfW', 'PP_lat_lon': '50.70418#-3.52428', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Waste_municipalsolidwaste', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#ConventionalSteam'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Carland_Cross_RP.owl#PowerGenerator_Carland_Cross_RP', 'PP_lat_lon': '50.34995#-5.05364', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Den_Brook.owl#PowerGenerator_Den_Brook', 'PP_lat_lon': '50.785#-3.863', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Batsworthy_Cross.owl#PowerGenerator_Batsworthy_Cross', 'PP_lat_lon': '50.98024#-3.67625', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Goonhilly_Downs_2.owl#PowerGenerator_Goonhilly_Downs_2', 'PP_lat_lon': '50.04945#-5.2052', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/St_Breock.owl#PowerGenerator_St_Breock', 'PP_lat_lon': '50.4823#-4.85861', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Denzell_Downs.owl#PowerGenerator_Denzell_Downs', 'PP_lat_lon': '50.46262#-4.96945', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOnshore'}]
    res_queryBusTopologicalInformation = [{'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-001', 'Bus_lat_lon': [57.46987, -4.49067], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-001_Scotland.owl#EBus-001_Scotland'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-002', 'Bus_lat_lon': [57.47452, -1.79982], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-002_Scotland.owl#EBus-002_Scotland'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-003', 'Bus_lat_lon': [56.707, -4.01079], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-003_Scotland.owl#EBus-003_Scotland'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-004', 'Bus_lat_lon': [56.03863, -3.88907], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-004_Scotland.owl#EBus-004_Scotland'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-005', 'Bus_lat_lon': [55.80952, -4.47682], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-005_Scotland.owl#EBus-005_Scotland'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-006', 'Bus_lat_lon': [55.75094, -4.08051], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-006_Scotland.owl#EBus-006_Scotland'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-007', 'Bus_lat_lon': [55.96636, -2.40824], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-007_Scotland.owl#EBus-007_Scotland'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-008', 'Bus_lat_lon': [55.66849, -2.32998], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-008_Scotland.owl#EBus-008_Scotland'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-009', 'Bus_lat_lon': [54.94193, -2.9618], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-009_North_West_England.owl#EBus-009_North_West_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-010', 'Bus_lat_lon': [54.97442, -1.73299], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-010_North_East_England.owl#EBus-010_North_East_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-011', 'Bus_lat_lon': [53.74435, -2.75499], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-011_North_West_England.owl#EBus-011_North_West_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-012', 'Bus_lat_lon': [53.22924, -3.03174], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-012_Wales.owl#EBus-012_Wales'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-013', 'Bus_lat_lon': [53.42696, -2.37878], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-013_North_West_England.owl#EBus-013_North_West_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-014', 'Bus_lat_lon': [53.48778, -1.60162], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-014_Yorkshire_and_the_Humber.owl#EBus-014_Yorkshire_and_the_Humber'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-015', 'Bus_lat_lon': [53.90023, -0.82358], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-015_Yorkshire_and_the_Humber.owl#EBus-015_Yorkshire_and_the_Humber'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-016', 'Bus_lat_lon': [53.5973, -0.7558], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-016_East_Midlands.owl#EBus-016_East_Midlands'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-017', 'Bus_lat_lon': [52.86291, -1.25763], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-017_East_Midlands.owl#EBus-017_East_Midlands'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-018', 'Bus_lat_lon': [52.25124, -1.97351], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-018_West_Midlands_(county).owl#EBus-018_West_Midlands_(county)'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-019', 'Bus_lat_lon': [52.72692, 0.19812], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-019_East_of_England.owl#EBus-019_East_of_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-020', 'Bus_lat_lon': [52.07165, 1.06316], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-020_East_of_England.owl#EBus-020_East_of_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-021', 'Bus_lat_lon': [51.93513, 0.11679], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-021_East_of_England.owl#EBus-021_East_of_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-022', 'Bus_lat_lon': [51.92706, -0.90993], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-022_South_East_England.owl#EBus-022_South_East_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-023', 'Bus_lat_lon': [51.37497, -2.14415], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-023_South_West_England.owl#EBus-023_South_West_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-024', 'Bus_lat_lon': [51.33589, -1.07755], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-024_South_East_England.owl#EBus-024_South_East_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-025', 'Bus_lat_lon': [51.50774, -0.12715], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-025_London.owl#EBus-025_London'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-026', 'Bus_lat_lon': [51.36846, 0.74141], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-026_South_East_England.owl#EBus-026_South_East_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-027', 'Bus_lat_lon': [51.10502, 0.97611], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-027_South_East_England.owl#EBus-027_South_East_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-028', 'Bus_lat_lon': [50.91637, -1.03831], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-028_South_East_England.owl#EBus-028_South_East_England'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-029', 'Bus_lat_lon': [50.76746, -3.40616], 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-029_South_West_England.owl#EBus-029_South_West_England'}]
    
    # pp_test = [{'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Denzell_Downs.owl#PowerGenerator_Denzell_Downs', 'PP_lat_lon': '52.389119#0.054085', 'LACode_PP': 'K02000001', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Wind', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#WindOffshore'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Walland_Farm.owl#PowerGenerator_Walland_Farm', 'PP_lat_lon': '50.97536#-4.34893', 'LACode_PP': 'E12000009', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar'}]
    aggragatedBusList = [[8, 'E12000001']]
    a = generatorCluster()
    
    res_queryPowerPlantAttributes = query_topo.queryPowerPlantAttributes(None, False, 'ukdigitaltwin')
    # res1 = a.closestBus_withBoundCheck(res_queryBusTopologicalInformation, res_queryPowerPlantAttributes, aggragatedBusList)
    # res2 = a.closestBus_withBristolAndEdinburghBoundCheck(res_queryBusTopologicalInformation, res_queryPowerPlantAttributes, aggragatedBusList)
    # res = a.sameRegionWithBus(res_queryBusTopologicalInformation, res_queryPowerPlantAttributes, aggragatedBusList)
    res3 = a.closestBus(res_queryBusTopologicalInformation, res_queryPowerPlantAttributes, aggragatedBusList)
    #res = a.closestBus_reducedquery(res_queryBusTopologicalInformation, res_queryPowerPlantAttributes, aggragatedBusList)
    # res = a.closestBus_withPreKnown(res_queryBusTopologicalInformation, res_queryPowerPlantAttributes, aggragatedBusList)
    
    # print(len(res1))
    # print(len(res2))
    # print(len(res3))
    
    # for r in res1: 
    #     r['Bus_node'] = int(r['Bus_node'].split('#EquipmentConnection_EBus-')[1])
    
    # for r in res2: 
    #     r['Bus_node'] = int(r['Bus_node'].split('#EquipmentConnection_EBus-')[1])
    
    for r in res3: 
        r['Bus_node'] = int(r['Bus_node'].split('#EquipmentConnection_EBus-')[1])
    
    # genLocationJSONCreator(res1, '29-bus-boundCheck-EW')
    # genLocationJSONCreator(res2, '29-bus-boundCheck-EWS')
    genLocationJSONCreator(res3, '29-bus-No-boundCheck')
    
    # p = shapely.geometry.MultiPolygon([(0,0), (2,0), (2,1), (0,2), (1,1)])
    # p1 = shapely.geometry.Polygon([(1,2), (0,2), (1,1)])
    # p2 = shapely.geometry.Polygon([(1,2), (2,1),(2,0), (0,0)])
    # p3 = shapely.geometry.Polygon([(3, 0), (4, 0), (4, 1), (3, 1)])
    # mp = shapely.geometry.MultiPolygon([p2, p3])
    # ls1 = shapely.geometry.LineString([(1, 0.5), (3.5, 1)])
    # # ls1 = shapely.geometry.LineString([(1, 0.5), (3, 1)])
    # # ls1 = shapely.geometry.LineString([(0.5, 0.25), (0.5, 1.75)])
    # ls1 = shapely.geometry.LineString([(0.5, 0.25), (3,0.25)])
    
    
    # p1 = shapely.geometry.Polygon([(0,0), (1,0), (0,1), (1,1)])
    # p2 = shapely.geometry.Polygon([(1,2), (1,0), (2,0), (2,2)])
    # print(p1.intersection(p2))
    # print(mp.crosses(ls1))
    # print(p1.disjoint(ls1))
    # print(p2.disjoint(ls1))
    # print(p.contains(ls1))
    # print(p.intersects(ls1))
    
  