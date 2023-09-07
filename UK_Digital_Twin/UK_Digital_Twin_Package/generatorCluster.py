###################################################
# Author: Wanni Xie (wx243@cam.ac.uk)             #
# Last Update Date: 12 April 2022                 #
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
from shapely.validation import explain_validity as make_valid
import time
import matplotlib.pyplot as plt


"""This class is designed to allow a more flexible way for selecting a suitable generator cluster 
Find the closest bus of a given GPS location (of a power plant or generator)"""
class generatorCluster(object):
    
    """"This cluster is firstly appoled in the 10-bus model (UK) as a simplest generator allocation principle, 
    which is to assign the genenrators to the bus who locatates in the same region. 
    However, when the same region has more than one bus, this cluster method may not be suitable anymore."""   
    def sameRegionWithBus(self, busInfoList, res_queryPowerPlantAttributes, aggregatedBusList):     
        ons_label = endpointList.ONS['label']
        # Find the located region of each bus
        res_queryBusTopologicalInformation = busLocatedRegionFinder(busInfoList, ons_label, 'Bus_lat_lon')
        # identify the aggragatedBus: same bus but represents different areas (regions)
        if len(aggregatedBusList) != 0:
            for aggregatedBus in aggregatedBusList:
                i = 0
                while i <= len(res_queryBusTopologicalInformation):
                #for bus in res_queryBusTopologicalInformation:
                    if i == int(aggregatedBus[0]) - 1:
                        bus = res_queryBusTopologicalInformation[i]
                        aggregatedBusDict = {}
                        aggregatedBusDict = {**bus, **aggregatedBusDict} # dictionary cannot be renamed, merge a blank dict with an exisiting one equals to rename the dict
                        aggregatedBusDict['Bus_LACode'] = str(aggregatedBus[1])
                        res_queryBusTopologicalInformation.append(aggregatedBusDict)
                        break
                    i += 1
                    
        # check one region has at most one bus as this cluster method can only be used in this occasion 
        region = [ str(r['Bus_LACode']) for r in res_queryBusTopologicalInformation ]
        duplicatesOfRegion = [k for k, v in Counter(region).items() if v > 1]
        if len(duplicatesOfRegion) > 0:
            print("The duplicatesOfRegion are: ", duplicatesOfRegion, 'the method sameRegionWithBus cannot deal with this situation, please choose another demand load allocater.')
            return None
            # raise Exception("More than one buses located in the same region. This cluster principle cannot deal with this situation.")
        elif len(region) > 12:
            raise Exception('The total number of the region exceeds 12.')
        
        print('****The cluster principle is sameRegionWithBus****')
        powerPlantAndBusPairList = []   
        for pp in res_queryPowerPlantAttributes:
            powerPlantAndBusPair = {}
            for bus in res_queryBusTopologicalInformation:
                if pp['LACode_PP'].strip() == bus['Bus_LACode'].strip():
                    powerPlantAndBusPair = {**pp, **bus}
                    del powerPlantAndBusPair['Bus_LocatedRegionBoundary']
                    powerPlantAndBusPairList.append(powerPlantAndBusPair) 
                    break
            if len(powerPlantAndBusPair) == 0: # if the length of pp has not changed which means no region being found in the bus node from the above loop
                pp_within_region = str(query_topo.queryWithinRegion(pp['LACode_PP'], ons_label)) # if the LA code attached to the pp is under the region level in hierarchy
                if str(pp_within_region).strip() != str(None): 
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
                        if "WindOffshore" in str(pp['GenerationTechnology']).strip(): # the offshore wind turbine is outside the landmass boundaries
                            raise Exception('This is a WindOffshore power plant, ', pp['PowerGenerator'], 'which may not being included in any landmass boundaries.')
                        elif len(region) < 12: 
                            print('This power plant may located in the other regions which are not be served by the given bus list, mostly likely located in Northern Ireland.')
                        elif len(region) == 12:
                            print('This power plant may not locate in the UK or its lat-lon/GenerationTechnology is wrong.')
                        else:
                            raise Exception('UNKNOWN REASON, please debug the function sameRegionWithBus.')
      
        return powerPlantAndBusPairList  
  
    
    """This cluster algorithm is firstly employed in the 29-bus model (UK) which assign the generators to its closest bus on the straight line.
    However, it may generate some unpractical design. For example, a generator located in Walse will be allocated to a bus across the Bristol channel, which is aginst the reality."""
    # This function is modified from: John Atherton (ja685@cam.ac.uk) #   
    def closestBus(self, busInfoList, res_queryPowerPlantAttributes, aggregatedBusList, busLatLonLabel:str = "Bus_lat_lon", busNodelabel:str = "Bus_node", ppLatLonLabel:str = "PP_lat_lon"):  
        print('****The cluster principle is closestBus****')
        # busInfoList = [Bus_node, Bus_lat_lon (LA code region) ]
        # res_queryPowerPlantAttributes = [PowerGenerator, LACode_PP, PP_lat_lon, PrimaryFuel, GenerationTechnology]
        ons_label = endpointList.ONS['label']
        # detect the location of the bus, in GB or in NI
        busInGB, busInNorthernIreland, countryBoundaryDict = busLocationFinderForGBOrNI(busInfoList, ons_label, busLatLonLabel)
        
        powerPlantAndBusPairList = [] 
        busNumberArray = []
        for bus in busInfoList:
            busNumberArray.append(str(bus[busNodelabel]))
      
        for pp in res_queryPowerPlantAttributes: 
            powerPlantAndBusPair = {}
            if len(busInGB) > 0:
                if 'LACode_PP' in pp.keys():
                    if pp['LACode_PP'].strip() == 'K03000001':
                        pp_within_flag = True
                    else:
                        print("---Checking if the generator/power plant located in GB (used in generator cluster of the closestBus)---")
                        pp_within_flag = query_topo.queryifWithin(pp['LACode_PP'].strip(), 'K03000001', ons_label)
                else: 
                    pp_within_flag = False
                j = 0
                distances = [65534]*len(busInGB) # the large number is the earth's circumference
                BusPowerPlantGPSPairList = []
                if pp_within_flag == True: # power plant located in GB
                    for bus in busInGB:
                        GPSLocationPair = [pp[ppLatLonLabel][0], pp[ppLatLonLabel][1], bus[busLatLonLabel][0], bus[busLatLonLabel][1]]    
                        distances[j] = GPS_distance(GPSLocationPair)
                        BusPowerPlantGPSPairList.append(GPSLocationPair)
                        j += 1
                else:
                    pp_lonlat_point = shapely.geometry.Point(pp[ppLatLonLabel][1], pp[ppLatLonLabel][0])
                    interior_pp = countryBoundaryDict['K03000001'].intersects(pp_lonlat_point)
                    if interior_pp == True:
                        for bus in busInGB:
                            GPSLocationPair = [pp[ppLatLonLabel][0], pp[ppLatLonLabel][1], bus[busLatLonLabel][0], bus[busLatLonLabel][1]]    
                            distances[j] = GPS_distance(GPSLocationPair)
                            BusPowerPlantGPSPairList.append(GPSLocationPair)
                            j += 1
                
                if min(distances) == 65534: # The power plant is not located in the area which the buses located in, like the pp in NI
                    print('######', pp['PowerGenerator'])
                    print('######The power plant is not located in GB, or the power plant is the type of offshore wind farm.')
                else:
                    bus_index = distances.index(min(distances))    
                    powerPlantAndBusPair = {**busInGB[bus_index], **pp} 
                    powerPlantAndBusPairList.append(powerPlantAndBusPair) 
                    # check if all buses are connected with generators 
                    # busNo = int(powerPlantAndBusPair['Bus_node'].split("EBus-")[1])
                    if powerPlantAndBusPair[busNodelabel] in busNumberArray:
                        busNumberArray.remove(powerPlantAndBusPair[busNodelabel])
                    continue # if the power plant is within GB, then it is not necessary to check its distance from the buses located in NI, jump from the current loop
                if len(busInNorthernIreland) > 0:
                    if 'LACode_PP' in pp.keys():
                        if pp['LACode_PP'].strip() == 'N92000002' or pp['LACode_PP'].strip() == 'N07000001':
                            pp_within_flag = True
                        else:
                            pp_within_flag = query_topo.queryifWithin(pp['LACode_PP'].strip(), 'N07000001', ons_label)
                    else: 
                        pp_within_flag = False
                    j = 0
                    distances = [65534]*len(busInNorthernIreland) # the large number is the earth's circumference
                    if pp_within_flag == True: # power plant located in NI
                        for bus in busInNorthernIreland:
                            GPSLocationPair = [pp[ppLatLonLabel][0], pp[ppLatLonLabel][1], bus[busLatLonLabel][0], bus[busLatLonLabel][1]]    
                            distances[j] = GPS_distance(GPSLocationPair)
                            j += 1
                    else: ## in case the power plant being attached with a LA code which represents UK
                        pp_lonlat_point = shapely.geometry.Point(pp[ppLatLonLabel][1], pp[ppLatLonLabel][0])
                        interior_pp = countryBoundaryDict['N92000002'].intersects(pp_lonlat_point)
                        if interior_pp == True:
                            for bus in busInNorthernIreland:
                                GPSLocationPair = [pp[ppLatLonLabel][0], pp[ppLatLonLabel][1], bus[busLatLonLabel][0], bus[busLatLonLabel][1]]    
                                distances[j] = GPS_distance(GPSLocationPair)
                                j += 1
                    
                    if min(distances) == 65534: # The power plant is not located in the area which the buses located in, like the pp in NI
                        print('######', pp['PowerGenerator'])
                        print('######The power plant is not located in Northern Ireland, or the power plant is the type of offshore wind farm.')
                    else:
                        bus_index = distances.index(min(distances))    
                        powerPlantAndBusPair = {**busInNorthernIreland[bus_index], **pp}                    
                        powerPlantAndBusPairList.append(powerPlantAndBusPair) 
                        #check if all buses are connected with generators busLocatedCountryFinder
                        # busNo = int(powerPlantAndBusPair['Bus_node'].split("EBus-")[1])
                        if powerPlantAndBusPair[busNodelabel] in busNumberArray:
                            busNumberArray.remove(powerPlantAndBusPair[busNodelabel])
        if len(busNumberArray) != 0:
            print("WARNING: There are buses not being connected by the generators, which are:", busNumberArray)
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
        ons_label = endpointList.ONS['label']
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
        ons_label = endpointList.ONS['label']
        # detect the location of the bus, in GB or in NI
        busInGB, busInNorthernIreland, countryBoundaryDict = busLocationFinderForGBOrNI(res_queryBusTopologicalInformation, ons_label)
        # query the bounderies of England&Wales, England, Wales
        EngAndWalesBound, EngBound, WalesBound, ScotlandBound = query_topo.queryEnglandAndWalesAndScotlandBounderies(ons_label)
        # the border between England and Wales
        BorderOfEnglandAndWales = make_valid(EngBound).intersection(make_valid(WalesBound))
        # the south coast and North coast of Edinburgh sea channel
        EdinburghChannelNorthCoast = make_valid(ScotlandBound).intersection(make_valid(EdinburghChannelNorthShapely))
        EdinburghChannelSouthCoast = make_valid(ScotlandBound).intersection(make_valid(EdinburghChannelSouthShapely))
        
        ###################################################
        # # Plot each polygon shape directly
        # for geom in EdinburghChannelNorthCoast.geoms:
        #     plt.plot(*geom.exterior.xy)
        # for geom in EdinburghChannelSouthCoast.geoms:
        #     plt.plot(*geom.exterior.xy)
        ##################################################
        
        powerPlantAndBusPairList = [] 
        busNumberArray = list(range(1, len(res_queryBusTopologicalInformation) + 1)) 
        mismatch = 0
        for pp in res_queryPowerPlantAttributes: 
             powerPlantAndBusPair = {}
             if len(busInGB) > 0:
                 # initial the ppWithinGBFlag for identifying if the current pp located in the GB
                 ppWithinGBFlag = False
                 if pp['LACode_PP'] == 'E92000001' or pp['LACode_PP'] == 'W08000001' or pp['LACode_PP'] == 'W92000004' \
                     or query_topo.queryifWithin(pp['LACode_PP'], 'K04000001', ons_label) == True: # power plant located in England and Wales
                      pp.update({'EWS_LACode': 'K04000001'})
                      ppWithinGBFlag = True
                         
                 elif pp['LACode_PP'] == 'S04000001' or pp['LACode_PP'] == 'S92000003' or query_topo.queryifWithin(pp['LACode_PP'], 'S04000001', ons_label) == True : # power plant located in Scotland
                        pp.update({'EWS_LACode': 'S04000001'})
                        ppWithinGBFlag = True                  
                 else: # in case the power plant being attached with a LA code which represents GB or UK
                        pp_lonlat_point = shapely.geometry.Point(pp['PP_lat_lon'][1], pp['PP_lat_lon'][0])                      
                        if make_valid(EngAndWalesBound).intersects(pp_lonlat_point) == True:
                            pp.update({'EWS_LACode': 'K04000001'})
                            ppWithinGBFlag = True    
                        elif make_valid(ScotlandBound).intersects(pp_lonlat_point) == True:
                            pp.update({'EWS_LACode': 'S04000001'})
                            ppWithinGBFlag = True

                 if ppWithinGBFlag == False:
                     print('######', pp['PowerGenerator'], pp['LACode_PP'])
                     print('######The power plant is not located in GB, or the power plant is an offshore wind farm.')
                 elif ppWithinGBFlag == True:  
                     j = 0
                     distances = [65534]*len(busInGB) # the large number is the earth's circumference
                     BusPowerPlantGPSPairList = []
                     for bus in busInGB:
                        GPSLocationPair = [pp['PP_lat_lon'][0], pp['PP_lat_lon'][1], bus['Bus_lat_lon'][0], bus['Bus_lat_lon'][1]]    
                        distances[j] = GPS_distance(GPSLocationPair)
                        BusPowerPlantGPSPairList.append(GPSLocationPair)
                        j += 1
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

                            print(BusPowerPlantGPSPair)
                            if make_valid(EngBound).disjoint(BusPowerPlantGPSTie) == False and make_valid(WalesBound).disjoint(BusPowerPlantGPSTie) == False:
                                print('!!!!!!This pp is not allowed to connected the bus', busInGB[bus_index]['Bus_node'], \
                                      busInGB[bus_index]['Bus_lat_lon'], pp['PowerGenerator'], pp['PP_lat_lon'], min(distances))
                                print(BusPowerPlantGPSPair)
                                distances[bus_index] = 65534
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
                         # check if the bus-pp tie crosses Firth of Forth Channel
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
                 if pp['LACode_PP'] == 'N92000002' or pp['LACode_PP'] == 'N07000001' or query_topo.queryifWithin(pp['LACode_PP'], 'N07000001', ons_label) == True: # power plant located in NI
                    for bus in busInNorthernIreland:
                        GPSLocationPair = [pp['PP_lat_lon'][0], pp['PP_lat_lon'][1], bus['Bus_lat_lon'][0], bus['Bus_lat_lon'][1]]    
                        distances[j] = GPS_distance(GPSLocationPair)
                        j += 1
                 else: # in case the power plant being attached with a LA code which represents UK
                        pp_lonlat_point = shapely.geometry.Point(pp['PP_lat_lon'][1], pp['PP_lat_lon'][0])
                        interior_pp = countryBoundaryDict['N92000002'].intersects(pp_lonlat_point)
                        if interior_pp == True:
                            for bus in busInNorthernIreland:
                                GPSLocationPair = [pp['PP_lat_lon'][0], pp['PP_lat_lon'][1], bus['Bus_lat_lon'][0], bus['Bus_lat_lon'][1]]    
                                distances[j] = GPS_distance(GPSLocationPair)
                                j += 1
                    
                 if min(distances) == 65534: # The power plant is not located in the area which the buses located in, like the pp in NI
                     print('######', pp['PowerGenerator'], pp['LACode_PP'])
                     print('######The power plant is not located in Northern Ireland, or the power plant is an offshore wind farm.')
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
        print('********************The number of the mismatch is', mismatch)
        return powerPlantAndBusPairList 
    
"""This function is used to check where does each bus locates in, GB or Nortern Ireland (NI)"""    
def busLocationFinderForGBOrNI(res_queryBusTopologicalInformation, ons_label, busLatLonLabel:str = "Bus_lat_lon"):
    ## declare the lists
    busInGB = []
    busInNorthernIreland = []
    countryBoundaryDict = {}
    ## query the boundary of GB and NI
    res_GBOrNIBoundary = list(query_topo.queryGBOrNIBoundary(ons_label)) # [LACode_area, Geo_InfoList]       
    for bus in res_queryBusTopologicalInformation:
      # bus['Bus_lat_lon'] = [float(bus['Bus_lat_lon'].split('#')[0]), float(bus['Bus_lat_lon'].split('#')[1])]
      bus_lonlat_point = shapely.geometry.Point(bus[busLatLonLabel][1], bus[busLatLonLabel][0])
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

#TODO: for visualisation 
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