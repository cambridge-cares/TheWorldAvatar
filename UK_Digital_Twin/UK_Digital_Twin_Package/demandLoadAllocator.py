###################################################
# Author: Wanni Xie (wx243@cam.ac.uk)             #
# Last Update Date: 12 Sept 2023                  #
###################################################
from collections import Counter
import sys, os
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package.DistanceCalculator import DistanceBasedOnGPSLocation as GPS_distance
from UK_Power_Grid_Model_Generator.SPARQLQueryUsedInModel import queryElectricityConsumption_LocalArea
import UK_Power_Grid_Topology_Generator.SPARQLQueriesUsedInTopologyABox as query_topo
import UK_Power_Grid_Model_Generator.SPARQLQueryUsedInModel as query_model
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel as endpointList
from UK_Power_Grid_Topology_Generator.topologyABoxGeneration import checkaggregatedBus
from UK_Digital_Twin_Package.busLocatedRangeFinder import busLocatedRegionFinder
from UK_Digital_Twin_Package.generatorCluster import busLocationFinderForGBOrNI
from UK_Digital_Twin_Package.polygonCoversEdinburghChannel import EdinburghChannelNorthShapely, EdinburghChannelSouthShapely, complementaryBorderShapely
from math import sin, cos, sqrt, atan2, radians, degrees
import shapely.geometry
from shapely.validation import explain_validity as make_valid

"""This class is designed to provide several ways of allocating the electricity demand load to each bus based on different allocation principles"""
class demandLoadAllocator(object):
    
    """"This allocation principle is firstly appoled in the 10-bus model (UK) as a most strightforward way. 
    which is to assign the regional demanding to the bus who locatates in the same region. 
    However, when the same region has more than one bus, this method may not be suitable anymore."""  
    def regionalDemandLoad(self, res_queryBusTopologicalInformation, startTime_of_EnergyConsumption, numOfBus, res_queryElectricityConsumption_LocalArea, busLatLonLabel):
        # res_queryBusTopologicalInformation = [BusNodeIRI, BusLatLon[]]
        # res_queryElectricityConsumption_Region = [RegionOrCountry_LACode, v_TotalELecConsumption]

        # The query endpoints
        ons_label = endpointList.ONS['label']
        ons_iri = endpointList.ONS['queryendpoint_iri']
        UKPowerSystemBaseWorld_iri = endpointList.UKPowerSystemBaseWorld['queryendpoint_iri']
        # query regional consumption
        res_queryElectricityConsumption_Region = list(query_model.queryElectricityConsumption_Region(startTime_of_EnergyConsumption, UKPowerSystemBaseWorld_iri, ons_iri))
        # Find the located region of each bus 
        busLatLonKey = 'BusLatLon'
        res_queryBusTopologicalInformation = busLocatedRegionFinder(res_queryBusTopologicalInformation, ons_label, busLatLonKey)
        
        # Check one region has at most one bus as this cluster method can only be used in this occasion 
        region = [ str(r['Bus_LACode']) for r in res_queryBusTopologicalInformation]
        duplicatesOfRegion = [k for k, v in Counter(region).items() if v > 1]
        if len(duplicatesOfRegion) > 0:
            print("The duplicatesOfRegion are: ", duplicatesOfRegion, 'the method regionalDemandLoad cannot deal with this situation, please choose another demand load allocater.')
            return None, None
            # raise Exception("More than one buses located in the same region. This cluster principle cannot deal with this situation.")
        elif len(region) > 12:
            raise Exception('The total number of the region exceeds 12.')
        
        # check the aggregatedBus 
        aggregatedBusList = checkaggregatedBus(numOfBus)  
        aggregatedBusFlag = False
        # identify the aggregatedBus: same bus but represents different areas (regions)
        if len(aggregatedBusList) != 0:
            aggregatedBusFlag = True
            for aggregatedBus in aggregatedBusList:               
                for bus in res_queryBusTopologicalInformation:
                    print("...................", bus[busLatLonKey], [aggregatedBus[2], aggregatedBus[3]])
                    distance = (float(bus[busLatLonKey][0]) - float(aggregatedBus[2]))**2 + (float(bus[busLatLonKey][1]) - float(aggregatedBus[3]))**2
                    if distance < 0.0001:
                    ##if bus[busLatLonKey] == [aggregatedBus[2], aggregatedBus[3]]:
                        aggregatedBusDict = {}
                        aggregatedBusDict = {**bus, **aggregatedBusDict} # dictionary cannot be renamed, merge a blank dict with an exisiting one equals to rename the dict
                        aggregatedBusDict['Bus_LACode'] = str(aggregatedBus[1])
                        res_queryBusTopologicalInformation.append(aggregatedBusDict)
                        break
      
        print('****The demanding allocation principle is regionalDemandLoad****')
        
        busAndDemandPairList = []
        
        for consumption in res_queryElectricityConsumption_Region: 
            busAndDemandPair = {}
            for bus in res_queryBusTopologicalInformation:
                if str(consumption['RegionOrCountry_LACode']) == str(bus['Bus_LACode']):
                    busAndDemandPair = {**consumption, **bus}
                    busAndDemandPairList.append(busAndDemandPair)  
                    break
      
        return busAndDemandPairList, res_queryElectricityConsumption_LocalArea, aggregatedBusFlag
        
   
    """This method is firstly employed in the 29-bus model (UK) which assign the consumption loads to its closest bus on the straight line.
    However, it may generate some unpractical design. For example, a place located in Walse will be allocated to a bus across the Bristol channel,
    which is aginst the reality."""
    # This function is modified from: John Atherton (ja685@cam.ac.uk) #   
    def closestDemandLoad(self, res_queryBusTopologicalInformation, startTime_of_EnergyConsumption, numOfBus, res_queryElectricityConsumption_LocalArea, busLatLonLabel:str = "Bus_lat_lon" ):
      # res_queryBusTopologicalInformation = [BusNodeIRI, BusLatLon[]]
      # res_queryElectricityConsumption_LocalArea = [Area_LACode, v_TotalELecConsumption, Geo_InfoList]
      ons_label = endpointList.ONS['label']
      ons_iri = endpointList.ONS['queryendpoint_iri']
      UKPowerSystemBaseWorld_iri = endpointList.UKPowerSystemBaseWorld['queryendpoint_iri']
    #   # query the local consumption
    #   res_queryElectricityConsumption_LocalArea = list(query_model.queryElectricityConsumption_LocalArea(startTime_of_EnergyConsumption, UKPowerSystemBaseWorld_iri, ons_iri))
    
    #   print('****The cluster principle is closestDemandLoad****')
    #   # find the centroid of the polygon, the value of the 
    #   for ec in res_queryElectricityConsumption_LocalArea:
    #       if ec['Geo_InfoList'].geom_type == 'MultiPolygon':
    #          ec['Geo_InfoList'] = centroidOfMultipolygon(ec['Geo_InfoList']) 
    #       elif ec['Geo_InfoList'].geom_type == 'Polygon':
    #           lon = ec['Geo_InfoList'].centroid.x
    #           lat = ec['Geo_InfoList'].centroid.y
    #           ec['Geo_InfoList'] = [lat, lon] 
      
      # detect the location of the bus, in GB or in NI
      busInGB, busInNorthernIreland, countryBoundaryDict = busLocationFinderForGBOrNI(res_queryBusTopologicalInformation, ons_label, busLatLonLabel)   

      busAndDemandPairList = []
      busNumberArray = [] #list(range(1, len(res_queryBusTopologicalInformation) + 1)) 
      for bus in res_queryBusTopologicalInformation:
        busNumberArray.append(str(bus["BusNodeIRI"]))
      for ec in res_queryElectricityConsumption_LocalArea:
        if ec['Area_LACode'] in ["K03000001", "K02000001", "W92000004", "S92000003", "E12000001", "E12000002", "E12000003", "E12000004", "E12000005", 
                            "E12000006", "E12000007", "E12000008", "E12000009", "E13000001", "E13000002", "N92000002"]:
          continue
        busAndDemandPair = {}  
        if len(busInGB) > 0: 
            demandArea_within_flag = query_topo.queryifWithin(ec['Area_LACode'], 'K03000001', ons_label)
            if demandArea_within_flag == True: # demanding located in GB
                j = 0
                distances = [65534]*len(busInGB) # the large number is the earth's circumference
                for bus in busInGB:
                  GPSLocationPair = [float(ec['Geo_InfoList'][0]), float(ec['Geo_InfoList'][1]), float(bus['BusLatLon'][0]), float(bus['BusLatLon'][1])]    
                  distances[j] = GPS_distance(GPSLocationPair)
                  j += 1
                bus_index = distances.index(min(distances))  
                busAndDemandPair = {**busInGB[bus_index], **ec}
                ec['busNodeIRI'] = busInGB[bus_index]['BusNodeIRI']
                if len(busAndDemandPairList) != 0:
                    hitFlag = False
                    for bd in busAndDemandPairList: 
                        if str(busAndDemandPair['BusNodeIRI']) == bd['BusNodeIRI']:
                            bd['v_TotalELecConsumption'] = round((float(bd['v_TotalELecConsumption'])+ float(busAndDemandPair['v_TotalELecConsumption'])), 4)
                            hitFlag = True
                            break
                    if hitFlag == False:
                        busAndDemandPairList.append(busAndDemandPair) 
                else: 
                    busAndDemandPairList.append(busAndDemandPair) 
                    
                # busNo = int(busAndDemandPair['Bus_node'].split("EBus-")[1])
                if busAndDemandPair['BusNodeIRI'] in busNumberArray:
                   busNumberArray.remove(busAndDemandPair['BusNodeIRI'])
                continue # if the demand area is within GB, then it is not necessary to check its distance from the buses located in NI, jump from the current loop
     
        if len(busInNorthernIreland) > 0:       
            demandArea_within_flag = query_topo.queryifWithin(ec['Area_LACode'], 'N07000001', ons_label)
            if demandArea_within_flag == True: # power plant located in GB
                j = 0
                distances = [65534]*len(busInNorthernIreland) # the large number is the earth's circumference
                for bus in busInNorthernIreland:
                  GPSLocationPair = [float(ec['Geo_InfoList'][0]), float(ec['Geo_InfoList'][1]), float(bus['BusLatLon'][0]), float(bus['BusLatLon'][1])]    
                  distances[j] = GPS_distance(GPSLocationPair)
                  j += 1
                
                bus_index = distances.index(min(distances))    
                busAndDemandPair = {**busInNorthernIreland[bus_index], **ec}
                ec['busNodeIRI'] = busInGB[bus_index]['BusNodeIRI']
                if len(busAndDemandPairList) != 0:
                    hitFlag = False
                    for bd in busAndDemandPairList: 
                        if str(busAndDemandPair['Bus_node']) == bd['Bus_node']:
                            bd['v_TotalELecConsumption'] = round((float(bd['v_TotalELecConsumption']) + float(busAndDemandPair['v_TotalELecConsumption'])), 4)
                            hitFlag = True
                            break
                    if hitFlag == False:
                        busAndDemandPairList.append(busAndDemandPair) 
                else: 
                    busAndDemandPairList.append(busAndDemandPair)   
                    
                if busAndDemandPair['BusNodeIRI'] in busNumberArray:
                   busNumberArray.remove(busAndDemandPair['BusNodeIRI'])

      # check if all buses are assigned with loads  
      if len(busNumberArray) != 0:
          print("WARNING: There are buses not being assigned with any load, which are:", busNumberArray)
      else:
          print("************All buses are assigned with demand loads************") 

      aggregatedBusFlag = False       
      return busAndDemandPairList, res_queryElectricityConsumption_LocalArea, aggregatedBusFlag

    

############################################THE FOLLOWING ARE NOT IN USE######################################################################################################################
 #TODO: Add boundary checking    
    def closestDemandLoad_withEWSBoundCheck(self, res_queryBusTopologicalInformation, startTime_of_EnergyConsumption, numOfBus, numOfBranch):
      # res_queryBusTopologicalInformation = [Bus_node, EBus, Bus_lat_lon[]]
      # res_queryElectricityConsumption_LocalArea = [Area_LACode, v_TotalELecConsumption, Geo_InfoList]
      print('****The cluster principle is closestDemandLoad_withEWSBoundCheck****')
      ons_label = endpointList.ONS['label']
      ons_iri = endpointList.ONS['queryendpoint_iri']
      UKPowerSystemBaseWorld_iri = endpointList.UKPowerSystemBaseWorld['queryendpoint_iri']
      # query the local consumption
      res_queryElectricityConsumption_LocalArea = list(query_model.queryElectricityConsumption_LocalArea(startTime_of_EnergyConsumption, UKPowerSystemBaseWorld_iri, ons_iri))
      # detect the location of the bus, in GB or in NI
      busInGB, busInNorthernIreland, countryBoundaryDict = busLocationFinderForGBOrNI(res_queryBusTopologicalInformation, ons_label)   
      # query the bounderies of England&Wales, England, Wales
      EngAndWalesBound, EngBound, WalesBound, ScotlandBound = query_topo.queryEnglandAndWalesAndScotlandBounderies(ons_label)
      # the border between England and Wales
      BorderOfEnglandAndWales = make_valid(EngBound).intersection(make_valid(WalesBound))
      # the south coast and North coast of Edinburgh sea channel
      EdinburghChannelNorthCoast = make_valid(ScotlandBound).intersection(make_valid(EdinburghChannelNorthShapely))
      EdinburghChannelSouthCoast = make_valid(ScotlandBound).intersection(make_valid(EdinburghChannelSouthShapely))
      
      # find the centroid of the polygon, the value of the 
      for ec in res_queryElectricityConsumption_LocalArea:
          if ec['Geo_InfoList'].geom_type == 'MultiPolygon':
             ec['Geo_InfoList'] = centroidOfMultipolygon_withAreaWeighted(ec['Geo_InfoList']) 
          elif ec['Geo_InfoList'].geom_type == 'Polygon':
              lon = ec['Geo_InfoList'].centroid.x
              lat = ec['Geo_InfoList'].centroid.y
              ec['Geo_InfoList'] = [lat, lon] 

      busAndDemandPairList = []
      busAndDemandPairList_duplicated = []
      busNumberArray = list(range(1, len(res_queryBusTopologicalInformation) + 1)) 
      mismatch = 0
      for ec in res_queryElectricityConsumption_LocalArea:
        busAndDemandPair = {}  
        if len(busInGB) > 0: 
            # initial the demandAreaWithinGBFlag for identifying if the current pp located in the GB
            demandAreaWithinGBFlag = False
            if query_topo.queryifWithin(ec['Area_LACode'], 'K04000001', ons_label) == True: # demand area located in England and Wales
                ec.update({'EWS_LACode': 'K04000001'})
                demandAreaWithinGBFlag = True               
            elif query_topo.queryifWithin(ec['Area_LACode'], 'S04000001', ons_label) == True: # check if the demand area is located in Scotland
                ec.update({'EWS_LACode': 'S04000001'})
                demandAreaWithinGBFlag = True               
            
            if demandAreaWithinGBFlag == False:
                print('######', ec['Area_LACode'], ' is not located in GB.')
            elif demandAreaWithinGBFlag == True:  
                j = 0
                distances = [65534]*len(busInGB) # the large number is the earth's circumference
                BusDemandAreaGPSPairList = []
                for bus in busInGB:
                  GPSLocationPair = [float(ec['Geo_InfoList'][0]), float(ec['Geo_InfoList'][1]), float(bus['Bus_lat_lon'][0]), float(bus['Bus_lat_lon'][1])]    
                  distances[j] = GPS_distance(GPSLocationPair)
                  BusDemandAreaGPSPairList.append(GPSLocationPair)
                  j += 1
                  
                bus_index = distances.index(min(distances))  
                BusDemandAreaGPSPair = BusDemandAreaGPSPairList[bus_index] 
                BusDemandAreaGPSTie = shapely.geometry.LineString([(BusDemandAreaGPSPair[1], BusDemandAreaGPSPair[0]), \
                                                                                    (BusDemandAreaGPSPair[3], BusDemandAreaGPSPair[2])])
                
                # check the mismatch which is to avoid the situation like a power plant being connected to a bus located across the Bristol channel
                if ec['EWS_LACode'] == 'K04000001':
                   while make_valid(EngAndWalesBound).crosses(BusDemandAreaGPSTie) == True and BorderOfEnglandAndWales.crosses(BusDemandAreaGPSTie) == False \
                       and complementaryBorderShapely.crosses(BusDemandAreaGPSTie) == False:
                       print('%%%%%%%This demand-bus tie crosses the E&W', busInGB[bus_index]['Bus_node'], \
                                 busInGB[bus_index]['Bus_lat_lon'], ec['Area_LACode'], ec['Geo_InfoList'], min(distances))
                       # print('If crosses the E&W Border', BorderOfEnglandAndWales.crosses(BusDemandAreaGPSTie), BusDemandAreaGPSTie)
                       print(BusDemandAreaGPSPair)
                       if make_valid(EngBound).disjoint(BusDemandAreaGPSTie) == False and make_valid(WalesBound).disjoint(BusDemandAreaGPSTie) == False:
                           print('!!!!!!This demand area is not allowed to be allocated to the bus', busInGB[bus_index]['Bus_node'], \
                                 busInGB[bus_index]['Bus_lat_lon'], ec['Area_LACode'], ec['Geo_InfoList'], min(distances))
                           print(BusDemandAreaGPSPair)
                           distances[bus_index] = 65534
                           bus_index = distances.index(min(distances))  
                           BusDemandAreaGPSPair = BusDemandAreaGPSPairList[bus_index] 
                           BusDemandAreaGPSTie = shapely.geometry.LineString([(BusDemandAreaGPSPair[1], BusDemandAreaGPSPair[0]), \
                                                                                    (BusDemandAreaGPSPair[3], BusDemandAreaGPSPair[2])])
                           print('@@@@@@@@@The new pair is', BusDemandAreaGPSPair)
                           mismatch +=1
                           print('^^^^^^^^The number of the mismatch is:', mismatch)
                       else:
                           break
                elif ec['EWS_LACode'] == 'S04000001':    
                   while make_valid(EdinburghChannelNorthCoast).crosses(BusDemandAreaGPSTie) == True and make_valid(EdinburghChannelSouthCoast).crosses(BusDemandAreaGPSTie) == True:
                       print('%%%%%%%This pp-bus tie crosses the Edinburgh channel', busInGB[bus_index]['Bus_node'], \
                                 busInGB[bus_index]['Bus_lat_lon'], ec['Area_LACode'], ec['Geo_InfoList'], min(distances))
                       distances[bus_index] = 65534
                      
                       bus_index = distances.index(min(distances))  
                       BusDemandAreaGPSPair = BusDemandAreaGPSPairList[bus_index] 
                       BusDemandAreaGPSTie = shapely.geometry.LineString([(BusDemandAreaGPSPair[1], BusDemandAreaGPSPair[0]), \
                                                                                    (BusDemandAreaGPSPair[3], BusDemandAreaGPSPair[2])])
                       print('@@@@@@@@@The new pair is', BusDemandAreaGPSPair)
                       mismatch +=1
                       print('^^^^^^^^The number of the mismatch is:', mismatch)
                   
                busAndDemandPair = {**busInGB[bus_index], **ec}
                if len(busAndDemandPairList) != 0:
                    hitFlag = False
                    for bd in busAndDemandPairList: 
                        if str(busAndDemandPair['Bus_node']) == bd['Bus_node']:
                            bd['v_TotalELecConsumption'] = round((float(bd['v_TotalELecConsumption'])+ float(busAndDemandPair['v_TotalELecConsumption'])), 4)
                            hitFlag = True
                            break
                    if hitFlag == False:
                        busAndDemandPairList.append(busAndDemandPair) 
                else: 
                    busAndDemandPairList.append(busAndDemandPair) 
                    
                ###########################################################
                # busAndDemandPairList_duplicated is used to visualise the allocation result in testing mode
                busAndDemandPairList_duplicated.append(busAndDemandPair)  
                ###########################################################
                
                busNo = int(busAndDemandPair['Bus_node'].split("EBus-")[1])
                if busNo in busNumberArray:
                   busNumberArray.remove(busNo)
                continue # if the demand area is within GB, then it is not necessary to check its distance from the buses located in NI, jump from the current loop
      
        if len(busInNorthernIreland) > 0:       
            #demandArea_within_flag = query_topo.queryifWithin(ec['Area_LACode'], 'N07000001', ons_label)
            if query_topo.queryifWithin(ec['Area_LACode'], 'N07000001', ons_label) == True: # demand area located in NI
                j = 0
                distances = [65534]*len(busInNorthernIreland) # the large number is the earth's circumference
                for bus in busInNorthernIreland:
                  GPSLocationPair = [float(ec['Geo_InfoList'][0]), float(ec['Geo_InfoList'][1]), float(bus['Bus_lat_lon'][0]), float(bus['Bus_lat_lon'][1])]    
                  distances[j] = GPS_distance(GPSLocationPair)
                  j += 1
                
                bus_index = distances.index(min(distances))    
                busAndDemandPair = {**busInNorthernIreland[bus_index], **ec}
                if len(busAndDemandPairList) != 0:
                    hitFlag = False
                    for bd in busAndDemandPairList: 
                        if str(busAndDemandPair['Bus_node']) == bd['Bus_node']:
                            bd['v_TotalELecConsumption'] = round((float(bd['v_TotalELecConsumption']) + float(busAndDemandPair['v_TotalELecConsumption'])), 4)
                            hitFlag = True
                            break
                    if hitFlag == False:
                        busAndDemandPairList.append(busAndDemandPair) 
                else: 
                    busAndDemandPairList.append(busAndDemandPair)  
                    
                ###########################################################
                # busAndDemandPairList_duplicated is used to visualise the allocation result in testing mode
                busAndDemandPairList_duplicated.append(busAndDemandPair)  
                ###########################################################  
                
                busNo = int(busAndDemandPair['Bus_node'].split("EBus-")[1])
                if busNo in busNumberArray:
                   busNumberArray.remove(busNo)
      
      # check if all buses are assigned with loads  
      if len(busNumberArray) != 0:
          print("WARNING: There are buses not being assigned with any load, which are number:", busNumberArray)
      else:
          print("************All buses are assigned with demand loads************") 
      aggregatedBusFlag = False      
      return busAndDemandPairList, busAndDemandPairList_duplicated, aggregatedBusFlag 

# This method is developed to find an approximal centroid of the Multipolygon. 
# The centroid of each polygon made up the multipolygon is found at first and the centroid of the multipolygon is approximated by the centre of those centroids.
def centroidOfMultipolygon(multipolygon):
  # x ,y , z = 0, 0, 0
  centroidPointList = []
  for polygon in multipolygon:
    centroidPoint = polygon.centroid
    lon = polygon.centroid.x
    lat = polygon.centroid.y
    # print('lon-lat', lon, lat)
    centroidPoint = [lat, lon]
    centroidPointList.append(centroidPoint)
  centroid = centroidOfMultiplePoints(centroidPointList)
  return centroid

# This function is developed to find the centre of multiple points
def centroidOfMultiplePoints(PointList):
  x ,y , z = 0, 0, 0  
  numOfPoint = len(PointList) 
  for lat, lon in PointList:
      lat = radians(float(lat))
      lon = radians(float(lon))
      
      x += cos(lat) * cos(lon)
      y += cos(lat) * sin(lon)
      z += sin(lat)

  x = float(x/numOfPoint)
  y = float(y/numOfPoint)
  z = float(z/numOfPoint)
  
  lon = degrees(atan2(y,x)) 
  lat = degrees(atan2(z, sqrt(x * x + y * y)))  
  centroid = [lat, lon]  
  return centroid

#TODO: add the area Weighted comparison 
def centroidOfMultipolygon_withAreaWeighted(multipolygon):
  # x ,y , z = 0, 0, 0
  centroidPointList = []
  arealist = []
  polygonList = multipolygon.geoms
  for polygon in polygonList:
    centroidPoint = polygon.centroid
    areaOfpolygon = polygon.area
    lon = polygon.centroid.x
    lat = polygon.centroid.y
    # print('lon-lat', lon, lat)
    centroidPoint = [lat, lon]
    centroidPointList.append(centroidPoint)
    arealist.append(areaOfpolygon)
  centroid = centroidOfMultiplePoints_withAreaWeighted(centroidPointList, arealist)
  return centroid

def centroidOfMultiplePoints_withAreaWeighted(PointList, arealist):
  x ,y , z = 0, 0, 0 
  index = 0
  
  numOfPoint = len(PointList)
  totalAreaOfMultipolygon = sum(arealist)
  
  for lat, lon in PointList:
      lat = radians(float(lat))
      lon = radians(float(lon))
      
      ratio = arealist[index] / totalAreaOfMultipolygon
    
      x += cos(lat) * cos(lon) * ratio
      y += cos(lat) * sin(lon) * ratio
      z += sin(lat) * ratio
      index += 1
      
  x = float(x/numOfPoint)
  y = float(y/numOfPoint)
  z = float(z/numOfPoint)
  
  lon = degrees(atan2(y,x)) 
  lat = degrees(atan2(z, sqrt(x * x + y * y)))  
  centroid = [lat, lon]  
  return centroid

def test_MultipolygonCentroid():
    ons_label = endpointList.ONS['label']
    cardiffBoundary = query_topo.queryCardiffBound(ons_label)
    # arealist = []
    # polygonListOfcardiffBoundary = cardiffBoundary.geoms
    # for polygon in polygonListOfcardiffBoundary:    
    #     areaOfpolygon = polygon.area
    #     arealist.append(areaOfpolygon)
    
    # ratio = arealist[1] / arealist[0]
    # print(ratio)
    centroid = centroidOfMultipolygon_withAreaWeighted(cardiffBoundary)
    return centroid

