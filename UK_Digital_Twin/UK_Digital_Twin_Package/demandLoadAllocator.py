###################################################
# Author: Wanni Xie (wx243@cam.ac.uk)             #
# Last Update Date: 14 Oct 2021                   #
###################################################
from UK_Digital_Twin_Package.DistanceCalculator import DistanceBasedOnGPSLocation as GPS_distance
from collections import Counter
import sys, os
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Power_Grid_Model_Generator.SPARQLQueryUsedInModel import queryElectricityConsumption_LocalArea
from math import sin, cos, sqrt, atan2, radians, degrees

"""This class is designed to provide several ways of allocating the electricity demand load to each bus based on different allocation principles"""
class demandLoadAllocator(object):
    
    """"This allocation principle is firstly appoled in the 10-bus model (UK) as a most strightforward way. 
    which is to assign the regional demanding to the bus who locatates in the same region. 
    However, when the same region has more than one bus, this method may not be suitable anymore."""   
    def regionalDemandLoad(self, res_queryBusLocation, res_queryElectricityConsumption_Region, res_queryElectricityConsumption_LocalArea):
        # check one region has at most one bus as this ckuster method can only be used in this occasion 
        region = [ str(r['Region']) for r in res_queryBusLocation]
        duplicatesOfRegion = [k for k, v in Counter(region).items() if v > 1]
        if len(duplicatesOfRegion) > 0:
            print("The duplicatesOfRegion are: ", duplicatesOfRegion)
            raise Exception("Sorry, more than one buses located in the same region. This principle cannot deal with this situation.")
        
        print('****The demanding allocation principle is regionalDemandLoad****')
        
        busAndDemandPairList = []
        
        for consumption in res_queryElectricityConsumption_Region:   
            if str(consumption['Region']) in region:
                indexOfBus = region.index(str(consumption['Region']))
                connectedBusNode = str(res_queryBusLocation[indexOfBus]['EBus'])               
                busAndDemandPair = []               
                busAndDemandPair.append(connectedBusNode)
                busAndDemandPair.append(consumption['TotalELecConsumption'])
               
                busAndDemandPairList.append(busAndDemandPair)  
            else:
                continue
        
        return busAndDemandPairList
        
    
    """This method is firstly employed in the 29-bus model (UK) which assign the consumption loads to its closest bus on the straight line.
    However, it may generate some unpractical design. For example, a place located in Walse will be allocated to a bus across the Bristol channel,
    which is aginst the reality."""
    # This function is modified from: John Atherton (ja685@cam.ac.uk) #   
    def closestDemandLoad(self, res_queryBusLocation, res_queryElectricityConsumption_Region, res_queryElectricityConsumption_LocalArea):  
      print('****The cluster principle is closestDemandLoad****')
      # find the centroid of the polygon
      for ec in res_queryElectricityConsumption_LocalArea:
          if ec['Geo_InfoList'].geom_type == 'MultiPolygon':
             ec['Geo_InfoList'] = centroidOfMultipolygon(ec['Geo_InfoList']) 
          elif ec['Geo_InfoList'].geom_type == 'Polygon':
              lon = ec['Geo_InfoList'].centroid.x
              lat = ec['Geo_InfoList'].centroid.y
              ec['Geo_InfoList'] = [lat, lon] 
             
      busAndDemandPairList = [ [ str(b['EBus']), 0.00 ]  for b in res_queryBusLocation ] 
      
      for ec in res_queryElectricityConsumption_LocalArea:
        distances = [65534]*len(res_queryBusLocation) # the large number is the earth's circumference
        j = 0
        for bus in res_queryBusLocation:
          GPSLocationPair = [float(ec['Geo_InfoList'][0]), float(ec['Geo_InfoList'][1]), float(bus['Bus_lat']), float(bus['Bus_lon'])]    
          distances[j] = GPS_distance(GPSLocationPair)
          j += 1
        bus_index = distances.index(min(distances)) 
        connectedBusNode = str(res_queryBusLocation[bus_index]['EBus'])
        for bd in busAndDemandPairList:
            if str(connectedBusNode) in bd:
                bd[1] = round((bd[1] + float(ec['TotalELecConsumption'])), 4)
    
      # check if all buses are assigned with loads   
      Non_AllocatedBus = []
      for bd in busAndDemandPairList:
          if bd[1] == 0.00:
              Non_AllocatedBus.append(bd[0])
      if len(Non_AllocatedBus) != 0:
            print("WARNING: There are buses not being assigned with loads, which are number:", Non_AllocatedBus)
      else:
            print("All buses are assigned with loads.") 
            
      return busAndDemandPairList 

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

if __name__ == '__main__':
    res_queryBusTopologicalInformation = [{'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-001', 'Bus_lat': '-0.1373639', 'Region': 'https://dbpedia.org/page/South_East_England', 'Bus_lon': '50.8223711'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-002', 'Bus_lat': '-2.5879675', 'Region': 'https://dbpedia.org/page/South_West_England', 'Bus_lon': '51.4545085'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-003', 'Bus_lat': '-0.1278966', 'Region': 'https://dbpedia.org/page/London', 'Bus_lon': '51.5073321'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-004', 'Bus_lat': '1.2972594', 'Region': 'https://dbpedia.org/page/East_of_England', 'Bus_lon': '52.6308914'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-005', 'Bus_lat': '-1.1395656', 'Region': 'https://dbpedia.org/page/East_Midlands', 'Bus_lon': '52.6365868'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-006', 'Bus_lat': '-1.8905143', 'Region': 'https://dbpedia.org/page/West_Midlands_(county)', 'Bus_lon': '52.4862263'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-007', 'Bus_lat': '-2.2427672', 'Region': 'https://dbpedia.org/page/North_West_England', 'Bus_lon': '53.4807532'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-008', 'Bus_lat': '-1.5492442', 'Region': 'https://dbpedia.org/page/Yorkshire_and_the_Humber', 'Bus_lon': '53.8007312'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-008', 'Bus_lat': '-1.5492442', 'Region': 'https://dbpedia.org/page/North_East_England', 'Bus_lon': '53.8007312'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-009', 'Bus_lat': '-3.1791789', 'Region': 'https://dbpedia.org/page/Wales', 'Bus_lon': '51.4815857'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/10_bus_model.owl#EquipmentConnection_EBus-010', 'Bus_lat': '-4.2519078', 'Region': 'https://dbpedia.org/page/Scotland', 'Bus_lon': '55.8642343'}]
    res_29_bus = [{'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-001', 'Bus_lat': '57.4698798', 'Region': 'https://dbpedia.org/page/Scotland', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-001_Scotland.owl#EBus-001_Scotland', 'Bus_lon': '-4.4906735'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-002', 'Bus_lat': '57.4745293', 'Region': 'https://dbpedia.org/page/Scotland', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-002_Scotland.owl#EBus-002_Scotland', 'Bus_lon': '-1.7998211'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-003', 'Bus_lat': '56.7070037', 'Region': 'https://dbpedia.org/page/Scotland', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-003_Scotland.owl#EBus-003_Scotland', 'Bus_lon': '-4.0107947'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-004', 'Bus_lat': '56.0386335', 'Region': 'https://dbpedia.org/page/Scotland', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-004_Scotland.owl#EBus-004_Scotland', 'Bus_lon': '-3.8890767'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-005', 'Bus_lat': '55.8095298', 'Region': 'https://dbpedia.org/page/Scotland', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-005_Scotland.owl#EBus-005_Scotland', 'Bus_lon': '-4.4768292'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-006', 'Bus_lat': '55.7509421', 'Region': 'https://dbpedia.org/page/Scotland', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-006_Scotland.owl#EBus-006_Scotland', 'Bus_lon': '-4.0805189'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-007', 'Bus_lat': '55.966361', 'Region': 'https://dbpedia.org/page/Scotland', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-007_Scotland.owl#EBus-007_Scotland', 'Bus_lon': '-2.4082467'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-008', 'Bus_lat': '55.6684972', 'Region': 'https://dbpedia.org/page/Scotland', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-008_Scotland.owl#EBus-008_Scotland', 'Bus_lon': '-2.3299805'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-009', 'Bus_lat': '54.9419311', 'Region': 'https://dbpedia.org/page/North_West_England', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-009_North_West_England.owl#EBus-009_North_West_England', 'Bus_lon': '-2.9618091'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-010', 'Bus_lat': '54.9744212', 'Region': 'https://dbpedia.org/page/North_East_England', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-010_North_East_England.owl#EBus-010_North_East_England', 'Bus_lon': '-1.7329921'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-011', 'Bus_lat': '53.7443568', 'Region': 'https://dbpedia.org/page/North_West_England', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-011_North_West_England.owl#EBus-011_North_West_England', 'Bus_lon': '-2.7549931'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-012', 'Bus_lat': '53.2292472', 'Region': 'https://dbpedia.org/page/Wales', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-012_Wales.owl#EBus-012_Wales', 'Bus_lon': '-3.0317476'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-013', 'Bus_lat': '53.4269672', 'Region': 'https://dbpedia.org/page/North_West_England', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-013_North_West_England.owl#EBus-013_North_West_England', 'Bus_lon': '-2.3787821'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-014', 'Bus_lat': '53.4877894', 'Region': 'https://dbpedia.org/page/Yorkshire_and_the_Humber', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-014_Yorkshire_and_the_Humber.owl#EBus-014_Yorkshire_and_the_Humber', 'Bus_lon': '-1.6016288'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-015', 'Bus_lat': '53.9002325', 'Region': 'https://dbpedia.org/page/Yorkshire_and_the_Humber', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-015_Yorkshire_and_the_Humber.owl#EBus-015_Yorkshire_and_the_Humber', 'Bus_lon': '-0.8235841'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-016', 'Bus_lat': '53.5973069', 'Region': 'https://dbpedia.org/page/East_Midlands', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-016_East_Midlands.owl#EBus-016_East_Midlands', 'Bus_lon': '-0.755805'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-017', 'Bus_lat': '52.862919', 'Region': 'https://dbpedia.org/page/East_Midlands', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-017_East_Midlands.owl#EBus-017_East_Midlands', 'Bus_lon': '-1.257635'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-018', 'Bus_lat': '52.2512438', 'Region': 'https://dbpedia.org/page/West_Midlands_(county)', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-018_West_Midlands_(county).owl#EBus-018_West_Midlands_(county)', 'Bus_lon': '-1.9735155'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-019', 'Bus_lat': '52.7269277', 'Region': 'https://dbpedia.org/page/East_of_England', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-019_East_of_England.owl#EBus-019_East_of_England', 'Bus_lon': '0.1981251'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-020', 'Bus_lat': '52.0716528', 'Region': 'https://dbpedia.org/page/East_of_England', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-020_East_of_England.owl#EBus-020_East_of_England', 'Bus_lon': '1.0631638'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-021', 'Bus_lat': '51.9351319', 'Region': 'https://dbpedia.org/page/East_of_England', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-021_East_of_England.owl#EBus-021_East_of_England', 'Bus_lon': '0.1167908'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-022', 'Bus_lat': '51.9270632', 'Region': 'https://dbpedia.org/page/South_East_England', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-022_South_East_England.owl#EBus-022_South_East_England', 'Bus_lon': '-0.9099366'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-023', 'Bus_lat': '51.3749726', 'Region': 'https://dbpedia.org/page/South_West_England', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-023_South_West_England.owl#EBus-023_South_West_England', 'Bus_lon': '-2.1441581'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-024', 'Bus_lat': '51.3358918', 'Region': 'https://dbpedia.org/page/South_East_England', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-024_South_East_England.owl#EBus-024_South_East_England', 'Bus_lon': '-1.0775578'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-025', 'Bus_lat': '51.5077431', 'Region': 'https://dbpedia.org/page/London', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-025_London.owl#EBus-025_London', 'Bus_lon': '-0.1271547'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-026', 'Bus_lat': '51.3684603', 'Region': 'https://dbpedia.org/page/South_East_England', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-026_South_East_England.owl#EBus-026_South_East_England', 'Bus_lon': '0.7414151'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-027', 'Bus_lat': '51.1050295', 'Region': 'https://dbpedia.org/page/South_East_England', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-027_South_East_England.owl#EBus-027_South_East_England', 'Bus_lon': '0.9761146'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-028', 'Bus_lat': '50.9163709', 'Region': 'https://dbpedia.org/page/South_East_England', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-028_South_East_England.owl#EBus-028_South_East_England', 'Bus_lon': '-1.0383188'}, {'Bus_node': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid_topology/29_bus_model.owl#EquipmentConnection_EBus-029', 'Bus_lat': '50.7674626', 'Region': 'https://dbpedia.org/page/South_West_England', 'EBus': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_grid/29_bus_model/Model_EBus-029_South_West_England.owl#EBus-029_South_West_England', 'Bus_lon': '-3.4061633'}]
    test_pp = [{'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Eveley.owl#PowerGenerator_Eveley', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'Region': 'https://dbpedia.org/page/South_East_England', 'lon': '-1.5348087', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'lat': '51.1074573'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Bann_Road.owl#PowerGenerator_Bann_Road', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'Region': 'https://dbpedia.org/page/Northern_Ireland', 'lon': '-6.5039674', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'lat': '54.9635344'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Crundale.owl#PowerGenerator_Crundale', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'Region': 'https://dbpedia.org/page/Wales', 'lon': '-4.921855', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'lat': '51.824051'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Coltishall_1.owl#PowerGenerator_Coltishall_1', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'Region': 'https://dbpedia.org/page/East_of_England', 'lon': '1.3619712', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'lat': '52.7296013'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Ermine_Street.owl#PowerGenerator_Ermine_Street', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'Region': 'https://dbpedia.org/page/East_Midlands', 'lon': '-0.5235887', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'lat': '53.0153552'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Broxted.owl#PowerGenerator_Broxted', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'Region': 'https://dbpedia.org/page/East_of_England', 'lon': '0.5162253', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'lat': '52.1328103'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Lerwick.owl#PowerGenerator_Lerwick', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Oil', 'Region': 'https://dbpedia.org/page/Scotland', 'lon': '-1.1669053', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#OpenCycleGasTurbine', 'lat': '60.1669668'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Melbourn_Muncey_Farm.owl#PowerGenerator_Melbourn_Muncey_Farm', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'Region': 'https://dbpedia.org/page/East_of_England', 'lon': '-0.0186262', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'lat': '52.0751864'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Exning.owl#PowerGenerator_Exning', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'Region': 'https://dbpedia.org/page/East_of_England', 'lon': '0.3888921', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'lat': '52.2796967'}, {'PowerGenerator': 'http://www.theworldavatar.com/kb/UK_Digital_Twin/UK_power_plant/operationalPowerPlantBy2019/Waterloo_Farm_1_2.owl#PowerGenerator_Waterloo_Farm_1_2', 'PrimaryFuel': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'Region': 'https://dbpedia.org/page/East_of_England', 'lon': '-0.2635285', 'GenerationTechnology': 'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar', 'lat': '52.1514863'}]
    location = [[102.0, 2.0], [103.0, 2.0], [103.0, 3.0], [102.0, 3.0], [102.0, 2.0]]
    # centroid = centroidOfMultiplePoints(location)
    # print(centroid)
    ONS_json = "http://statistics.data.gov.uk/sparql.json"
    ukdigitaltwinendpoint = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin/sparql"
    res_ec = queryElectricityConsumption_LocalArea("2017-01-31", ukdigitaltwinendpoint, ONS_json)
    cl = demandLoadAllocator()
    res =cl.closestDemandLoad(res_29_bus, None, res_ec)
    print(res)
    