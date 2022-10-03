##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 3 Oct 2022           #
##########################################

import sys, os
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, BASE)
from UK_Digital_Twin_Package import EndPointConfigAndBlazegraphRepoLabel as endpointList
import UK_Power_Grid_Model_Generator.SPARQLQueryUsedInModel as query_model
from UK_Digital_Twin_Package.demandLoadAllocator import centroidOfMultipolygon
from UK_Power_Grid_Model_Generator.SPARQLQueryUsedInModel import queryElectricityConsumption_LocalArea

def demandingAreaCentriodFinder(queryEndpoint_iri, startTime_of_EnergyConsumption,storedPath):
    ons_iri = endpointList.ONS['queryendpoint_iri']
    # query the local consumption
    res_queryElectricityConsumption_LocalArea = list(query_model.queryElectricityConsumption_LocalArea(startTime_of_EnergyConsumption, queryEndpoint_iri, ons_iri))
    for ec in res_queryElectricityConsumption_LocalArea:
        if ec['Geo_InfoList'].geom_type == 'MultiPolygon':
            ec['Geo_InfoList'] = centroidOfMultipolygon(ec['Geo_InfoList']) 
        elif ec['Geo_InfoList'].geom_type == 'Polygon':
            lon = ec['Geo_InfoList'].centroid.x
            lat = ec['Geo_InfoList'].centroid.y
            ec['Geo_InfoList'] = [lat, lon] 
    return res_queryElectricityConsumption_LocalArea


if __name__ == '__main__':
    ukdigitaltwinendpoint = "http://kg.cmclinnovations.com:81/blazegraph_geo/namespace/ukdigitaltwin_test2/sparql"
    res = demandingAreaCentriodFinder(ukdigitaltwinendpoint, '2017-01-31', 'demandingAndCentroid.py')
    print(len(res))
    r1 = [res[0:50], res[50:100], res[100:150], res[150:200], res[200:250], res[250:300], res[300:350], res[350:]]
    for r in r1:
        print(r)
