################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to read the ONS area geometry data, 
# and instantiate the data into the Knowledge graph
# and convert it into geoJSON format for postGIS

import pickle
import numpy as np
from tqdm import tqdm
import time
import json
import pandas as pd
from geomet import wkt
import geopandas as gpd
import shapely.wkt

from agent.kgutils.querytemplates import *
import agentlogging
from agent.kgutils.kgclient import KGClient
from agent.datamodel.spec import *

logger = agentlogging.get_logger("prod")

def read_from_pickle(pathname):
    infile = open(pathname,'rb')
    results = pickle.load(infile)
    infile.close()

    # Initialisation 
    LSOA_codes=np.zeros(len(results),dtype=object)
    wkt_codes=np.zeros(len(results),dtype=object)

    for i in range(len(results)):
        LSOA_codes[i] = results[i,0].replace('http://statistics.data.gov.uk/id/statistical-geography/','') 
        wkt_codes[i] = results[i,1]

    return LSOA_codes, wkt_codes

def convert_to_geojson():
    
    my_geo_df = retrieve_geodata_from_KG(QUERY_ENDPOINT,UPDATE_ENDPOINT)
    
    print('Projecting to WGS84')
    my_geo_df = my_geo_df.to_crs("EPSG:4326")
    print('Parsing shapes as geoJSON...')
    my_geo_df['geojson'] =  my_geo_df['geom_str'].apply(lambda x: json.dumps(wkt.loads(x)))
    polygons = list(my_geo_df['geojson'])
    
    start = """{"type": "FeatureCollection","features": ["""
    
    for i in tqdm(range(len(polygons))):
        property_dict = {}
        polygons[i] = json.loads(polygons[i])
        polygons[i] = {"geometry":polygons[i]}
        if i != len(polygons)-1:
            start += str(polygons[i])+','
        else:
            start += str(polygons[i])
    
    end = ''']}'''
    start += end 
    start = str(start).replace("'", '"')
    start = start.replace(' ','')
    start = start.replace('/n','')

    print(start)

def retrieve_geodata_from_KG(query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT):
    '''
        perform SPARQL query to get the data from Blazegraph, return a DataFrame looks like:
              's'  'geom'  'geometry'
        0
        1
        2
        ...

        Arguments:
        year: the number of year of which the data you may want to read
        query_endpoint: str = QUERY_ENDPOINT,
        update_endpoint: str = UPDATE_ENDPOINT
    '''

    '''
    # Get query string
    query = output_query_template('ONS output area',iris = False)
    
    # Construct kg client
    kg_client = KGClient(query_endpoint, update_endpoint)
    result = kg_client.performQuery(query)
    
    # Parse the result into DataFrame
    df = pd.DataFrame(columns=['s','geom'])
    df = df.append(result)

    '''
###############   TBD    #############################
    infile = open('./Data/shapes_array','rb')
    result = pickle.load(infile)
    infile.close()
    df = pd.DataFrame(columns=['s','geom'])
    for i in range(len(result)):
        df.loc[i,'s']=result[i,0]
        df.loc[i,'geom']=result[i,1]
######################################################

    # check if WKT is valid and 
    # uploading polygons to Shapely to reduce precision to 5 DP (1m)
    for i in range(len(result[:,1])):
        shape = result[i,1]
        try:
            P = shapely.wkt.loads(shape)
            result[i,1] = shapely.wkt.dumps(P,rounding_precision=5)
        # if shape is invalid do chuff all 
        except TypeError:
            pass
        # if the shape is just a number (basically meaningless)
        # add to index of shapes to be deleted
        if type(result[i,1]) == int:
            del_ind = i 

    # get rid of invalid shapes
    result = np.delete(result,del_ind,axis=0)

    # specifying geodata frame
    df['geometry'] = gpd.GeoSeries.from_wkt(df['geom'])

    my_geo_df = gpd.GeoDataFrame(df, geometry='geometry')
    my_geo_df = my_geo_df.set_crs("EPSG:4326")
    print('Converting to Mercator projection (better than WGS84 for UK)')
    my_geo_df = my_geo_df.to_crs("EPSG:3395")
    print(my_geo_df)
    
    return my_geo_df

def upload_Geoinfo_to_KG(query_endpoint: str = QUERY_ENDPOINT,
                         update_endpoint: str = UPDATE_ENDPOINT):
    '''
        perform SPARQL update to upload the geo data of LSOA regions into Blazegraph
        
        Arguments:
        query_endpoint: str = QUERY_ENDPOINT,
        update_endpoint: str = UPDATE_ENDPOINT
    '''
    # Initialisation 
    LSOA_codes=[]
    wkt_codes=[]

    # Read the pickle and extract relavent data
    shapes_filename = './Data/shapes_array'
    LSOA_codes, wkt_codes = read_from_pickle(shapes_filename)

    # Split the queries into Batches
    # Perform SPARQL update query in chunks to avoid heap size/memory issues
    total = len(LSOA_codes)
    n_compile = total / 10
    remainder = total % 10
    n_compile = int(n_compile)
    len_query = np.zeros(n_compile+2)

    for i in range(1,len(len_query)-1):
        len_query[i] = len_query[i-1] + 10
        len_query[-1] = len_query[-2] + remainder

    for g in tqdm(range(len(len_query)-1)):
        i = int(len_query[g])
        region = LSOA_codes[i]
        wkt = wkt_codes[i]

        # Initialise update query
        query = f"INSERT DATA" + "{"
        triples = region_update_template(region,wkt)

        middle_num = int(len_query[g+1]-len_query[g])-2
        for j in range(middle_num):
                region = LSOA_codes[i+j+1]
                wkt=wkt_codes[i+j+1]
                triples += region_update_template(region,wkt)

        region = LSOA_codes[int(len_query[g+1])-1]
        wkt=wkt_codes[int(len_query[g+1])-1]
        triples += region_update_template(region,wkt)

        query +=triples + "}"

                # Instantiate all non-time series triples
        kg_client = KGClient(query_endpoint, update_endpoint)
        kg_client.performUpdate(query)

    #print('Observations/forecasts successfully instantiated/updated.')
    logger.info('Insert query for Electricity Consumption successfully performed.')
    return len(len_query) - 1

if __name__ == '__main__':
        
    '''
    print("\nUploading the WKT format geodata for each LSOA area")
    logger.info("Uploading the WKT format geodata for each LSOA area")

    t1= time.time()
    len_query=upload_Geoinfo_to_KG(QUERY_ENDPOINT,UPDATE_ENDPOINT)
    print(f"Number of WKT format geodata instantiated per LOSA output area :{len_query}")
    t2= time.time()
    diff = t2 - t1
    print(f'WKT format geodata - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    logger.info(f'WKT format geodata - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
'''
    retrieve_geodata_from_KG(QUERY_ENDPOINT,UPDATE_ENDPOINT)
    
    #convert_to_geojson()