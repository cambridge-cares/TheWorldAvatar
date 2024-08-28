################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

from tqdm import tqdm
import time
import pickle
import numpy as np
import pandas as pd
import uuid
from datetime import datetime
import netCDF4 as nc
from shapely.geometry import Point, MultiPoint
from shapely.ops import nearest_points
from shapely import wkt

import agentlogging
from agent.kgutils.kgclient import KGClient
from agent.kgutils.querytemplates import *
from agent.errorhandling.exceptions import *
from agent.utils.env_configs import YEAR
from agent.utils.stack_configs import (QUERY_ENDPOINT, UPDATE_ENDPOINT)

# Initialise logger
logger = agentlogging.get_logger("prod")  

# ------------------------- shortcut functions ----------------------------------- #
def remove_nan_to_NAN(*array_group):
  '''
  Remove all nan value in an array, change them into "'NaN'^^<{XSD_STRING}>" as per 
  XSD Schema specification
  '''
  return_array_group = []
  for array in array_group:
    return_array = [f"'NaN'^^<{XSD_STRING}>" if np.isnan(array) else array for array in array]
    return_array_group.append(return_array)
  
  return return_array_group

def get_key(val, my_dict):
    for key, value in my_dict.items():
        if val == value:
            return key
    return None

def read_from_pickle(pathname: str):
    '''
    This function is to read the local pickle file to get the ONS geographic data

    Argument:
    pathname: the path of the pickle file, default as .\data\shape_array
    '''
    try: 
          infile = open(pathname,'rb')
    except Exception as ex:
      print(ex)
      logger.error(f"can not find the pickle file, check {pathname} if which is there")
      raise InvalidInput(f'can not find the pickle file, check {pathname} if which is there') from ex
    
    results = pickle.load(infile)
    infile.close()
    logger.info('ONS geographic data successfully retrieved from pickle file')
    return results

# ------------------------- Retrieve data from local path------------------------------------ #
def retrieve_elec_data(year: str = YEAR):
    '''
        Read the data from the local xlsx file
        
        Arguments:
        year: the number of year of which the data you may want to read
    '''
# Retrieve reading from web
    logger.info('Retrieving Electricity consumption data from Excel ...')
    try:
        data = pd.read_excel(
            f'./data/LSOA_domestic_elec_2010-{year[2:3]}.xlsx',
            sheet_name=year,
            skiprows=4,
            skipfooter=1
        )
        
    except Exception as ex:
        print(ex)
        logger.error(f"Excel file fail to be found -- electricity consumption")
        raise InvalidInput(f'Excel file fail to be found, please check if the electricity consumption xlsx has been placed in ./data folder') from ex
    
    return data

def retrieve_gas_data(year: str = YEAR):
    '''
        Read the data from the local xlsx file
        
        Arguments:
        year: the number of year of which the data you may want to read
    '''
# Retrieve reading from web
    logger.info('Retrieving Gas consumption data from Excel ...')
    try:
        data = pd.read_excel(
            f'./data/LSOA_domestic_gas_2010-{year[2:3]}.xlsx',
            sheet_name=YEAR,
            skiprows=4,
            skipfooter=1
        )
        
    except Exception as ex:
        print(ex)
        logger.error(f"Excel file fail to be found -- gas consumption")
        raise InvalidInput(f'Excel file fail to be found, please check if the gas consumption xlsx has been placed in ./data folder') from ex
    
    return data

def retrieve_geo_data():
    '''
    Read the data from the local shape file
    '''
    logger.info('Retrieving ONS geographic data from local pickle file ...')
    try:
        LSOA_codes, wkt_codes = read_from_pickle('./data/shapes_array')
    except Exception as ex:
        print(ex)
        logger.error(f"shape pickle file fail to be found -- electricity consumption")
        raise InvalidInput(f'shape pickle file fail to be found, please check if the shape pickle file has been placed in ./data folder') from ex
    
    return LSOA_codes, wkt_codes

def retrieve_fuel_poverty_data(year: str = YEAR):
    '''
        Read the data from the local xlsx file
        
        Arguments:
        year: the number of year of which the data you may want to read
    '''
# Retrieve reading from web
    logger.info('Retrieving fuel poverty data from Excel ...')
    try:
        year_published = str(int(year) + 2)
        data = pd.read_excel(
            f'./data/sub-regional-fuel-poverty-{year_published}-tables.xlsx',
            sheet_name="Table 3",
            skiprows=2,
            skipfooter=8
        )
    except Exception as ex:
        print(ex)
        logger.error(f"Excel file fail to be found -- fuel poverty")
        raise InvalidInput(f'Excel file fail to be found, please check if the fuel poverty xlsx has been placed in ./data folder') from ex
    
    return data

def read_nc(var_name, year, loc=True):
    '''
    Given a variable in the specified year of HadUK Grid dataset, reads the file
    and returns the grid of observations.
    '''
    try:
        fn = f'./data/{var_name}_hadukgrid_uk_1km_mon_{year}01-{year}12.nc'
    except Exception as ex:
        print(ex)
        logger.error(f"nc file fail to be found -- climate temperature")
        raise InvalidInput(f'nc file fail to be found, please check if the climate temperature data has been placed in ./data folder') from ex

    ds = nc.Dataset(fn)
    var_grid = ds.variables[var_name][:]
    logger.info(f'{year} {var_name}: hadUK climate data from {fn} succesfully retrieved and calculated')
    if loc == True:
        lon = ds.variables['longitude'][:]
        lat = ds.variables['latitude'][:]
        ds.close()
        return lon, lat, var_grid
    else:
        ds.close()
        return var_grid

# ------------------------- Upload data to KG ------------------------------------ #
def upload_elec_data_to_KG (
                data: pd.DataFrame,
                year: str = YEAR,
                query_endpoint: str = QUERY_ENDPOINT,
                update_endpoint: str = UPDATE_ENDPOINT,
):
    '''
        perform SPARQL update to upload the Electricity consumption/meters data into Blazegraph
        
        Arguments:
        data: The dataframe retrieved from the Excel 
        year: the number of year of which the data you may want to read
        query_endpoint: str = QUERY_ENDPOINT,
        update_endpoint: str = UPDATE_ENDPOINT
    '''

    LSOA_codes = data["LSOA code"].values
    met_num = data["Number\nof meters\n"].values
    consump = data["Total \nconsumption\n(kWh)"].values

# Replace the 'null' data to 'NaN'
    met_num, consump = remove_nan_to_NAN(met_num, consump)

# Define the reference time and Convert into correct format
    start_time = datetime.strptime(year + "-01-01T12:00:00.000Z", '%Y-%m-%dT%H:%M:%S.000Z')
    end_time = datetime.strptime(year + "-12-31T12:00:00.000Z", '%Y-%m-%dT%H:%M:%S.000Z')

# Split the queries into Batches
# Perform SPARQL update query in chunks to avoid heap size/memory issues
    total = len(LSOA_codes)
    n_compile = total / 10
    remainder = total % 10
    n_compile = int(n_compile)
    len_query = np.zeros(n_compile + 2)
    if remainder == 0:
        len_query = np.zeros(n_compile + 1)

    logger.info('Create triples to instantiate Electrical Consumption data ...')
    for i in range(1, len(len_query) - 1):
        len_query[i] = len_query[i - 1] + 10
        len_query[-1] = len_query[-2] + remainder

    for g in tqdm(range(len(len_query) - 1)):
        i = int(len_query[g])
        region = LSOA_codes[i]
        meters = met_num[i]
        cons = consump[i]

        # Initialise update query
        query = f"INSERT DATA" + "{"
        used_uuid = COMPA + 'hasConsumed_' + str(uuid.uuid4())
        met_uuid = COMPA + 'ElectricityMeter_' + str(uuid.uuid4())
        kw_uuid = COMPA + 'KW_' + str(uuid.uuid4())
        mes_uuid = COMPA + 'Measure_' + str(uuid.uuid4())
        triples = electricity_update_template(
                mes_uuid,
                used_uuid,
                start_time,
                end_time,
                region,
                kw_uuid,
                cons,
                met_uuid,
                meters) 

        middle_num = int(len_query[g + 1] - len_query[g]) - 2
        for j in range(middle_num):
            region = LSOA_codes[i + j + 1]
            meters = met_num[i + j + 1]
            cons = consump[i + j + 1]

            used_uuid = COMPA + 'hasConsumed_' + str(uuid.uuid4())
            met_uuid = COMPA + 'ElectricityMeter_' + str(uuid.uuid4())
            kw_uuid = COMPA + 'KW_' + str(uuid.uuid4())
            mes_uuid = COMPA + 'Measure_' + str(uuid.uuid4())
            triples += electricity_update_template(
                    mes_uuid,
                    used_uuid,
                    start_time,
                    end_time,
                    region,
                    kw_uuid,
                    cons,
                    met_uuid,
                    meters) 

        region = LSOA_codes[int(len_query[g + 1]) - 1]
        meters = met_num[int(len_query[g + 1]) - 1]
        cons = consump[int(len_query[g + 1]) - 1]

        used_uuid = COMPA + 'hasConsumed_' + str(uuid.uuid4())
        met_uuid = COMPA + 'ElectricityMeter_' + str(uuid.uuid4())
        kw_uuid = COMPA + 'KW_' + str(uuid.uuid4())
        mes_uuid = COMPA + 'Measure_' + str(uuid.uuid4())

        triples += electricity_update_template(
                mes_uuid,
                used_uuid,
                start_time,
                end_time,
                region,
                kw_uuid,
                cons,
                met_uuid,
                meters) 
        query +=triples + "}"

        # Instantiate all non-time series triples
        kg_client = KGClient(query_endpoint, update_endpoint)
        kg_client.performUpdate(query)

    #print('Observations/forecasts successfully instantiated/updated.')
    logger.info('Insert query for Electricity Consumption successfully performed.')

    return total

def upload_gas_data_to_KG (
                data: pd.DataFrame,
                year: str = YEAR,
                query_endpoint: str = QUERY_ENDPOINT,
                update_endpoint: str = UPDATE_ENDPOINT):
    '''
        perform SPARQL update to upload the gas consumption/meter/nonmeter data into Blazegraph
        
        Arguments:
        data: The dataframe retrieved from the Excel 
        year: the number of year of which the data you may want to read
        query_endpoint: str = QUERY_ENDPOINT,
        update_endpoint: str = UPDATE_ENDPOINT
    '''

    LSOA_codes = data["LSOA code"].values
    met_num = data["Number\nof meters\n"].values
    consump = data["Total \nconsumption\n(kWh)"].values
    non_met_num = data['Number of\nnon-consuming meters'].values

# Replace the 'null' data to 'NaN'
    met_num, consump, non_met_num = remove_nan_to_NAN(met_num, consump, non_met_num)

# Define the reference time and Convert into correct format
    start_time = datetime.strptime(year + "-01-01T12:00:00.000Z", '%Y-%m-%dT%H:%M:%S.000Z')
    end_time = datetime.strptime(year + "-12-31T12:00:00.000Z", '%Y-%m-%dT%H:%M:%S.000Z')

# Split the queries into Batches
# Perform SPARQL update query in chunks to avoid heap size/memory issues
    total = len(LSOA_codes)
    n_compile = total / 10
    remainder = total % 10
    n_compile = int(n_compile)
    len_query = np.zeros(n_compile + 2)
    if remainder == 0:
        len_query = np.zeros(n_compile + 1)

    logger.info('Create triples to instantiate Electrical Consumption data ...')
    for i in range(1, len(len_query) - 1):
        len_query[i] = len_query[i - 1] + 10
        len_query[-1] = len_query[-2] + remainder
    
    for g in tqdm(range(len(len_query) - 1)):
        i = int(len_query[g])
        region = LSOA_codes[i]
        meters = met_num[i]
        non_meters = non_met_num[i]
        cons = consump[i]

        # Initialise update query
        query = f"INSERT DATA" + "{"
        used_uuid = COMPA + 'hasConsumed_' + str(uuid.uuid4())
        met_uuid = COMPA + 'GasMeter_' + str(uuid.uuid4())
        kw_uuid = COMPA + 'KW_' + str(uuid.uuid4())
        mes_uuid = COMPA + 'Measure_' + str(uuid.uuid4())
        triples = gas_update_template(mes_uuid,used_uuid,start_time,end_time, \
                                      region, kw_uuid, cons, met_uuid, meters, non_meters)

        middle_num = int(len_query[g + 1] - len_query[g]) - 2
        for j in range(middle_num):
            region = LSOA_codes[i + j + 1]
            meters = met_num[i + j + 1]
            cons = consump[i + j + 1]
            non_meters = non_met_num[i + j + 1]

            used_uuid = COMPA + 'hasConsumed_' + str(uuid.uuid4())
            met_uuid = COMPA + 'GasMeter_' + str(uuid.uuid4())
            kw_uuid = COMPA + 'KW_' + str(uuid.uuid4())
            mes_uuid = COMPA + 'Measure_' + str(uuid.uuid4())
            triples += gas_update_template(mes_uuid,used_uuid,start_time,end_time, \
                                                  region, kw_uuid, cons, met_uuid, meters, non_meters)

        region = LSOA_codes[int(len_query[g + 1]) - 1]
        meters = met_num[int(len_query[g + 1]) - 1]
        cons = consump[int(len_query[g + 1]) - 1]
        non_meters = non_met_num[int(len_query[g + 1]) - 1]

        used_uuid = COMPA + 'hasConsumed_' + str(uuid.uuid4())
        met_uuid = COMPA + 'GasMeter_' + str(uuid.uuid4())
        kw_uuid = COMPA + 'KW_' + str(uuid.uuid4())
        mes_uuid = COMPA + 'Measure_' + str(uuid.uuid4())
        triples += gas_update_template(mes_uuid,used_uuid,start_time,end_time, \
                                                  region, kw_uuid, cons, met_uuid, meters, non_meters)
        
        query +=triples + "}"

        # Instantiate all non-time series triples
        kg_client = KGClient(query_endpoint, update_endpoint)
        kg_client.performUpdate(query)

    #print('Observations/forecasts successfully instantiated/updated.')
    logger.info('Insert query for Gas Consumption successfully performed.')

    return total

def upload_Geoinfo_to_KG(
                         LSOA_codes:list,
                         wkt_codes:list,
                         query_endpoint: str = QUERY_ENDPOINT,
                         update_endpoint: str = UPDATE_ENDPOINT):
    '''
        perform SPARQL update to upload the geo data of LSOA regions into Blazegraph
        
        Arguments:
        data: The dataframe retrieved from the Excel 
        query_endpoint: str = QUERY_ENDPOINT,
        update_endpoint: str = UPDATE_ENDPOINT
    '''
    # Split the queries into Batches
    # Perform SPARQL update query in chunks to avoid heap size/memory issues
    total = len(LSOA_codes)
    n_compile = total / 10
    remainder = total % 10
    n_compile = int(n_compile)
    len_query = np.zeros(n_compile+2)
    if remainder == 0:
        len_query = np.zeros(n_compile + 1)

    logger.info('Create triples to instantiate ONS geographical data ...')
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
    logger.info('Insert query for ONS geographic data successfully performed.')
    
    return total

def upload_fuel_poverty_to_KG (
                data:pd.DataFrame,
                year: str = YEAR,
                query_endpoint: str = QUERY_ENDPOINT,
                update_endpoint: str = UPDATE_ENDPOINT):
    '''
        perform SPARQL update to upload the fuel poor number, total household number data into Blazegraph
        
        Arguments:
        year: the number of year of which the data you may want to read
        query_endpoint: str = QUERY_ENDPOINT,
        update_endpoint: str = UPDATE_ENDPOINT
        path: mainly for testing reason, provide an path to a json/xslx file which can be read and upload to knowledge graph
              example json files can be seen in ./tests/data folder
    '''
    
    LSOA_codes = data["LSOA Code"].values
    house_num = data["Number of households"].values
    poor_num = data["Number of households in fuel poverty"].values

    # Replace the 'null' data to 'NaN'
    house_num, poor_num = remove_nan_to_NAN(house_num, poor_num)

    # Define the reference time and Convert into correct format
    start_time = datetime.strptime(year + "-01-01T12:00:00.000Z", '%Y-%m-%dT%H:%M:%S.000Z')
    end_time = datetime.strptime(year + "-12-31T12:00:00.000Z", '%Y-%m-%dT%H:%M:%S.000Z')

# Split the queries into Batches
# Perform SPARQL update query in chunks to avoid heap size/memory issues
    total = len(LSOA_codes)
    n_compile = total / 10
    remainder = total % 10
    n_compile = int(n_compile)
    len_query = np.zeros(n_compile + 2)
    if remainder == 0:
        len_query = np.zeros(n_compile + 1)

    logger.info('Create triples to instantiate Fuel poverty data ...')
    for i in range(1, len(len_query) - 1):
        len_query[i] = len_query[i - 1] + 10
        len_query[-1] = len_query[-2] + remainder

    for g in tqdm(range(len(len_query) - 1)):
        i = int(len_query[g])
        region = LSOA_codes[i]
        houses = house_num[i]
        poor = poor_num[i]

        # Initialise update query
        query = f"INSERT DATA" + "{"
        house_uuid = OFPT + 'Household_' + str(uuid.uuid4())
        triples = fuel_poor_update_template(region,house_uuid,start_time,end_time,houses,poor)
        
        middle_num = int(len_query[g + 1] - len_query[g]) - 2
        for j in range(middle_num):
            region = LSOA_codes[i + j + 1]
            houses = house_num[i + j + 1]
            poor = poor_num[i + j + 1]

            house_uuid = OFPT + 'Household_' + str(uuid.uuid4())
            triples += fuel_poor_update_template(region,house_uuid,start_time,end_time,houses,poor)
        
        region = LSOA_codes[int(len_query[g + 1]) - 1]
        houses = house_num[int(len_query[g + 1]) - 1]
        poor = poor_num[int(len_query[g + 1]) - 1]

        house_uuid = OFPT + 'Household_' + str(uuid.uuid4())
        triples += fuel_poor_update_template(region,house_uuid,start_time,end_time,houses,poor)
        query +=triples + "}"

        # Instantiate all non-time series triples
        kg_client = KGClient(query_endpoint, update_endpoint)
        kg_client.performUpdate(query)
    
    #print('Observations/forecasts successfully instantiated/updated.')
    logger.info('Insert query for Electricity Consumption successfully performed.')

    return total

def upload_hadUK_climate_to_KG (
                data:pd.DataFrame,
                year: str = YEAR,
                query_endpoint: str = QUERY_ENDPOINT,
                update_endpoint: str = UPDATE_ENDPOINT):
    '''
        perform SPARQL update to upload the Climate data into Blazegraph
        The climate data for each LSOA is calculated based on hadUK grid climate data
        details can be found in CoMo preprints.279
        
        Arguments:
        year: the number of year of which the data you may want to read
        query_endpoint: str = QUERY_ENDPOINT,
        update_endpoint: str = UPDATE_ENDPOINT
        path: mainly for testing reason, provide an path to a nc file which can be read and upload to knowledge graph
              example json files can be seen in ./tests/data folder
    '''
    months_dict = {'January':0,'February':1,'March':2,'April':3,'May':4,'June':5,'July':6,'August':7,'September':8,'October':9,'November':10,'December':11}
    
    def month_num(month_str):
      '''
      Converts a string of month name to that month's number 
      Starting at 0
      '''
      months = {'January':0,'February':1,'March':2,'April':3,'May':4,'June':5,'July':6,'August':7,'September':8,'October':9,'November':10,'December':11}
      month_ends = {'January':31,'February':28,'March':31,'April':30,'May':31,'June':30,'July':30,'August':31,'September':30,'October':29,'November':30,'December':31}
      month = months[month_str]
      month_end = month_ends[month_str]
      return month, month_end

    def gridded_data_to_array(lon,lat,nc_vars,month):
        '''
        Calculate gridded data to array, filter out meaningless point
        for point do have value, extract the position of point, and respective value

        return arrays of points, and the respective temperature [tasmin,tas,tasmax]
        '''
        
        overall_centroids = []
        var_store = np.array([[0 for i in range(len(nc_vars))]])
        for i in range(len(nc_vars)):
            nc_vars[i] = nc_vars[i].filled(np.nan)
        for i in tqdm(range(len(lat[:,0]))):
            for j in range(len(lon[0,:])):
                nc_var_temp = [0 for i in range(len(nc_vars))]
                for k in range(len(nc_vars)):
                    nc_var_temp[k] = nc_vars[k][month,i,j]
                if nc_var_temp[0] > -1e8:
                    point = Point([lon[i,j],lat[i,j]])
                    centroid = point
                    overall_centroids.append(centroid)
                    var_store = np.append(var_store,[nc_var_temp],axis=0)
        var_store = var_store[1:,:]
        logger.info(f'Gridded data for {get_key(month,months_dict)} have been converted to array')
        return overall_centroids, var_store
    
    def get_treated_shape():
      '''
      Perform treatment of geographic data to assist calculation of climate data
      return list containing LSOA code and shape
      '''
      # Get geodata
      LSOA_codes, wkt_codes = read_from_pickle('./data/shapes_array')
      usage_vals = np.column_stack((LSOA_codes, wkt_codes))
      # preassigning centroid array
      centroids = np.zeros((len(usage_vals),1),dtype='object')
      i = 0 
      del_index = [] 
      # iterating over WKT Literals
      for area in usage_vals: 
          # loading WKT
          try:
              wkt_repr = wkt.loads(area[-1])
              usage_vals[i,-1] = wkt_repr
              # calculating and storing centroid
              centroids[i,:] = [wkt_repr.centroid]
              i += 1 
          except TypeError: 
              del_index.append(i)
              usage_vals[i,-1] = 'None'
              centroids[i,:] = ['None']
              i += 1 
      # concatenating results 
      LSOA = np.concatenate((usage_vals,centroids),axis=1)
      LSOA = np.delete(LSOA,del_index,axis=0)
      logger.info('ONS geometry data successfully retrieved and centroids calculated.')

      return LSOA
    
    def from_grid_find_temp(lon,lat,nc_vars,month,overall_centroids):
        '''
        After performed gridded_data_to_array function, where the grid that do have 
        real temperature reading is retrieved as array.
        This function will use such an array to extract the temperature reading using
        the specified points. If one reading is missed which will report
                    '''
        point_indices = {Point(lon[i, j], lat[i, j]): (i, j) for i in range(lon.shape[0]) for j in range(lon.shape[1])}
        nc_vars = [var.filled(np.nan) for var in nc_vars]
        var_store = np.zeros((len(overall_centroids), len(nc_vars)))
        missing_data = []
        for idx, point in enumerate(overall_centroids):
            i, j = point_indices.get(point)
            nc_var_temp = [nc_vars[k][month, i, j] for k in range(len(nc_vars))]
            if nc_var_temp[0] > -1e8:
                var_store[idx] = nc_var_temp
            else:
              # If there is a missing data, assuming it is [9, 14, 17] and report it, for sake of calculation
              # don't distrbute it this module just takes too long to run :(
                var_store[idx] = [9, 14, 17]
                missing_data.append((point, get_key(month, months_dict)))
        if missing_data:
            print(f'CAUTIONS! Climate data for points {missing_data} is missing!')
        logger.info(f'Gridded data for {get_key(month,months_dict)} have been converted to array')
        return var_store
    
    logger.info('Retrieving ONS geometry data for later assosiation ...')
    LSOA = get_treated_shape()

    logger.info('Retrieving hadUK grid temperature from web ...')
    lon,lat,tas = read_nc('tas', year, loc=True)
    tasmin = read_nc('tasmin', year, loc=False)
    tasmax = read_nc('tasmax', year, loc=False)

    # Define some list for searching data
    months = ['January','February','March','April','May','June','July','August','September','October','November','December']
    clim_vars = ['tasmin','tas','tasmax']

    logger.info('Converting gridded data to array ...')
    nc_vars_full = [] 
    grid_loc,nc_vars_add = gridded_data_to_array(lon,lat,[tasmin,tas,tasmax],0)
    nc_vars_full.append(nc_vars_add)
    for i in range(1,len(months)):
        nc_vars_add = from_grid_find_temp(lon,lat,[tasmin,tas,tasmax],i,grid_loc)
        nc_vars_full.append(nc_vars_add)
    
    full_grid = MultiPoint(points=list(grid_loc))
    
    logger.info('Computing associated points for each LSOA area...')
    logger.info('Be patient and get a cuppa tea, this will take long time to run...')
    for i in tqdm(range(0, int(len(LSOA)))):
      query = climate_temperature_query_template(LSOA[i,0])
      kg_client = KGClient(query_endpoint, update_endpoint)
      res = kg_client.performQuery(query)

      if len(res) == 36:
          pass
      else: 
        assoc_mask = [full_grid.geoms[j].within(LSOA[i,1]) for j in range(len(grid_loc))]
        if any(assoc_mask) == False:
            assoc_point = nearest_points(full_grid,LSOA[i,2])[0]
            for j in range(len(grid_loc)):
                if assoc_point == grid_loc[j]:
                    assoc_mask[j] = True 
                    break
        for month_it in range(len(months)):
            LSOA_vars_full = nc_vars_full[month_it][assoc_mask,:]

            '''
            (Comment from the creater of this code Tom Savage)
            Following code is responsible for aggregating mean minimum and maximum temperature values

            LSOA_vars_full contains a matrix with columns of tasmin, tas and tasmax
            and respective rows for every grid point that falls within a region.

            if the variable is tasmin then the minimum of these values is taken, mean for tas and max for tasmax

            [[tasmin ,  tas   , tasmax],
            [tasmin ,  tas   , tasmax],
            [tasmin ,  tas   , tasmax],
            [tasmin ,  tas   , tasmax],
            [tasmin ,  tas   , tasmax],
            [tasmin ,  tas   , tasmax],
            [tasmin ,  tas   , tasmax]]

            [[   |   ,   |    ,   |   ],
            [   |   ,   |    ,   |   ],
            [   |   ,   |    ,   |   ],
            [np.min ,np.mean , np.max],
            [   |   ,   |    ,   |   ],
            [   |   ,   |    ,   |   ],
            [   |   ,   |    ,   |   ]]


            [np.min ,np.mean , np.max]
            (Comment from editor: yea well explained)
            '''
            post_proc_vars = np.zeros(3)
            for k in range(3):
            # Calculate tasmin, tas, tasmax of each LSOA area based on grid temperature
                if clim_vars[k] == 'tasmin':
                    post_proc_vars[k] = np.min(LSOA_vars_full[:,k])
                elif clim_vars[k] == 'tas': 
                    post_proc_vars[k] = np.mean(LSOA_vars_full[:,k])
                else:
                    post_proc_vars[k] = np.max(LSOA_vars_full[:,k])

            LSOA_vars = post_proc_vars
            LSOA_IRI = LSOA[i,0]
            month_str = str(month_it+1)
            month_str_len = len(month_str)
            _, month_end = month_num(months[month_it])
            if month_str_len == 1:
                month_str = '0'+month_str

            LSOA_code = LSOA_IRI.split('/')[-1]
            startUTC = datetime.strptime('2020-'+month_str+'-01T12:00:00.000Z', '%Y-%m-%dT%H:%M:%S.000Z')
            endUTC = datetime.strptime('2020-'+month_str+'-'+str(month_end)+'T12:00:00.000Z', '%Y-%m-%dT%H:%M:%S.000Z')

            # Initialise update query
            query = f"INSERT DATA" + "{"
            for var in range(len(LSOA_vars)):
                meas_uuid = CLIMA + 'ClimateMeasurement_' + str(uuid.uuid4())
                clim_var = CLIMA + str(clim_vars[var])
                temp_uuid = CLIMA + 'Temperature_' + str(uuid.uuid4())
                val_uuid = CLIMA + 'Value_' + str(uuid.uuid4())
                query += climate_temperature_update_template(LSOA_code,meas_uuid,clim_var,startUTC,endUTC,temp_uuid,val_uuid,LSOA_vars[var])
            query += "}"

            # Instantiate all non-time series triples
            kg_client = KGClient(query_endpoint, update_endpoint)
            kg_client.performUpdate(query)

    logger.info('Insert query for hadUK climate data successfully performed')
    return int(len(LSOA))

def upload_all(year: str = YEAR, query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT):
    
    # instantiate the Electricity consumption data
    print("\nUploading the Electricity consumption data:")
    logger.info("Uploading the Electricity consumption data...")
    t1 = time.time()
    data = retrieve_elec_data(year)
    num_elec = upload_elec_data_to_KG(data, year, query_endpoint, update_endpoint)
    print(f"Number of LOSA output area with instantiated Electricity consumption/meter data: {num_elec}")
    t2= time.time()
    diff = t2 - t1
    print(f'Electricity consumption - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    logger.info(f'Electricity consumption - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    
    # instantiate the Gas consumption data
    print("\nUploading the Gas consumption data:")
    logger.info("Uploading the Gas consumption data...")
    t1 = time.time()
    data = retrieve_gas_data(year)
    num_gas = upload_gas_data_to_KG(data, year, query_endpoint, update_endpoint)
    print(f"Number of LOSA output area with instantiated Gas consumption data: {num_elec}")
    t2= time.time()
    diff = t2 - t1
    print(f'Gas consumption data - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    logger.info(f'Gas consumption data - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')

    # instantiate the ONS geographic data
    print("\nUploading the ONS geographic data:")
    logger.info("Uploading the ONS geographic data...")
    t1 = time.time()
    LSOA_codes, wkt_codes = retrieve_geo_data()
    num_shape = upload_Geoinfo_to_KG(LSOA_codes, wkt_codes, query_endpoint, update_endpoint)
    print(f"Number of LOSA output area with instantiated ONS geographic data: {num_elec}")
    t2= time.time()
    diff = t2 - t1
    print(f'ONS geographic data - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    logger.info(f'ONS geographic data - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    
    # instantiate the Fuel poverty data
    print("\nUploading the Fuel poverty data:")
    logger.info("Uploading the Fuel poverty data...")
    t1 = time.time()
    data = retrieve_fuel_poverty_data(year)
    num_fuelpoor = upload_fuel_poverty_to_KG(data, year, query_endpoint, update_endpoint)
    print(f"Number of LOSA output area with instantiated Fuel poverty data: {num_elec}")
    t2= time.time()
    diff = t2 - t1
    print(f'Fuel poverty - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    logger.info(f'Fuel poverty - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    
    # instantiate the hadUK climate data
    print("\nUploading the hadUK climate data:")
    logger.info("Uploading the hadUK climate data...")
    t1 = time.time()
    num_temp = upload_hadUK_climate_to_KG(year, query_endpoint, update_endpoint)
    print(f"Number of LOSA output area with instantiated hadUK climate data: {num_elec}")
    t2= time.time()
    diff = t2 - t1
    print(f'hadUK climate data - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    logger.info(f'hadUK climate data - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')

    return num_elec, num_gas, num_shape, num_fuelpoor, num_temp

if __name__ == '__main__':
# years = ['2018','2017','2016','2015']
# for year in years:
#         upload_year(year)
    num_elec, num_gas, num_shape, num_fuelpoor, num_temp = upload_all()
    print(f'Number of LSOA area with instantiated Electricity consumption/meters :{num_elec}')
    print(f'Number of LSOA area with instantiated Gas consumption/meters/nonmeters :{num_gas}')
    print(f'Number of LSOA area with instantiated Shape data :{num_shape}')
    print(f'Number of LSOA area with instantiated Fuel Poverty :{num_fuelpoor}')
    print(f'Number of LSOA area with instantiated hadUK climate data :{num_temp}')
