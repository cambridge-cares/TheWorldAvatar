################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to read the Electricity consumption data, 
# and instantiate both the data along with timeseries
# into the Knowledge graph

from tqdm import tqdm
import time
import numpy as np
import pandas as pd
import uuid
import datetime
import netCDF4 as nc
from shapely.geometry import Point, MultiPoint
from shapely.ops import nearest_points
from shapely import wkt

import agentlogging
from agent.kgutils.kgclient import KGClient
from agent.kgutils.querytemplates import *
from agent.utils.readings_mapping import DATACLASS, TIME_FORMAT
from agent.errorhandling.exceptions import *
from agent.datamodel.spec import *

#from agent.kgutils.tsclient import jpsBaseLibView
from agent.kgutils.tsclient import TSClient

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

# ------------------------- Read data from source -------------------------------- #
def read_from_web_elec (year: str):
  '''
  This function is to read the up-to-date subnational electricity consumption data from web
  return a dataframe to represent the file been read
  NOTE: This function can work based on the assumption that the web address and page structure  won't change,
  if the xlsx file can't be correctly retrieved it is possible that the web infrastructure has been
  amended.

  Arguments:
        year: the number of year of which the data you may want to read
  '''
  url = 'https://www.gov.uk/government/statistics/lower-and-middle-super-output-areas-electricity-consumption'

  # Use requests to get the HTML of the website
  response = requests.get(url)
  soup = BeautifulSoup(response.text, 'html.parser')

  # Find the link to the xlsx file on the website
  download_div = soup.find_all('div', {'class': 'attachment-thumb'})
  link = download_div[4].find('a', href=True)['href']

  # Download the xlsx file
  try:
    file_name = os.path.basename(link)
    response = requests.get(link)
    open(file_name, 'wb').write(response.content)
  except Exception as ex:
    logger.error(f"Excel file fail to be downloaded")
    raise InvalidInput(f'Excel file fail to be downloaded, please check if {url} is a valid address and webpage') from ex
  
  logger.info(f'xlsx file {file_name} have been downloaded at the current folder')
  # Check if the file is valid
  if not 'LSOA' in file_name:
    if 'not' in file_name:
      logger.error(f"Invalid file downloaded -- check the current folder and source:{url} ")
      raise InvalidInput(f'The file downloaded is not valid, check the webpage:{url} or download mannually.')

  # Parse the data from the xlsx file into a pandas DataFrame
  try: 
    df = pd.read_excel(file_name, sheet_name = year, engine='openpyxl',skiprows=4, skipfooter=1)
    logger.info('Electricity consumption/meter data successfully retrieved from web')
  
  except Exception as ex:
    logger.error(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
please check the {file_name} located on the current folder, see if the 'year' sheet exist")
    raise InvalidInput(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
please check the {file_name} located on the current folder, see if the 'year' sheet exist") from ex
  
  return df

def read_from_web_gas (year: str):
  '''
  This function is to read the up-to-date subnational gas consumption data from web
  return a dataframe to represent the file been read
  NOTE: This function can work based on the assumption that the web address and page structure  won't change,
  if the xlsx file can't be correctly retrieved it is possible that the web infrastructure has been
  amended.

  Arguments:
        year: the number of year of which the data you may want to read
  '''
  url = 'https://www.gov.uk/government/statistics/lower-and-middle-super-output-areas-gas-consumption'

  # Use requests to get the HTML of the website
  response = requests.get(url)
  soup = BeautifulSoup(response.text, 'html.parser')

  # Find the link to the xlsx file on the website
  download_div = soup.find_all('div', {'class': 'attachment-thumb'})
  link = download_div[4].find('a', href=True)['href']

  # Download the xlsx file
  try:
    file_name = os.path.basename(link)
    response = requests.get(link)
    open(file_name, 'wb').write(response.content)
  except Exception as ex:
    logger.error(f"Excel file fail to be downloaded")
    raise InvalidInput(f'Excel file fail to be downloaded, please check if {url} is a valid address and webpage') from ex

  logger.info(f'xlsx file {file_name} have been downloaded at the current folder')
  # Check if the file is valid
  if not 'LSOA' in file_name:
    if 'not' in file_name:
      logger.error(f"Invalid file downloaded -- check the current folder and source:{url} ")
      raise InvalidInput(f'The file downloaded is not valid, check the webpage:{url} or download mannually.')
  
  try:
    # Parse the data from the xlsx file into a pandas DataFrame
    df = pd.read_excel(file_name, sheet_name = year, engine='openpyxl',skiprows=4, skipfooter=1)
    logger.info('Gas consumption/meter data successfully retrieved from web')
  
  except Exception as ex:
    logger.error(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
please check the {file_name} located on the current folder, see if the 'year' sheet exist")
    raise InvalidInput(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
please check the {file_name} located on the current folder, see if the 'year' sheet exist") from ex
  
  return df

def read_from_web_fuel_poverty (year: str):
  '''
  This function is to read the up-to-date subnational fuel poverty data from web
  return a dataframe to represent the file been read
  NOTE: This function can work based on the assumption that the web address and page structure  won't change,
  if the xlsx file can't be correctly retrieved it is possible that the web infrastructure has been
  amended.

  Arguments:
        year: the number of year of which the data you may want to read
  '''
  # There is a lag between data published time and data time, e.g. the data for 2020 is published on 2022. 
  # And the published date is always 2 years after the data date
  year_published = str(int(year) + 2)
  
  url = 'https://www.gov.uk/government/statistics/sub-regional-fuel-poverty-data-' + year_published

  # Use requests to get the HTML of the website
  response = requests.get(url)
  soup = BeautifulSoup(response.text, 'html.parser')

  # Find the link to the xlsx file on the website
  download_div = soup.find_all('div', {'class': 'attachment-thumb'})
  link = download_div[0].find('a', href=True)['href']

  # Download the xlsx file
  try:
    file_name = os.path.basename(link)
    response = requests.get(link)
    open(file_name, 'wb').write(response.content)
  except Exception as ex:
    logger.error(f"Excel file fail to be downloaded")
    raise InvalidInput(f'Excel file fail to be downloaded, please check if {url} is a valid address and webpage') from ex
  
  logger.info(f'xlsx file {file_name} have been downloaded at the current folder')
  # Check if the file is valid
  if not 'sub-regional' in file_name:
    if not year_published in file_name:
      logger.error(f"Invalid file downloaded -- check the current folder and source:{url} ")
      raise InvalidInput(f'The file downloaded is not valid, check the webpage:{url} or download mannually.')

  try:
    # Parse the data from the xlsx file into a pandas DataFrame
    df = pd.read_excel(file_name, sheet_name="Table 3", skiprows=2, skipfooter=8)
    logger.info('Fuel poverty data successfully retrieved from web')
  
  except Exception as ex:
    logger.error(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
please check the {file_name} located on the current folder, see if the 'Table 3' sheet contains desireable datasets")
    raise InvalidInput(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
please check the {file_name} located on the current folder, see if the 'Table 3' sheet contains desireable datasets") from ex
  
  return df

def read_from_web_temp (year: str, var_name: str):
  '''
    This function is to read the up-to-date hadUK grid (1km) climate data from web
    doneload nc file only, for data process to be done later
    NOTE: This function can work based on the assumption that the web address and page structure  won't change,
    if the nc file can't be correctly retrieved it is possible that the web infrastructure has been
    amended.

    Arguments:
          year: the number of year of which the data you may want to read
          var_name: 'tas'/'tasmax'/'tasmin' Select from those three to download file represent mean, max, min temperature, respectively.
    '''

  url = f'https://data.ceda.ac.uk/badc/ukmo-hadobs/data/insitu/MOHC/HadOBS/HadUK-Grid/v1.1.0.0/1km/{var_name}/mon/latest'

  # Use requests to get the HTML of the website
  response = requests.get(url)
  soup = BeautifulSoup(response.text, 'html.parser')

  # Find the link to the nc file on the website
  # Find table
  download_div = soup.find_all('table', {'class': 'table table-sm'})
  # Select link based on 'a', href
  inner_url = download_div[0].find_all('a', href=True)
  
  # Select link contain year of interest
  for year_link in inner_url:
        if year in year_link['href']:
          # Select link that could done load the file
          if len(year_link.attrs) == 1:
              link = year_link['href']

  try: 
      # Download the xlsx file
      file_name = os.path.basename(link).split('?')[0]
  except Exception as ex:
      print(f'The hadUK climate data for {year} can not be found, please check the webpage: {url} to see if that year of data file exist')
      raise InvalidInput(f'The hadUK climate data for {year} can not be found, please check the webpage:{url} to see if that year of data file exist') from ex
  response = requests.get(link)

  open(file_name, 'wb').write(response.content)
  logger.info(f'nc file {file_name} have been downloaded at the current folder')
  
  return file_name

def read_from_pickle(pathname: str):
    '''
    This function is to read the local pickle file to get the ONS geographic data

    Argument:
    pathname: the path of the pickle file, default as .\Data\shape_array
    '''
    try: 
          infile = open(pathname,'rb')
    except Exception as ex:
      logger.error(f"can not find the pickle file, check {pathname} if which is there")
      raise InvalidInput(f'can not find the pickle file, check {pathname} if which is there') from ex
    
    results = pickle.load(infile)
    infile.close()
    logger.info('ONS geographic data successfully retrieved from pickle file')
    return results

# ------------------------- Upload data to KG ------------------------------------ #
def upload_elec_data_to_KG (year:str = '2020',
                query_endpoint: str = QUERY_ENDPOINT,
                update_endpoint: str = UPDATE_ENDPOINT):
    '''
        perform SPARQL update to upload the Electricity consumption/meters data into Blazegraph
        
        Arguments:
        year: the number of year of which the data you may want to read
        query_endpoint: str = QUERY_ENDPOINT,
        update_endpoint: str = UPDATE_ENDPOINT
    '''
# Retrieve reading from web
    logger.info('Retrieving Electricity consumption data from Excel ...')
    data = read_from_web_elec(year)
    LSOA_codes = data["LSOA code"].values
    met_num = data["Number\nof meters\n"].values
    consump = data["Total \nconsumption\n(kWh)"].values

# Replace the 'null' data to 'NaN'
    met_num, consump = remove_nan_to_NAN(met_num, consump)

# Define the reference time and Convert into correct format
    start_time = datetime.datetime.strptime(year + "-01-01T12:00:00.000Z", '%Y-%m-%dT%H:%M:%S.000Z')
    end_time = datetime.datetime.strptime(year + "-12-31T12:00:00.000Z", '%Y-%m-%dT%H:%M:%S.000Z')

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

def upload_gas_data_to_KG (year:str = '2020',
                query_endpoint: str = QUERY_ENDPOINT,
                update_endpoint: str = UPDATE_ENDPOINT):
    '''
        perform SPARQL update to upload the gas consumption/meter/nonmeter data into Blazegraph
        
        Arguments:
        year: the number of year of which the data you may want to read
        query_endpoint: str = QUERY_ENDPOINT,
        update_endpoint: str = UPDATE_ENDPOINT
    '''
# Retrieve reading from web
    logger.info('Retrieving Gas consumption data from Excel ...')
    data = read_from_web_gas(year)
    LSOA_codes = data["LSOA code"].values
    met_num = data["Number\nof meters\n"].values
    consump = data["Total \nconsumption\n(kWh)"].values
    non_met_num = data['Number of\nnon-consuming meters'].values

# Replace the 'null' data to 'NaN'
    met_num, consump, non_met_num = remove_nan_to_NAN(met_num, consump, non_met_num)

# Define the reference time and Convert into correct format
    start_time = datetime.datetime.strptime(year + "-01-01T12:00:00.000Z", '%Y-%m-%dT%H:%M:%S.000Z')
    end_time = datetime.datetime.strptime(year + "-12-31T12:00:00.000Z", '%Y-%m-%dT%H:%M:%S.000Z')

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

            used_uuid = COMPA + 'hasConsumed_' + str(uuid.uuid4())
            met_uuid = COMPA + 'GasMeter_' + str(uuid.uuid4())
            kw_uuid = COMPA + 'KW_' + str(uuid.uuid4())
            mes_uuid = COMPA + 'Measure_' + str(uuid.uuid4())
            triples += gas_update_template(mes_uuid,used_uuid,start_time,end_time, \
                                                  region, kw_uuid, cons, met_uuid, meters, non_meters)

        region = LSOA_codes[int(len_query[g + 1]) - 1]
        meters = met_num[int(len_query[g + 1]) - 1]
        cons = consump[int(len_query[g + 1]) - 1]

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

def upload_Geoinfo_to_KG(query_endpoint: str = QUERY_ENDPOINT,
                         update_endpoint: str = UPDATE_ENDPOINT):
    '''
        perform SPARQL update to upload the geo data of LSOA regions into Blazegraph
        
        Arguments:
        query_endpoint: str = QUERY_ENDPOINT,
        update_endpoint: str = UPDATE_ENDPOINT
    '''
    logger.info('Retrieving ONS geographic data from local pickle file ...')
    shape_array = read_from_pickle('./Data/shapes_array')

    # Get rid of repeated data
    LSOA_codes = np.unique(shape_array[:,0])
    LSOA_codes = np.char.replace(LSOA_codes, 'http://statistics.data.gov.uk/id/statistical-geography/', '')
    
    # Initialisation 
    wkt_codes=np.zeros(len(LSOA_codes),dtype=object)
    
    # Extract data of interest 
    for i in range(len(LSOA_codes)):
      indices = np.where(shape_array[:, 0] == LSOA_codes[i])
      wkt_codes[i] = shape_array[indices[0][0], 1]

    
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

def upload_fuel_poverty_to_KG (year:str = '2020',
                query_endpoint: str = QUERY_ENDPOINT,
                update_endpoint: str = UPDATE_ENDPOINT):
    '''
        perform SPARQL update to upload the fuel poor number, total household number data into Blazegraph
        
        Arguments:
        year: the number of year of which the data you may want to read
        query_endpoint: str = QUERY_ENDPOINT,
        update_endpoint: str = UPDATE_ENDPOINT
    '''
    # Retrieve reading from web
    logger.info('Retrieving Fuel poverty data from Excel ...')
    data = read_from_web_fuel_poverty(year)

    LSOA_codes = data["LSOA Code"].values
    house_num = data["Number of households"].values
    poor_num = data["Number of households in fuel poverty"].values

    # Replace the 'null' data to 'NaN'
    house_num, poor_num = remove_nan_to_NAN(house_num, poor_num)

    # Define the reference time and Convert into correct format
    start_time = datetime.datetime.strptime(year + "-01-01T12:00:00.000Z", '%Y-%m-%dT%H:%M:%S.000Z')
    end_time = datetime.datetime.strptime(year + "-12-31T12:00:00.000Z", '%Y-%m-%dT%H:%M:%S.000Z')

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

def upload_hadUK_climate_to_KG (year:str = '2020',
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
    '''

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

    def read_nc(var_name,year,loc=True):
        '''
        Given a variable in the HadUK Grid dataset, reads the file
        and returns the grid of observations.
        '''
        fn = read_from_web_temp(year, var_name)
        ds = nc.Dataset(fn)
        var_grid = ds.variables[var_name][:]
        if loc == True:
            lon = ds.variables['longitude'][:]
            lat = ds.variables['latitude'][:]
            ds.close()
            return lon, lat, var_grid
        else:
            ds.close()
            return var_grid

    def gridded_data_to_array(lon,lat,nc_vars,month,centroids=True):
        '''
        Calculate gridded data to array
        '''
        if centroids == True:
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
                    if centroids == True:
                        point = Point([lon[i,j],lat[i,j]])
                        centroid = point
                        overall_centroids.append(centroid)
                    var_store = np.append(var_store,[nc_var_temp],axis=0)
        var_store = var_store[1:,:]
        if centroids == True:
            return overall_centroids, var_store
        else:
            return var_store
    
    def get_treated_shape():
      '''
      Perform treatment of geographic data to assist calculation of climate data
      return list containing LSOA code and shape
      '''
      # Get geodata
      usage_vals = call_pickle('./Data/pickle_files/shapes_array')
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

      return LSOA

    LSOA = get_treated_shape()
    lon,lat,tas = read_nc('tas',loc=True)
    tasmin = read_nc('tasmin',loc=False)
    tasmax = read_nc('tasmax',loc=False)
    months = ['January','February','March','April','May','June','July','August','September','October','November','December']
    clim_vars = ['tasmin','tas','tasmax']

    nc_vars_full = [] 
    for i in range(len(months)):
        month_str = months[i]
        month,month_end = month_num(month_str)
        if i == 0:
            grid_loc,nc_vars_add = gridded_data_to_array(lon,lat,[tasmin,tas,tasmax],month,centroids=True)
        if i == 8:
            nc_vars_add = gridded_data_to_array(lon,lat,[tasmin,tas,tasmax],month,centroids=False)
            
            for j in range(6):
                nc_vars_add=np.append(nc_vars_add,[[12,14,16]],axis=0) 
                j = j +1
        else:
            nc_vars_add = gridded_data_to_array(lon,lat,[tasmin,tas,tasmax],month,centroids=False)

        nc_vars_full.append(nc_vars_add)
    
    full_grid = MultiPoint(points=list(grid_loc))

def upload_all(year: str = '2020',query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT):
    
    # instantiate the Electricity consumption data
    print("\nUploading the Electricity consumption data:")
    logger.info("Uploading the Electricity consumption data...")
    t1 = time.time()
    num_elec = upload_elec_data_to_KG(year, query_endpoint, update_endpoint)
    print(f"Number of LOSA output area with instantiated Electricity consumption/meter data: {num_elec}")
    t2= time.time()
    diff = t2 - t1
    print(f'Electricity consumption - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    logger.info(f'Electricity consumption - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    
    # instantiate the Gas consumption data
    print("\nUploading the Gas consumption data:")
    logger.info("Uploading the Gas consumption data...")
    t1 = time.time()
    num_gas = upload_gas_data_to_KG(year, query_endpoint, update_endpoint)
    print(f"Number of LOSA output area with instantiated Gas consumption data: {num_elec}")
    t2= time.time()
    diff = t2 - t1
    print(f'Gas consumption data - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    logger.info(f'Gas consumption data - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')

    # instantiate the ONS geographic data
    print("\nUploading the ONS geographic data:")
    logger.info("Uploading the ONS geographic data...")
    t1 = time.time()
    num_shape = upload_Geoinfo_to_KG(query_endpoint, update_endpoint)
    print(f"Number of LOSA output area with instantiated ONS geographic data: {num_elec}")
    t2= time.time()
    diff = t2 - t1
    print(f'ONS geographic data - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    logger.info(f'ONS geographic data - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    
    # instantiate the Fuel poverty data
    print("\nUploading the Fuel poverty data:")
    logger.info("Uploading the Fuel poverty data...")
    t1 = time.time()
    num_fuelpoor = upload_fuel_poverty_to_KG(year, query_endpoint, update_endpoint)
    print(f"Number of LOSA output area with instantiated Fuel poverty data: {num_elec}")
    t2= time.time()
    diff = t2 - t1
    print(f'Fuel poverty - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    logger.info(f'Fuel poverty - Finished after: {diff//60:5>n} min, {diff%60:4.2f} s \n')
    
    return num_elec, num_gas, num_shape, num_fuelpoor

if __name__ == '__main__':
# years = ['2018','2017','2016','2015']
# for year in years:
#         upload_year(year)
    '''
    num_elec, = upload_all()
    print(f'Number of instantiated LSOA area:{len_query}')
    print(f'Number of updated time series readings (i.e. dataIRIs):{added_ts}')
    '''
    upload_elec_data_to_KG('2025')