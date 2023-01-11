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
  df = pd.read_excel(file_name, sheet_name = year, engine='openpyxl',skiprows=4, skipfooter=1)
  logger.info('Electricity consumption/meter data successfully retrieved from web')
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
  
  # Parse the data from the xlsx file into a pandas DataFrame
  df = pd.read_excel(file_name, sheet_name = year, engine='openpyxl',skiprows=4, skipfooter=1)
  logger.info('Gas consumption/meter data successfully retrieved from web')
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

  # Parse the data from the xlsx file into a pandas DataFrame
  df = pd.read_excel(file_name, sheet_name="Table 3", skiprows=2, skipfooter=8)
  logger.info('Fuel poverty data successfully retrieved from web')
  return df

def read_from_web_temp (year: str, var_name: str):
  '''
  This function is to read the up-to-date hadUK climate data (1km grid) from web
  return a dataframe to represent the file been read
  NOTE: This function can work based on the assumption that the web address and page structure  won't change,
  if the xlsx file can't be correctly retrieved it is possible that the web infrastructure has been
  amended.

  Arguments:
        year: the number of year of which the data you may want to read
  '''
  url = f'https://data.ceda.ac.uk/badc/ukmo-hadobs/data/insitu/MOHC/HadOBS/HadUK-Grid/v1.1.0.0/1km/{var_name}/mon/latest'

  # Use requests to get the HTML of the website
  response = requests.get(url)
  soup = BeautifulSoup(response.text, 'html.parser')

  # Find the link to the xlsx file on the website
  download_div = soup.find_all('table', {'class': 'table table-sm'})
  inner_url = download_div[0].find_all('a', href=True)
  
  for year_link in inner_url:
        if year in year_link['href']:
            link = year_link['href']

  try: 
      # Download the xlsx file
      file_name = os.path.basename(link).split('?')[0]

  except Exception as ex:
      print(f'The hadUK climate data for {year} can not be found, please check the webpage: {url} to see if that year of data file exist')
      raise InvalidInput(f'The hadUK climate data for {year} can not be found, please check the webpage:{url} to see if that year of data file exist') from ex
  
  response = requests.get(link)
  open(file_name, 'wb').write(response.content)

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
    met_num =  [f"'NaN'^^<{XSD_STRING}>" if np.isnan(met_num) else met_num for met_num in met_num]  
    consump =  [f"'NaN'^^<{XSD_STRING}>" if np.isnan(consump) else consump for consump in consump]   
# Define the reference time
    start_time = year + "-01-01T12:00:00"
    end_time = year + "-12-31T12:00:00"

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
    met_num =  [f"'NaN'^^<{XSD_STRING}>" if np.isnan(met_num) else met_num for met_num in met_num]  
    non_met_num =  [f"'NaN'^^<{XSD_STRING}>" if np.isnan(non_met_num) else non_met_num for non_met_num in non_met_num]   
    consump =  [f"'NaN'^^<{XSD_STRING}>" if np.isnan(consump) else consump for consump in consump]   
# Define the reference time
    start_time = year + "-01-01T12:00:00"
    end_time = year + "-12-31T12:00:00"

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

    # Initialisation 
    LSOA_codes=np.zeros(len(shape_array),dtype=object)
    wkt_codes=np.zeros(len(shape_array),dtype=object)
    
    # Extract data of interest 
    for i in range(len(shape_array)):
        LSOA_codes[i] = shape_array[i,0].replace('http://statistics.data.gov.uk/id/statistical-geography/','') 
        wkt_codes[i] = shape_array[i,1]
    
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
    
    return num_elec, num_gas, num_shape

if __name__ == '__main__':
# years = ['2018','2017','2016','2015']
# for year in years:
#         upload_year(year)
    '''
    num_elec, = upload_all()
    print(f'Number of instantiated LSOA area:{len_query}')
    print(f'Number of updated time series readings (i.e. dataIRIs):{added_ts}')
    '''
    read_from_web_temp('2020','tas')