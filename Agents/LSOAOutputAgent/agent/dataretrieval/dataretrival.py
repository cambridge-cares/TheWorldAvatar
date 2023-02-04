################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to perform data retrieval from web or knowledge graph
# 
# NOTE: pd.DataFrame is used frequently in this module, where for all the dataframe
# without special notice, the column[0] should all be LSOA code used as identifier

import agentlogging
from agent.kgutils.kgclient import KGClient
from agent.kgutils.querytemplates import output_query_template
from agent.errorhandling.exceptions import *
from agent.datamodel.functionality import *
from agent.utils.env_configs import YEAR
from agent.utils.stack_configs import (QUERY_ENDPOINT, UPDATE_ENDPOINT)

import geopandas as gpd
import pandas as pd
import numpy as np
import os
import requests
import shapely.wkt
from bs4 import BeautifulSoup 

# Initialise logger
logger = agentlogging.get_logger("prod")

# ------------------------ Data retrieval (from Web) ------------------------ #
def read_from_web_monthly_distribution_elec(year:str = YEAR):
  '''
  This function is to read the up-to-date national electricity/gas consumption monthly distribution data from web
  return a list to represent the file been read, [Jan, Feb, ..., Nov, Dec]
  NOTE: This function can work based on the assumption that the web address and page structure  won't change,
  if the xlsx file can't be correctly retrieved it is possible that the web infrastructure has been
  amended.

  Arguments:
        year: the number of year of which the data you may want to read
  '''
  url ='https://www.gov.uk/government/statistics/electricity-section-5-energy-trends'

  # Use requests to get the HTML of the website
  response = requests.get(url)
  soup = BeautifulSoup(response.text, 'html.parser')

  # Find the link to the xlsx file on the website
  download_div = soup.find_all('div', {'class': 'attachment-thumb'})
  link = download_div[5].find('a', href=True)['href']

  # Download the xlsx file
  try:
    file_name = os.path.basename(link) #'Monthly distribution of natinal electricity/gas consumption'
    response = requests.get(link)
    open('./downloads/'+ file_name, 'wb').write(response.content)
  except Exception as ex:
    logger.error(f"Excel file fail to be downloaded")
    raise InvalidInput(f'Excel file fail to be downloaded, please check if {url} is a valid address and webpage') from ex
  
  logger.info(f'xlsx file {file_name} have been downloaded at the ./downloads folder')
  # Check if the file is valid
  if not '5.5' in file_name:
    if not 'ET' in file_name:
        logger.error(f"Invalid file downloaded -- check the file in ./downloads folder and source:{url} ")
        raise InvalidInput(f'The file downloaded is not valid, check the webpage:{url} or download mannually.') 

  # Parse the data from the xlsx file into a pandas DataFrame
  try: 
    df = pd.read_excel('./downloads/'+ file_name, sheet_name = 'Month', engine='openpyxl',skiprows=5)
    df = df[df['Month'].str.contains(year)]
    df['Electricity'] = df[df.columns[df.columns.str.contains('Total electricity consumption')]]
    df['Electricity'] = df['Electricity'].apply(convert_to_float)
    value_list = df['Electricity'].tolist()
    logger.info(f'Monthly distribution for electricity successfully retrieved from web')

  except Exception as ex:
    logger.error(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
please check the {file_name} located on the file in ./downloads folder, see if the desire sheet (and sheet name) exist")
    raise InvalidInput(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
please check the {file_name} located on the file in ./downloads folder, see if the desire sheet (and sheet name) exist") from ex
  
  return value_list

def read_from_web_monthly_distribution_gas(year:str = YEAR):
  '''
  This function is to read the up-to-date national electricity/gas consumption monthly distribution data from web
  return a list to represent the file been read, [Jan, Feb, ..., Nov, Dec]
  NOTE: This function can work based on the assumption that the web address and page structure  won't change,
  if the xlsx file can't be correctly retrieved it is possible that the web infrastructure has been
  amended.

  Arguments:
        year: the number of year of which the data you may want to read
  '''
  url = 'https://www.gov.uk/government/statistics/total-energy-section-1-energy-trends'

  # Use requests to get the HTML of the website
  response = requests.get(url)
  soup = BeautifulSoup(response.text, 'html.parser')

  # Find the link to the xlsx file on the website
  download_div = soup.find_all('div', {'class': 'attachment-thumb'})
  link = download_div[2].find('a', href=True)['href']

  # Download the xlsx file
  try:
    file_name = os.path.basename(link) #'Monthly distribution of natinal electricity/gas consumption'
    response = requests.get(link)
    open('./downloads/'+ file_name, 'wb').write(response.content)
  except Exception as ex:
    logger.error(f"Excel file fail to be downloaded")
    raise InvalidInput(f'Excel file fail to be downloaded, please check if {url} is a valid address and webpage') from ex
  
  logger.info(f'xlsx file {file_name} have been downloaded at the ./downloads folder')
  # Check if the file is valid
  if not '1.2' in file_name:
    if not 'ET' in file_name:
      logger.error(f"Invalid file downloaded -- check the file in ./downloads folder and source:{url} ")
      raise InvalidInput(f'The file downloaded is not valid, check the webpage:{url} or download mannually.')
  
  # Parse the data from the xlsx file into a pandas DataFrame
  try: 
        df = pd.read_excel('./downloads/'+ file_name, sheet_name = 'Month', usecols=range(0,9), engine='openpyxl',skiprows=5)
        df = df[df['Month'].str.contains(year)]
        df['Gas'] = df[df.columns[df.columns.str.contains('Natural gas')]]
        df['Gas'] = df['Gas'].apply(convert_to_float)
        value_list = df['Gas'].tolist()
        logger.info(f'Monthly distribution for gas consumption successfully retrieved from web')

  except Exception as ex:
    logger.error(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
please check the {file_name} located on the file in ./downloads folder, see if the desire sheet (and sheet name) exist")
    raise InvalidInput(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
please check the {file_name} located on the file in ./downloads folder, see if the desire sheet (and sheet name) exist") from ex
  
  return value_list
#Monthly_gas_con = read_from_web_monthly_distribution_gas('2020')
#Monthly_elec_con = read_from_web_monthly_distribution_elec('2020')

def read_from_web_price_elec(year:str = YEAR):
  '''
  This function is to read the up-to-date national electricity/gas prices from web
  return a float value for price (pounds/kwh)
  NOTE: This function can work based on the assumption that the web address and page structure  won't change,
  if the xlsx file can't be correctly retrieved it is possible that the web infrastructure has been
  amended.

  Arguments:
        year: the number of year of which the data you may want to read
  '''
  url = 'https://www.gov.uk/government/statistical-data-sets/annual-domestic-energy-price-statistics'

  # Use requests to get the HTML of the website
  response = requests.get(url)
  soup = BeautifulSoup(response.text, 'html.parser')

  # Find the link to the xlsx file on the website
  download_div = soup.find_all('div', {'class': 'attachment-thumb'})
  link = download_div[3].find('a', href=True)['href']

  # Download the xlsx file
  try:
    file_name = os.path.basename(link) #'Monthly distribution of natinal electricity/gas consumption'
    response = requests.get(link)
    open('./downloads/'+ file_name, 'wb').write(response.content)
  except Exception as ex:
    logger.error(f"Excel file fail to be downloaded")
    raise InvalidInput(f'Excel file fail to be downloaded, please check if {url} is a valid address and webpage') from ex
  
  logger.info(f'xlsx file {file_name} have been downloaded at the ./downloads folder')
  # Check if the file is valid
  if not '224' in file_name:
        logger.error(f"Invalid file downloaded -- check the file in ./downloads folder and source:{url} ")
        raise InvalidInput(f'The file downloaded is not valid, check the webpage:{url} or download mannually.')
  
  # Parse the data from the xlsx file into a pandas DataFrame
  try: 
        df = pd.read_excel('./downloads/'+ file_name, sheet_name = '2.2.4', engine='openpyxl',skiprows=12)
        year = convert_to_int(year)
        df = df.loc[df.iloc[:,0] == year]
        row = df.loc[df.iloc[:,1] == "United Kingdom"]
        # get the value at the desire column in this row
        row = row.loc[:,row.columns.str.contains('Overall: Average variable unit price')]
        value = row.iloc[:,-1].values[0]
        logger.info(f'{year} electricity price successfully retrieved from web')

  except Exception as ex:
    logger.error(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
please check the {file_name} located on the file in ./downloads folder, see if the desire sheet (and sheet name) exist")
    raise InvalidInput(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
please check the {file_name} located on the file in ./downloads folder, see if the desire sheet (and sheet name) exist") from ex
  
  return value

def read_from_web_price_gas(year:str = YEAR):
  '''
  This function is to read the up-to-date national electricity/gas prices from web
  return a float value for price (pounds/kwh)
  NOTE: This function can work based on the assumption that the web address and page structure  won't change,
  if the xlsx file can't be correctly retrieved it is possible that the web infrastructure has been
  amended.

  Arguments:
        year: the number of year of which the data you may want to read
  '''
  url = 'https://www.gov.uk/government/statistical-data-sets/annual-domestic-energy-price-statistics'

  # Use requests to get the HTML of the website
  response = requests.get(url)
  soup = BeautifulSoup(response.text, 'html.parser')

  # Find the link to the xlsx file on the website
  download_div = soup.find_all('div', {'class': 'attachment-thumb'})
  link = download_div[8].find('a', href=True)['href']

  # Download the xlsx file
  try:
    file_name = os.path.basename(link) #'Monthly distribution of natinal electricity/gas consumption'
    response = requests.get(link)
    open('./downloads/'+ file_name, 'wb').write(response.content)
  except Exception as ex:
    logger.error(f"Excel file fail to be downloaded")
    raise InvalidInput(f'Excel file fail to be downloaded, please check if {url} is a valid address and webpage') from ex
  
  logger.info(f'xlsx file {file_name} have been downloaded at the ./downloads folder')
  # Check if the file is valid
  if not '234' in file_name:
        logger.error(f"Invalid file downloaded -- check the file in ./downloads folder and source:{url} ")
        raise InvalidInput(f'The file downloaded is not valid, check the webpage:{url} or download mannually.')
  # Parse the data from the xlsx file into a pandas DataFrame
  try: 
        df = pd.read_excel('./downloads/'+ file_name, sheet_name = '2.3.4', engine='openpyxl',skiprows=10)
        year = convert_to_int(year)
        df = df.loc[df.iloc[:,0] == year]
        row = df.loc[df.iloc[:,1] == "Great Britain"]
        # get the value at the last column in this row
        row = row.loc[:,row.columns.str.contains('Overall: Average variable unit price')]
        value = row.iloc[:,-1].values[0]
        logger.info(f'{year} gas price successfully retrieved from web')

  except Exception as ex:
    logger.error(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
please check the {file_name} located on the file in ./downloads folder, see if the desire sheet (and sheet name) exist")
    raise InvalidInput(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
please check the {file_name} located on the file in ./downloads folder, see if the desire sheet (and sheet name) exist") from ex
  
  return value
#elec_price = read_from_web_price_elec('2020')
#gas_price = read_from_web_price_gas('2020')

def read_from_web_carbon_index(var: str, year:str = YEAR):
  '''
  This function is to read the up-to-date national electricity/gas carbon index from web
  return a float value for price (kgCO2e/unit)
  NOTE: This function can work based on the assumption that the web address and page structure  won't change,
  if the xlsx file can't be correctly retrieved it is possible that the web infrastructure has been
  amended.

  Arguments:
        var: 'Gas'/'Electricity'
        year: the number of year of which the data you may want to read
  '''
  url = 'https://www.gov.uk/government/publications/greenhouse-gas-reporting-conversion-factors-' + year

  # Use requests to get the HTML of the website
  response = requests.get(url)
  soup = BeautifulSoup(response.text, 'html.parser')

  # Find the link to the xlsx file on the website
  download_div = soup.find_all('div', {'class': 'attachment-thumb'})
  link = download_div[0].find('a', href=True)['href']

  # Download the xlsx file
  try:
    file_name = os.path.basename(link) #'Monthly distribution of natinal electricity/gas consumption'
    response = requests.get(link)
    open('./downloads/'+ file_name, 'wb').write(response.content)
  except Exception as ex:
    logger.error(f"Excel file fail to be downloaded")
    raise InvalidInput(f'Excel file fail to be downloaded, please check if {url} is a valid address and webpage') from ex
  
  logger.info(f'xlsx file {file_name} have been downloaded at the ./downloads folder')
  # Check if the file is valid
  if not 'conversion-factors' in file_name:
    if not year in file_name:
        logger.error(f"Invalid file downloaded -- check the file in ./downloads folder and source:{url} ")
        raise InvalidInput(f'The file downloaded is not valid, check the webpage:{url} or download mannually.')
  # Parse the data from the xlsx file into a pandas DataFrame
  if var == 'Gas':
    try:
        df = pd.read_excel('./downloads/'+ file_name, sheet_name = 'Fuels', engine='openpyxl',skiprows=10)
        # Find the cell that contains the value 'Activity'
        df_boo = df.apply(lambda x: x.astype(str).str.find('Natural gas'))
        cell_value = df[df_boo != -1].dropna(how='all')
        index = df.iloc[cell_value.index[0]+3, 4]
        logger.info(f'{year} {var} carbon index successfully retrieved from web')

    except Exception as ex:
        logger.error(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
    please check the {file_name} located on the file in ./downloads folder, see if the desire sheet (and sheet name) exist")
        raise InvalidInput(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
    please check the {file_name} located on the file in ./downloads folder, see if the desire sheet (and sheet name) exist") from ex
  
  if var == 'Electricity':
    try:
        df = pd.read_excel('./downloads/'+ file_name, sheet_name = 'UK electricity', engine='openpyxl',skiprows=10)
        # Find the cell that contains the value 'Activity'
        df_boo = df.apply(lambda x: x.astype(str).str.find('Electricity: UK'))
        cell_value = df[df_boo != -1].dropna(how='all')
        index = df.iloc[cell_value.index[0], 5]
        logger.info(f'{year} {var} carbon index successfully retrieved from web')

    except Exception as ex:
        logger.error(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
    please check the {file_name} located on the file in ./downloads folder, see if the desire sheet (and sheet name) exist")
        raise InvalidInput(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
    please check the {file_name} located on the file in ./downloads folder, see if the desire sheet (and sheet name) exist") from ex
  
  return index
#carb_index_elec = read_from_web_carbon_index('Electricity','2020')

# ------------------------ Data retrieval (from KG) ------------------------ #
def retrieve_elec_data_from_KG(query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT, year: str = YEAR, per_household: bool = False) -> pd.DataFrame:
    '''
        perform SPARQL query to get the data from Blazegraph, return a DataFrame looks like:
            's'  'usage'  'meter' ('elec_consump_perhousehold')
        0
        1
        2
        ...

        Arguments:
        query_endpoint: str = QUERY_ENDPOINT,
        update_endpoint: str = UPDATE_ENDPOINT
        year: the number of year of which the data you may want to read
        per_household: bool, default as False, which the return value is at national scale. if it is yes, will return the value for per household
    '''
    # Get query string
    query = output_query_template('Electricity', year)

    # Construct kg client
    logger.info('Querying electricity consumption data from Knowledge graph...')
    kg_client = KGClient(query_endpoint, update_endpoint)
    result = kg_client.performQuery(query)

    # Parse the result into DataFrame
    df = pd.DataFrame(columns=['s','usage','meter'])
    df = df.append(result)

    # Adjust the format
    df["usage"] = df["usage"].apply(convert_to_float)
    df["meter"] = df["meter"].apply(convert_to_int)

    if per_household == True:
        df['elec_consump_perhousehold'] = df["usage"].to_numpy() / df["meter"].to_numpy() 
    
    # Get rid of 'NaN' variables
    df = remove_unlocated_data(df)
    df = remove_NaN(df)
    logger.info(f'{df.shape[0]} number of LSOA of which electricity consumption/meter data have been retrieved')

    return df 

def retrieve_gas_data_from_KG(query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT, year: str = YEAR, per_household: bool = False) -> pd.DataFrame:
    '''
    perform SPARQL query to get the data from Blazegraph, return a DataFrame looks like:
        's'  'usage'  'consuming meter' ('gas_consump_perhousehold')
    0
    1
    2
    ...

    Arguments:
    query_endpoint: str = QUERY_ENDPOINT,
    update_endpoint: str = UPDATE_ENDPOINT
    year: the number of year of which the data you may want to read
    per_household: bool, default as False, which the return value is at national scale. if it is yes, will return the value for per household
    '''
    # Get query string
    query = output_query_template('Gas', year)

    # Construct kg client
    logger.info('Querying Gas consumption data from Knowledge graph...')
    kg_client = KGClient(query_endpoint, update_endpoint)
    result = kg_client.performQuery(query)

    # Parse the result into DataFrame
    df = pd.DataFrame(columns=['s','usage','meter','nonmeter'])
    df = df.append(result)

    # Adjust the format
    df["nonmeter"] = df["nonmeter"].apply(convert_to_zero)
    df["usage"] = df["usage"].apply(convert_to_float)
    df["meter"] = df["meter"].apply(convert_to_int)

    # Get rid of 'NaN' variables
    df = remove_NaN(df)
    df = remove_unlocated_data(df)

    # Calculate consuming meter which is the only meaningful var
    df['consuming meter'] = df['meter'] - df['nonmeter']
    df = drop_column(df,['meter','nonmeter'])

    if per_household == True:
        df['gas_consump_perhousehold'] = df["usage"].to_numpy() / df["meter"].to_numpy() 
    
    logger.info(f'{df.shape[0]} number of LSOA of which gas consumption/consuming meter data have been retrieved')
    return df

def retrieve_fuel_poor_from_KG(query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT, year: str = YEAR) -> pd.DataFrame:
    '''
    perform SPARQL query to get the data from Blazegraph, return a DataFrame looks like:
        's'  'result' 'num' 
    0
    1
    2
    ...

    Arguments:
    year: the number of year of which the data you may want to read
    query_endpoint: str = QUERY_ENDPOINT,
    update_endpoint: str = UPDATE_ENDPOINT
'''
    # Get query string
    query = output_query_template('Fuel poverty', year)

    # Construct kg client
    logger.info('Querying Fuel Poverty data from Knowledge graph...')
    kg_client = KGClient(query_endpoint, update_endpoint)
    result = kg_client.performQuery(query)

    # Parse the result into DataFrame
    df = pd.DataFrame(columns=['s', 'result', 'num'])
    df = df.append(result)

    # Adjust the format
    df["result"] = df["result"].apply(convert_to_float)
    df["num"] = df["num"].apply(convert_to_int)

    # Get rid of 'NaN' variables
    df = remove_unlocated_data(df)
    df = remove_NaN(df)
    logger.info(f'{df.shape[0]} number of LSOA of which Fuel poverty/number of household data have been retrieved')

    return df

def retrieve_ONS_shape_from_KG(query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT, year: str = YEAR) -> pd.DataFrame:
    '''
    perform SPARQL query to get the data from Blazegraph, return a DataFrame looks like:
    Note: NO NEED to specify year or iris
        's'  'geometry' 
    0
    1
    2
    ...

    Arguments:
    query_endpoint: str = QUERY_ENDPOINT,
    update_endpoint: str = UPDATE_ENDPOINT
'''

    # Get query string
    query = output_query_template('ONS output area', year, iris= False)

    # Construct kg client
    logger.info('Querying ONS output area shape from Knowledge graph...')
    kg_client = KGClient(query_endpoint, update_endpoint)
    result = kg_client.performQuery(query)

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
    # Parse the result into DataFrame
    df = pd.DataFrame(columns=['s', 'geom'])
    for i in range(len(result)):
        df.loc[i,'s']=result[i,0]
        df.loc[i,'geom']=result[i,1]
    
    # specifying geodata frame
    df['geometry'] = gpd.GeoSeries.from_wkt(df['geom'])
    df = drop_column(df,'geom')

    df = gpd.GeoDataFrame(df, geometry='geometry')
    df = df.set_crs("EPSG:4326")
    print('Converting to Mercator projection (better than WGS84 for UK)')
    df = df.to_crs("EPSG:3395")
    # Enable the func below to save the df to pickle file, save the time for querying and processing :)
    #save_pickle_variable(df_geo = df)

    df = remove_unlocated_data(df)

    logger.info(f'{df.shape[0]} number of LSOA of which ONS output area shape data have been retrieved')
    return df

def retrieve_temp_from_KG(query_endpoint: str = QUERY_ENDPOINT, update_endpoint: str = UPDATE_ENDPOINT, year: str = YEAR) -> pd.DataFrame:
    '''
    perform SPARQL query to get the data from Blazegraph, return a DataFrame looks like:
        s start var t  
    0
    1
    2
    ...

    Arguments:
    year: the number of year of which the data you may want to read
    iris: True to return data iri, False to not
    query_endpoint: str = QUERY_ENDPOINT,
    update_endpoint: str = UPDATE_ENDPOINT
'''
    # Get query string
    query = output_query_template('Temperature', year)

    # Construct kg client
    logger.info('Querying Climate temperature data from Knowledge graph...')
    kg_client = KGClient(query_endpoint, update_endpoint)
    result = kg_client.performQuery(query)

    # Parse the result into DataFrame
    df = pd.DataFrame(columns=['s', 'start', 'var', 't' ])
    df = df.append(result)

    df["t"] = df["t"].apply(convert_to_float)
    df = remove_unlocated_data(df)
    df = remove_NaN(df)

    logger.info(f'{df.shape[0]/36} number of LSOA of which Climate tempeature data (max, mean, min of each 12 months) have been retrieved')
    return df
