################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to perform data retrieval from web or knowledge graph
# 
# NOTE: pd.DataFrame is used frequently in this module, where for all the dataframe
# without special notice, the column[0] should all be LSOA code used as identifier

import agentlogging
from agent.errorhandling.exceptions import *
from agent.datamodel.functionality import *

import pandas as pd
import os
import requests
from bs4 import BeautifulSoup 

# Initialise logger
logger = agentlogging.get_logger("prod")

# ------------------------ Data retrieval (from Web) ------------------------ #
def read_from_web_monthly_distribution_elec(year:str ):
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

def read_from_web_monthly_distribution_gas(year:str ):
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

def read_from_web_price_elec(year:str ):
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

def read_from_web_price_gas(year:str ):
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

