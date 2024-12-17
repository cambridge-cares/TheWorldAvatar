################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to perform data retrieval from web or knowledge graph
# 
# NOTE: pd.DataFrame is used frequently in this module, where for all the dataframe
# without special notice, the column[0] should all be LSOA code used as identifier

from code.datamodel.functionality import convert_to_float,convert_to_int
import pandas as pd
import numpy as np
import os
import requests
from bs4 import BeautifulSoup 
import csv
XSD = "http://www.w3.org/2001/XMLSchema#"
XSD_STRING = XSD + "string"

def process_list(data):
      # Remove the first bracket using indexing
      data = data[0]
      # Convert the list to a numpy array
      data = np.array(data)
      return data

# ------------------------ Data retrieval (from Web) ------------------------ #
def read_from_web_monthly_distribution_elec(year:str = '2019'):
  '''
  This function is to read the up-to-date national electricity/gas consumption monthly distribution data from web
  return a list to represent the file been read, [Jan, Feb, ..., Nov, Dec]
  NOTE: This function can work based on the assumption that the web address and page structure  won't change,
  if the xlsx file can't be correctly retrieved it is possible that the web infrastructure has been
  amended.

  Arguments:
        year: the number of year of which the data you may want to read
  '''
  try:
     with open(f'./Data/properties_csv/{year}/Elec_distribution.csv', 'r') as file:
        reader = csv.reader(file)
        for row in reader:
            value_list = row[0]
  
  except:
    url ='https://www.gov.uk/government/statistics/electricity-section-5-energy-trends'

    # Use requests to get the HTML of the website
    response = requests.get(url)
    soup = BeautifulSoup(response.text, 'html.parser')

    # Find the link to the xlsx file on the website
    download_div = soup.find_all('div', {'class': 'attachment-thumb'})
    link = download_div[5].find('a', href=True)['href']

    # Download the xlsx file
    try:
      if not os.path.exists('./downloads'):
          os.makedirs('./downloads')
      file_name = os.path.basename(link) #'Monthly distribution of natinal electricity/gas consumption'
      response = requests.get(link)
      open('./downloads/'+ file_name, 'wb').write(response.content)
    except Exception as ex:
      raise KeyError(f'Excel file fail to be downloaded, please check if {url} is a valid address and webpage') from ex
    
    # Check if the file is valid
    if not '5.5' in file_name:
      if not 'ET' in file_name:
          raise KeyError(f'The file downloaded is not valid, check the webpage:{url} or download mannually.') 

    # Parse the data from the xlsx file into a pandas DataFrame
    try: 
      df = pd.read_excel('./downloads/'+ file_name, sheet_name = 'Month', engine='openpyxl',skiprows=5)
      df = df[df['Month'].str.contains(year)]
      df['Electricity'] = df[df.columns[df.columns.str.contains('Total electricity consumption')]]
      df['Electricity'] = df['Electricity'].apply(convert_to_float)
      value_list = df['Electricity'].tolist()

    except Exception as ex:
      raise KeyError(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
  please check the {file_name} located on the file in ./downloads folder, see if the desire sheet (and sheet name) exist") from ex
    
    with open(f'./Data/properties_csv/{year}/Elec_distribution.csv', 'w', newline='') as file:
        writer = csv.writer(file)
        writer.writerow([value_list])
    
  return list(value_list)

def read_from_web_monthly_distribution_gas(year:str = '2019'):
  '''
  This function is to read the up-to-date national electricity/gas consumption monthly distribution data from web
  return a list to represent the file been read, [Jan, Feb, ..., Nov, Dec]
  NOTE: This function can work based on the assumption that the web address and page structure  won't change,
  if the xlsx file can't be correctly retrieved it is possible that the web infrastructure has been
  amended.

  Arguments:
        year: the number of year of which the data you may want to read
  '''
  try:
     with open(f'./Data/properties_csv/{year}/Gas_distribution.csv', 'r') as file:
        reader = csv.reader(file)
        for row in reader:
            value_list = row[0]
  except:
      url = 'https://www.gov.uk/government/statistics/total-energy-section-1-energy-trends'

      # Use requests to get the HTML of the website
      response = requests.get(url)
      soup = BeautifulSoup(response.text, 'html.parser')

      # Find the link to the xlsx file on the website
      download_div = soup.find_all('div', {'class': 'attachment-thumb'})
      link = download_div[2].find('a', href=True)['href']

      # Download the xlsx file
      try:
        if not os.path.exists('./downloads'):
            os.makedirs('./downloads')
        file_name = os.path.basename(link) #'Monthly distribution of natinal electricity/gas consumption'
        response = requests.get(link)
        open('./downloads/'+ file_name, 'wb').write(response.content)
      except Exception as ex:
        raise KeyError(f'Excel file fail to be downloaded, please check if {url} is a valid address and webpage') from ex
      
      # Check if the file is valid
      if not '1.2' in file_name:
        if not 'ET' in file_name:
          raise KeyError(f'The file downloaded is not valid, check the webpage:{url} or download mannually.')
      
      # Parse the data from the xlsx file into a pandas DataFrame
      try: 
            df = pd.read_excel('./downloads/'+ file_name, sheet_name = 'Month', usecols=range(0,9), engine='openpyxl',skiprows=5)
            df = df[df['Month'].str.contains(year)]
            df['Gas'] = df[df.columns[df.columns.str.contains('Natural gas')]]
            df['Gas'] = df['Gas'].apply(convert_to_float)
            value_list = df['Gas'].tolist()

      except Exception as ex:
        raise KeyError(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
please check the {file_name} located on the file in ./downloads folder, see if the desire sheet (and sheet name) exist") from ex
  
      with open(f'./Data/properties_csv/{year}/Gas_distribution.csv', 'w', newline='') as file:
          writer = csv.writer(file)
          writer.writerow([value_list])

  return list(value_list)

def read_from_web_price_elec(year:str = '2019'):
  '''
  This function is to read the up-to-date national electricity/gas prices from web
  return a float value for price (pounds/kwh)
  NOTE: This function can work based on the assumption that the web address and page structure  won't change,
  if the xlsx file can't be correctly retrieved it is possible that the web infrastructure has been
  amended.

  Arguments:
        year: the number of year of which the data you may want to read
  '''
  try:
      with open(f'./Data/properties_csv/{year}/Elect_price.csv', 'r') as file:
          reader = csv.reader(file)
          for row in reader:
              value = row[0]
          
  except:
      url = 'https://www.gov.uk/government/statistical-data-sets/annual-domestic-energy-price-statistics'

      # Use requests to get the HTML of the website
      response = requests.get(url)
      soup = BeautifulSoup(response.text, 'html.parser')

      # Find the link to the xlsx file on the website
      download_div = soup.find_all('div', {'class': 'attachment-thumb'})
      link = download_div[3].find('a', href=True)['href']

      # Download the xlsx file
      try:
        if not os.path.exists('./downloads'):
            os.makedirs('./downloads')
        file_name = os.path.basename(link) #'Monthly distribution of natinal electricity/gas consumption'
        response = requests.get(link)
        open('./downloads/'+ file_name, 'wb').write(response.content)
      except Exception as ex:
        raise KeyError(f'Excel file fail to be downloaded, please check if {url} is a valid address and webpage') from ex
      
      # Check if the file is valid
      if not '224' in file_name:
            raise KeyError(f'The file downloaded is not valid, check the webpage:{url} or download mannually.')
      
      # Parse the data from the xlsx file into a pandas DataFrame
      try: 
            df = pd.read_excel('./downloads/'+ file_name, sheet_name = '2.2.4', engine='openpyxl',skiprows=12)
            year = convert_to_int(year)
            df = df.loc[df.iloc[:,0] == year]
            row = df.loc[df.iloc[:,1] == "United Kingdom"]
            # get the value at the desire column in this row
            row = row.loc[:,row.columns.str.contains('Overall: Average variable unit price')]
            value = row.iloc[:,-1].values[0]

      except Exception as ex:
        raise KeyError(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
    please check the {file_name} located on the file in ./downloads folder, see if the desire sheet (and sheet name) exist") from ex
      
      
      with open(f'./Data/properties_csv/{year}/Elect_price.csv', 'w', newline='') as file:
          writer = csv.writer(file)
          writer.writerow([value])

  return float(value)

def read_from_web_price_gas(year:str = '2019'):
  '''
  This function is to read the up-to-date national electricity/gas prices from web
  return a float value for price (pounds/kwh)
  NOTE: This function can work based on the assumption that the web address and page structure  won't change,
  if the xlsx file can't be correctly retrieved it is possible that the web infrastructure has been
  amended.

  Arguments:
        year: the number of year of which the data you may want to read
  '''
  try:
     with open(f'./Data/properties_csv/{year}/Gas_price.csv', 'r') as file:
        reader = csv.reader(file)
        for row in reader:
            value = row[0]
  except:
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
        raise KeyError(f'Excel file fail to be downloaded, please check if {url} is a valid address and webpage') from ex
      
      # Check if the file is valid
      if not '234' in file_name:
            raise KeyError(f'The file downloaded is not valid, check the webpage:{url} or download mannually.')
      # Parse the data from the xlsx file into a pandas DataFrame
      try: 
            df = pd.read_excel('./downloads/'+ file_name, sheet_name = '2.3.4', engine='openpyxl',skiprows=10)
            year = convert_to_int(year)
            df = df.loc[df.iloc[:,0] == year]
            row = df.loc[df.iloc[:,1] == "Great Britain"]
            # get the value at the last column in this row
            row = row.loc[:,row.columns.str.contains('Overall: Average variable unit price')]
            value = row.iloc[:,-1].values[0]

      except Exception as ex:
        raise KeyError(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
please check the {file_name} located on the file in ./downloads folder, see if the desire sheet (and sheet name) exist") from ex
  
      with open(f'./Data/properties_csv/{year}/Gas_price.csv', 'w', newline='') as file:
          writer = csv.writer(file)
          writer.writerow([value])

  return float(value)

def read_from_web_carbon_index(var: str, year:str = '2019'):
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
  try:
     with open(f'./Data/properties_csv/{year}/{var}_carbon_factor.csv', 'r') as file:
        reader = csv.reader(file)
        for row in reader:
            index = row[0]
  except:
      url = 'https://www.gov.uk/government/publications/greenhouse-gas-reporting-conversion-factors-' + year

      # Use requests to get the HTML of the website
      response = requests.get(url)
      soup = BeautifulSoup(response.text, 'html.parser')

      # Find the link to the xlsx file on the website
      download_div = soup.find_all('div', {'class': 'attachment-thumb'})
      link = download_div[0].find('a', href=True)['href']

      # Download the xlsx file
      try:
        if not os.path.exists('./downloads'):
            os.makedirs('./downloads')
        file_name = os.path.basename(link) #'Monthly distribution of natinal electricity/gas consumption'
        response = requests.get(link)
        open('./downloads/'+ file_name, 'wb').write(response.content)
      except Exception as ex:
        raise KeyError(f'Excel file fail to be downloaded, please check if {url} is a valid address and webpage') from ex
      
      # Check if the file is valid
      if not 'conversion-factors' in file_name:
        if not year in file_name:
            raise KeyError(f'The file downloaded is not valid, check the webpage:{url} or download mannually.')
      # Parse the data from the xlsx file into a pandas DataFrame
      if var == 'Gas':
        try:
            df = pd.read_excel('./downloads/'+ file_name, sheet_name = 'Fuels', engine='openpyxl',skiprows=10)
            # Find the cell that contains the value 'Activity'
            df_boo = df.apply(lambda x: x.astype(str).str.find('Natural gas'))
            cell_value = df[df_boo != -1].dropna(how='all')
            index = df.iloc[cell_value.index[0]+3, 4]

        except Exception as ex:
            raise KeyError(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
        please check the {file_name} located on the file in ./downloads folder, see if the desire sheet (and sheet name) exist") from ex
      
      if var == 'Electricity':
        try:
            df = pd.read_excel('./downloads/'+ file_name, sheet_name = 'UK electricity', engine='openpyxl',skiprows=10)
            # Find the cell that contains the value 'Activity'
            df_boo = df.apply(lambda x: x.astype(str).str.find('Electricity: UK'))
            cell_value = df[df_boo != -1].dropna(how='all')
            index = df.iloc[cell_value.index[0], 5]

        except Exception as ex:
            raise KeyError(f"Excel file fail to be read -- potentially there are changes of structure of the xlsx. \n \
        please check the {file_name} located on the file in ./downloads folder, see if the desire sheet (and sheet name) exist") from ex
      
      with open(f'./Data/properties_csv/{year}/{var}_carbon_factor.csv', 'w', newline='') as file:
          writer = csv.writer(file)
          writer.writerow([index])

  return float(index)

def read_from_excel_elec(year:str = '2020', dict = False):
    '''
        Return lists of readings from Excel
        
        Arguments:
        year: the number of year of which the data you may want to read
    '''

    try:
            data = pd.read_excel(f'./Data/source_data/LSOA_domestic_elec_2010-21.xlsx', sheet_name=year, skiprows=4)
    except Exception as ex:
            raise KeyError("Excel file can not be read -- try fixing by using absolute path") from ex

    if dict == False:
      LSOA_codes = data["LSOA code"].values
      met_num = data["Number\nof meters\n"].values
      consump = data["Total \nconsumption\n(kWh)"].values

      elec_consump = []
      elec_meter = []
      
      # Replace nan values with zeros using a list comprehension
      met_num =  [f"'NaN'^^<{XSD_STRING}>" if np.isnan(met_num) else met_num for met_num in met_num]  
      consump =  [f"'NaN'^^<{XSD_STRING}>" if np.isnan(consump) else consump for consump in consump]   
      
      elec_consump.append([[LSOA_codes[i],consump[i]] for i in range(len(LSOA_codes))])
      elec_meter.append([[LSOA_codes[i],met_num[i]] for i in range(len(LSOA_codes))])

      elec_consump = process_list(elec_consump)
      elec_meter = process_list(elec_meter)
    
    else:
      # Set "LSOA code" as the index of the dataframe
      data = data.set_index("LSOA code")

      # Create elec_consump dictionary
      elec_consump = {}
      for index, row in data.iterrows():
          elec_consump[index] = row["Total \nconsumption\n(kWh)"]

      # Create elec_meter dictionary
      elec_meter = {}
      for index, row in data.iterrows():
          elec_meter[index] = row["Number\nof meters\n"]
      # save_pickle_variable(elec_consump=elec_consump, elec_meter = elec_meter)
    
    
    print(f'Electricity consumption for year {year} successfully retrieved from Excel')
    return elec_consump, elec_meter

def read_from_excel_gas(year:str = '2020', dict = False):
    '''
        Return lists of readings from Excel
        
        Arguments:
        year: the number of year of which the data you may want to read
    '''

    try:
            data = pd.read_excel(f'./Data/source_data/LSOA_domestic_gas_2010-21.xlsx', sheet_name=year, skiprows=4)
    except Exception as ex:
            raise KeyError("Excel file can not be read -- try fixing by using absolute path") from ex
    if dict == False:
      LSOA_codes = data["LSOA code"].values
      met_num = data["Number\nof meters\n"].values
      non_met_num = data['Number of\nnon-consuming meters'].values
      consump = data["Total \nconsumption\n(kWh)"].values
      'Number of\nnon-consuming meters'

      gas_consump = []
      gas_meter = []
      gas_non_meter = []
      

      # Replace the 'null' data to zero
      met_num =  [f"'NaN'^^<{XSD_STRING}>" if np.isnan(met_num) else met_num for met_num in met_num]  
      non_met_num =  [f"'NaN'^^<{XSD_STRING}>" if np.isnan(non_met_num) else non_met_num for non_met_num in non_met_num]   
      consump =  [f"'NaN'^^<{XSD_STRING}>" if np.isnan(consump) else consump for consump in consump]  
      
      gas_consump.append([[LSOA_codes[i],consump[i]] for i in range(len(LSOA_codes))])
      gas_meter.append([[LSOA_codes[i],met_num[i]] for i in range(len(LSOA_codes))])
      gas_non_meter.append([[LSOA_codes[i],non_met_num[i]] for i in range(len(LSOA_codes))])

      gas_consump = process_list(gas_consump)
      gas_meter = process_list(gas_meter)
      gas_non_meter = process_list(gas_non_meter)

    else:
      # Set "LSOA code" as the index of the dataframe
      data = data.set_index("LSOA code")

      # Create gas_consump dictionary
      gas_consump = {}
      for index, row in data.iterrows():
          gas_consump[index] = row["Total \nconsumption\n(kWh)"]

      # Create gas_meter dictionary
      gas_meter = {}
      for index, row in data.iterrows():
          gas_meter[index] = row["Number\nof meters\n"]

      # Create gas_non_meter dictionary
      gas_non_meter = {}
      for index, row in data.iterrows():
          gas_non_meter[index] = row["Number\nof meters\n"]

      #save_pickle_variable(gas_consump=gas_consump, gas_meter = gas_meter, gas_non_meter = gas_non_meter)

    print(f'Gas consumption for year {year} successfully retrieved from Excel')
    return gas_consump, gas_meter, gas_non_meter

def read_from_excel_fuel_poor(year:str = '2020', dict = False):
  
  if year == '2019':
       data = pd.read_excel("./Data/source_data/2021-sub-regional-fuel-poverty-tables.xlsx",sheet_name="Table 3", skiprows=2, skipfooter=9)
     
  else:
       data = pd.read_excel("./Data/source_data/sub-regional-fuel-poverty-2022-tables.xlsx",sheet_name="Table 3", skiprows=2, skipfooter=9)

  if dict == False:
    LSOA_codes = data["LSOA Code"].values
    house_num = data["Number of households"].values
    poor_num = data["Number of households in fuel poverty"].values

      # Replace the 'null' data to zero
    house_num =  [f"'NaN'^^<{XSD_STRING}>" if np.isnan(house_num) else house_num for house_num in house_num]   
    poor_num =  [f"'NaN'^^<{XSD_STRING}>" if np.isnan(poor_num) else poor_num for poor_num in poor_num]  

    house_num_list = []
    fuel_poor = []

    house_num_list.append([[LSOA_codes[i],house_num[i]] for i in range(len(LSOA_codes))])
    fuel_poor.append([[LSOA_codes[i],poor_num[i] / house_num[i]] for i in range(len(LSOA_codes))])

    house_num_list = process_list(house_num_list)
    fuel_poor = process_list(fuel_poor)

  else:
    # Set "LSOA code" as the index of the dataframe
    data = data.set_index("LSOA Code")

    # Create house_num_list dictionary
    house_num_list = {}
    for index, row in data.iterrows():
      house_num_list[index] = row["Number of households"]

      # Create fuel_poor dictionary
    fuel_poor = {}
    for index, row in data.iterrows():
      fuel_poor[index] = row["Number of households in fuel poverty"] / row["Number of households"]
    # save_pickle_variable(house_num_list=house_num_list, fuel_poor = fuel_poor)

  print(f'Fuel poverty for year 2020 successfully retrieved from Excel')
  
  return house_num_list, fuel_poor


