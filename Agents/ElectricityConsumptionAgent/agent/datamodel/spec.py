import pickle
import numpy as np
import pandas as pd
import inspect
from agent.datamodel.iris import *
from agent.errorhandling.exceptions import *
import datetime

### --------------------------------- Spec Vars -------------------------------------------- ###
DEF_NAMESPACE = "ontogasgrid"
LOCAL_KG = "http://localhost:8080/blazegraph"
QUERY_ENDPOINT= UPDATE_ENDPOINT = LOCAL_KG + "/namespace/" + DEF_NAMESPACE + "/sparql"

DB_URL = "jdbc:postgresql:ts_example"
DB_USER = "postgres"
DB_PASSWORD = "postgres"
### ------------------------------------------------------------------------------------------ ###

### ---------------------------------- Index ------------------------------------------------- ###
monthly_electricity_consumption_2020 = [28.19,26.08,26.82,20.73,20.48,20.36,21.38,21.95,22.39,25.14,25.91,27.89]
monthly_gas_consumption_2020 = [7.88,7.54,7.54,4.86,4.14,3.78,3.78,3.64,4.05,6.09,6.74,8.46]
monthly_gas_consumption_2021 = [9.74 ,7.90 ,7.67 ,6.81 ,5.63 ,3.40 ,3.73 ,3.64 ,4.04 ,5.04 ,7.19 ,7.96]
monthly_elec_consumption_2021 = [19.67 ,19.62 ,19.45 ,18.37 ,18.87 ,20.95 ,18.23 ,18.88 ,17.35 ,19.45 ,19.20 ,19.06]

carbon_intensity_CO2e_elec_2021 = 0.212
carbon_intensity_CO2e_gas_2021 = 0.183
carbon_intensity_CO2_elec_2021 = 0.210
carbon_intensity_CO2_gas_2021 = 0.183
carbon_intensity_CO2e_elec_2020 = 0.233
carbon_intensity_CO2e_gas_2020 = 0.1838
carbon_intensity_CO2_elec_2020 = 0.231
carbon_intensity_CO2_gas_2020 = 0.1835

cost_elec_2020 = 0.172
cost_gas_2020 = 0.0355
cost_elec_2021 = 0.189
cost_gas_2021 = 0.0342

# COP = hp_efficiency * T_H / (T_H - T_C)
hp_efficiency = 0.35
T_H = 45 +273.15

# delta_elec = boiler_efficiency * propotion_heating * delta_gas / COP
boiler_efficiency = 0.8
propotion_heating = 0.9
### ------------------------------------------------------------------------------------------ ###

### ---------------------- Some useful 'shortcut' functions ----------------------------------- ###
def process_list(data):
      # Remove the first bracket using indexing
      data = data[0]
      # Convert the list to a numpy array
      data = np.array(data)
      return data

def parse_to_file(query, filepath = "demofile"):
  '''
  This module is to parse the result into a file, (default as called demofile.txt) so you can visualise it
  could be useful when the terminal contain too much annoying logging message
  '''
  f = open(f'./Data/{filepath}.txt', "w")
  f.write(str(query))
  f.close()

  #open and read the file after the appending:
  f = open(f"./Data/{filepath}.txt", "r")

def read_from_excel_elec(year:str = '2020', dict = False):
    '''
        Return lists of readings from Excel
        
        Arguments:
        year: the number of year of which the data you may want to read
    '''

    try:
            data = pd.read_excel('./Data/LSOA_domestic_elec_2010-20.xlsx', sheet_name=year, skiprows=4)
    except Exception as ex:
            raise InvalidInput("Excel file can not be read -- try fixing by using absolute path") from ex

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
      save_pickle_variable(elec_consump=elec_consump, elec_meter = elec_meter)
    
    
    print(f'Electricity consumption for year {year} successfully retrieved from Excel')
    return elec_consump, elec_meter

def read_from_excel_gas(year:str = '2020', dict = False):
    '''
        Return lists of readings from Excel
        
        Arguments:
        year: the number of year of which the data you may want to read
    '''

    try:
            data = pd.read_excel('./Data/LSOA_domestic_gas_2010-20.xlsx', sheet_name=year, skiprows=4)
    except Exception as ex:
            raise InvalidInput("Excel file can not be read -- try fixing by using absolute path") from ex
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

      save_pickle_variable(gas_consump=gas_consump, gas_meter = gas_meter, gas_non_meter = gas_non_meter)

    print(f'Gas consumption for year {year} successfully retrieved from Excel')
    return gas_consump, gas_meter, gas_non_meter

def read_from_excel_fuel_poor(dict = False):
  data = pd.read_excel("./Data/sub-regional-fuel-poverty-2022-tables.xlsx",sheet_name="Table 3", skiprows=2, skipfooter=9)

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
    save_pickle_variable(house_num_list=house_num_list, fuel_poor = fuel_poor)

  print(f'Fuel poverty for year 2020 successfully retrieved from Excel')
  return house_num_list, fuel_poor

def convert_df(df, filename: str = 'df'):
  '''
  This module is to parse the dataframe into a file called df.txt so you can visualise it
  could be useful when the terminal contain too much annoying logging message
  '''
  df.to_csv(f'./Data/{filename}.txt', sep='\t', index=False)
  print(f'Dataframe successfully printed at ./Data/{filename}.txt')

def call_pickle(pathname):
    '''
  This module is to retrieve the result of the a pickle file under the pathname you specified
  could be useful to retrieve the result of a pickle file
  '''
    try:
        infile = open(pathname,'rb')
        results = pickle.load(infile)
        infile.close()
    except Exception as ex:
        raise InvalidInput("filepath can not be read -- check if the file exist") from ex
        
    return results

def save_pickle(module,pathname):
    '''
  This module is to parse the result of the module into a pickle file under the pathname you specified
  could be useful to save the result of a module

  Note: this function is specific to the use for module. if you want to save the result of 
  a variable, try save_pickle_variable(**kwargs) module (see below)
  '''
    results = module(limit = False)
    outfile = open(pathname,'wb')
    pickle.dump(results,outfile)
    outfile.close()
    return results

def save_pickle_variable(**kwargs):
    '''
    ****************** Use this module along with 'resume_variables' module***************************

      This two modules can save the variable you specified, to the pickle file 
      under the "./Data/temp_Repo/{arg_name} in function {func_name}" filepath, and retrieve the 
      value you want. 
      This module will be particularly useful, when the program take a really long time to run,
      but you are at the developping phase so you have to run the whole script for a lot of times. 
      By saving the intermediate value of variables and retrieve the value by calling resume_variables(**kwargs) (see below), you can resume 
      the value of those variable without the cost of time of running previous programme.

      Here is how to use this module:
      1. you should run this module within another module, i.e. do not run this module in global statue
      2. you should identified which part of the script took too long time, and which you do not want to run again and again
      3. you should know which variables you want to save, this can be easily known by disable the part of the module 
      you do not want. Looking at the following part of the script, if the variable shown as 'undefined' 
      (shown white in VScode), OR looking at the 'PROBLEMS' panel in VScode, then, that's it!

      firstly, put this function just below the scripts you do not want to run again and again
      specify the variable you want to save by putting arguments like var1=var1, var2=var2
      e.g.:

      # Scripts I do not want
      { ... }
      save_pickle_variable(var1=var1, var2=var2)
      # Scripts I do want for testing purpose
      { ... }

      when this module is finished, you can now disable the previous part of script (such as using comma), 
      and resume the value of the variables by calling resume_variables module. 
      Remember that the resume_variables module need to specify the var as arguments as var='var'
      e.g.:

      """
      # Scripts I do not want
      { ... }
      
      """
      var1 = resume_variables(var1='var1')
      var2 = resume_variables(var2='var2')
      # Scripts I do want for testing purpose
      { ... }

    '''
      # Get the name of the calling function
    func_name = inspect.stack()[1][3]
    # Iterate through the arguments
    for arg_name, arg_value in kwargs.items():
        # Save the argument to a pickle file with the name of the argument as the filename
        filename = f"./Data/temp_Repo/{arg_name} in function {func_name}"
        with open(filename, 'wb') as outfile:
            pickle.dump(arg_value, outfile)
        print(f'The values of the {arg_name} have been saved as pickle files "{arg_name} in function {func_name}"')

def resume_variables(**kwargs):
    '''
    ****************** Use this module along with 'save_pickle_variable' module***************************

      This two modules can save the variable you specified, to the pickle file 
      under the "./Data/temp_Repo/{arg_name} in function {func_name}" filepath, and retrieve the 
      value you want. 
      This module will be particularly useful, when the program take a really long time to run,
      but you are at the developping phase so you have to run the whole script for a lot of times. 
      By calling the save_pickle_variable module (see above) to save the intermediate value of variables 
      and retrieve the value by calling resume_variables, you can resume the value of those variable 
      without the cost of time of running previous programme.

      Here is how to use this module:
      1. you should run this module within another module, i.e. do not run this module in global statue
      2. you should identified which part of the script took too long time, and which you do not want to run again and again
      3. you should know which variables you want to save, this can be easily known by disable the part of the module 
      you do not want. Looking at the following part of the script, if the variable shown as 'undefined' 
      (shown white in VScode), OR looking at the 'PROBLEMS' panel in VScode, then, that's it!

      firstly, put this function just below the scripts you do not want to run again and again
      specify the variable you want to save by putting arguments like var1=var1, var2=var2
      e.g.:

      # Scripts I do not want
      { ... }
      save_pickle_variable(var1=var1, var2=var2)
      # Scripts I do want for testing purpose
      { ... }

      when this module is finished, you can now disable the previous part of script (such as using comma), 
      and resume the value of the variables by calling resume_variables module. 
      Remember that the resume_variables module need to specify the var as arguments as var='var'
      e.g.:

      """
      # Scripts I do not want
      { ... }
      
      """
      var1 = resume_variables(var1='var1')
      var2 = resume_variables(var2='var2')
      # Scripts I do want for testing purpose
      { ... }

    '''
    func_name = inspect.stack()[1][3]
    for arg_name, arg_value in kwargs.items():
      # Load the data dictionary from the pickle file
      try:
        with open(f"./Data/temp_Repo/{arg_name} in function {func_name}",'rb') as f:
            data = pickle.load(f)

      except Exception as ex:
        raise InvalidInput("filepath can not be read -- check if the file exist") from ex

      return data

def valid_LSOA_list():

    gas_results, meters_results, non_meters_results = read_from_excel_gas()
    elec_results, elec_meters_results = read_from_excel_elec()
    house_num_result, fuel_poor_result = read_from_excel_fuel_poor()
    shape_result = call_pickle('./Data/pickle_files/shapes_array')
    temp_dict = call_pickle('./Data/temp_Repo/temp_dict in function get_all_data')
    '''
    #Disable them if you may not want the iri
    base_url = 'http://statistics.data.gov.uk/id/statistical-geography/'

    gas_results = np.array([base_url + item for item in gas_results[:, 0]])
    elec_results =  np.array([base_url + item for item in elec_results[:, 0]])
    house_num_result =  np.array([base_url + item for item in house_num_result[:, 0]])
'''
    unique_LSOA_1 = np.unique(gas_results[:, 0])
    unique_LSOA_2 = np.unique(elec_results[:, 0])
    unique_LSOA_3 = np.unique(house_num_result[:, 0])
    unique_LSOA_4 = np.unique(shape_result[:, 0])
    unique_LSOA_5 = np.array(list(temp_dict.keys()))

    for i in range(len(unique_LSOA_4)):
      unique_LSOA_4[i] = unique_LSOA_4[i].replace('http://statistics.data.gov.uk/id/statistical-geography/', '')

    for i in range(len(unique_LSOA_5)):
      unique_LSOA_5[i] = unique_LSOA_5[i].replace('http://statistics.data.gov.uk/id/statistical-geography/', '')

    unique_LSOA = set(unique_LSOA_1).union(unique_LSOA_2, unique_LSOA_3, unique_LSOA_4, unique_LSOA_5)
    unique_LSOA = list(unique_LSOA)

    print(len(unique_LSOA))
    print(len(unique_LSOA_1))
    print(len(unique_LSOA_2))
    print(len(unique_LSOA_3))
    print(len(unique_LSOA_4))
    print(len(unique_LSOA_5))
    save_pickle_variable(unique_LSOA = unique_LSOA)

def reformat_dates(input_dict):
    # Create a new dictionary to store the reformatted dates
    output_dict = {}
    
    # Iterate over the keys and values in the input dictionary
    for key, value in input_dict.items():
        # Check if the key is in the 'YYYY-MM-DDTHH:MM:SS' format
        if len(key) == 19:
            # Parse the date and time
            dt = datetime.datetime.strptime(key, '%Y-%m-%dT%H:%M:%S')
            # Reformat the date and time as 'YYYY-MM-DDTHH:MM:SS.000Z'
            key = dt.strftime('%Y-%m-%dT%H:%M:%S.000Z')
        # Add the key-value pair to the output dictionary
        output_dict[key] = value

    return output_dict

def get_all_data(limit = False):
    '''
  This module provide a 'shortcut' method to retrieve all the data required 
  for the project. Which returns a DataFrame looks like this:
    LSOA_code  ons_shape   Electricity_consump   Electricity_meter  Electricty_cosumption_per_household  Gas_consump   Gas_meter   Gas_nonmeter  Gas_consumption_per_household   FuelPoor_%   Household_num   temp
  0
  1
  2
  ...
  and this DataFrame will be stored as a pickle file under the ./Data folder
  so that this data can be called much much much more quick than query from the knowledge graph
    '''
    '''
    #Read all the data from pickle files
    filename = './Data/pickle_files/temp_array'
    gas_filename = './Data/pickle_files/gas_array'
    meters_filename = './Data/pickle_files/meters_array'
    elec_filename = './Data/pickle_files/elec_array'
    elec_meters_filename = './Data/pickle_files/elec_meters_array'
    fuel_poor_filename = './Data/pickle_files/fuel_poor'
    shapes_filename = './Data/pickle_files/shapes_array'

    all_results = call_pickle(filename)
    gas_results = call_pickle(gas_filename)
    meters_results = call_pickle(meters_filename)
    elec_results = call_pickle(elec_filename)
    elec_meters_results = call_pickle(elec_meters_filename)
    fuel_poor_results = call_pickle(fuel_poor_filename)
    LSOA_shapes = call_pickle(shapes_filename)

    #Get unique LSOA code
    unique_LSOA = np.unique(all_results[:, 0])

    #Trim the irrelavent data
    all_results = [[row[i] for i in range(len(row)) if i != 2] for row in all_results]

    # make a dict for temp array
    # So you may search a temperature value, by: temp_dict[LSOA_code][Month][Temperature_type]
    temp_dict = {}
    for entry in all_results:
      a , b, c, d = entry
      if a not in temp_dict:
            temp_dict[a] = {}
      if b not in temp_dict[a]:
            temp_dict[a][b] = {}
      temp_dict[a][b][c] = float(d)

    # Iterate over the top-level keys in the dictionary
    for key_1, value_1 in temp_dict.items():
      # Iterate over the second-level keys in the dictionary
      for key_2, value_2 in value_1.items():
        # Iterate over the third-level keys in the dictionary
        for key_3, value_3 in value_2.items():
          # Round the value to one decimal place and assign it back to the dictionary
          temp_dict[key_1][key_2][key_3] = round(value_3, 3)

    # make dicts for all other arrays
    non_meters_results = [[row[i] for i in range(len(row)) if i != 1] for row in meters_results]
    meters_results = [[row[i] for i in range(len(row)) if i != 2] for row in meters_results]
    fuel_poor_propotion_result = [[row[i] for i in range(len(row)) if i != 2] for row in fuel_poor_results]
    num_household_result = [[row[i] for i in range(len(row)) if i != 1] for row in fuel_poor_results]

################### Temperature one is bit special #####################################
    all_results = call_pickle('./Data/pickle_files/temp_array')
    
    #Trim the irrelavent data
    all_results = [[row[i] for i in range(len(row)) if i != 2] for row in all_results]

    # make a dict for temp array
    # So you may search a temperature value, by: temp_dict[LSOA_code][Month][Temperature_type]
    temp_dict = {}
    for entry in all_results:
      a , b, c, d = entry
      if a not in temp_dict:
            temp_dict[a] = {}
      if b not in temp_dict[a]:
            temp_dict[a][b] = {}
      temp_dict[a][b][c] = float(d)

    # Iterate over the top-level keys in the dictionary
    for key_1, value_1 in temp_dict.items():
      # Iterate over the second-level keys in the dictionary
      for key_2, value_2 in value_1.items():
        # Iterate over the third-level keys in the dictionary
        for key_3, value_3 in value_2.items():
          # Round the value to one decimal place and assign it back to the dictionary
          temp_dict[key_1][key_2][key_3] = round(value_3, 3)

######################################################################################################
    '''
    valid_LSOA_list()
    temp_dict = call_pickle('./Data/temp_Repo/temp_dict in function get_all_data')
    gas_results = call_pickle('./Data/temp_Repo/gas_consump in function read_from_excel_gas')
    meters_results = call_pickle('./Data/temp_Repo/gas_meter in function read_from_excel_gas')
    non_meters_results = call_pickle('./Data/temp_Repo/gas_non_meter in function read_from_excel_gas')
    elec_results = call_pickle('./Data/temp_Repo/elec_consump in function read_from_excel_elec')
    elec_meters_results = call_pickle('./Data/temp_Repo/elec_meter in function read_from_excel_elec')
    fuel_poor_propotion_result = call_pickle('./Data/temp_Repo/fuel_poor in function read_from_excel_fuel_poor')
    num_household_result = call_pickle('./Data/temp_Repo/house_num_list in function read_from_excel_fuel_poor')
    LSOA_shapes = call_pickle('./Data/pickle_files/shapes_array')
    LSOA_shapes = dict(LSOA_shapes) 
    unique_LSOA = call_pickle('./Data/temp_Repo/unique_LSOA in function valid_LSOA_list')

    # Remove some irrelavent data
    LSOA_shapes = {key.replace('http://statistics.data.gov.uk/id/statistical-geography/', ''): value for key, value in LSOA_shapes.items()}
    temp_dict = {key.replace('http://statistics.data.gov.uk/id/statistical-geography/', ''): value for key, value in  temp_dict.items()}
    # Get correct format of the datetime
    for key_1, value_1 in temp_dict.items():
        temp_dict[key_1] = reformat_dates(value_1)
    # Making the DataFrame
    df = pd.DataFrame(unique_LSOA, columns=['LSOA_code'])
    df['ons_shape'] = df['LSOA_code'].apply(lambda x: LSOA_shapes.get(x, np.nan))
    df['Electricity_consump'] = df['LSOA_code'].apply(lambda x: round(float(elec_results.get(x, np.nan)),3))
    df['Electricity_meter'] = df['LSOA_code'].apply(lambda x: float(elec_meters_results.get(x, np.nan)))
    df['Electricty_cosumption_per_household'] = df['Electricity_consump'].to_numpy() /df['Electricity_meter'].to_numpy()
    df['Gas_consump'] = df['LSOA_code'].apply(lambda x: round(float(gas_results.get(x, np.nan)),3))
    df['Gas_meter'] = df['LSOA_code'].apply(lambda x: float(meters_results.get(x, np.nan)))
    df['Gas_nonmeter'] = df['LSOA_code'].apply(lambda x: float(non_meters_results.get(x, np.nan)))
    df['Gas_consumption_per_household'] = df['Gas_consump'].to_numpy() /df['Gas_meter'].to_numpy()
    df['FuelPoor_%'] = df['LSOA_code'].apply(lambda x: round(float(fuel_poor_propotion_result.get(x, np.nan)),3))
    df['Household_num'] = df['LSOA_code'].apply(lambda x: float(num_household_result.get(x, np.nan)))
    df['temp'] = df['LSOA_code'].apply(lambda x: temp_dict.get(x, np.nan))

    save_pickle_variable(df = df)
    print(f"All the data for {len(df)} LSOA areas has been stored to pickle file")
    convert_df(df)
    return df

#save_pickle(read_the_temperature,"./Data/pickle_files/temp_all_results")
#get_all_data(limit = False)
#valid_LSOA_list()
get_all_data(limit=False)