import pickle
import numpy as np
import pandas as pd
import shapely.speedups
import inspect
shapely.speedups.enable()
from agent.datamodel.iris import *
from agent.errorhandling.exceptions import *

### --------------------------------- Spec Vars -------------------------------------------- ###
DEF_NAMESPACE = "ontogasgrid"
LOCAL_KG = "http://localhost:8080/blazegraph"
QUERY_ENDPOINT= UPDATE_ENDPOINT = LOCAL_KG + "/namespace/" + DEF_NAMESPACE + "/sparql"

DB_URL = "jdbc:postgresql:ts_example"
DB_USER = "postgres"
DB_PASSWORD = "postgres"
### ----------------------------------------------------------------------------------------------


### ---------------------- Some useful 'shortcut' functions ----------------------------------- ###
def parse_to_file(query, filepath = "demofile"):
  '''
  This module is to parse the result into a file, (default as called demofile.txt) so you can visualise it
  could be useful when the terminal contain too much annoying logging message
  '''
  f = open(f'{filepath}.txt', "w")
  f.write(str(query))
  f.close()

  #open and read the file after the appending:
  f = open(f"{filepath}.txt", "r")

def read_from_excel_elec(year:str = '2020'):
    '''
        Return lists of readings from Excel
        
        Arguments:
        year: the number of year of which the data you may want to read
    '''

    try:
            data = pd.read_excel('./Data/LSOA_domestic_elec_2010-20.xlsx', sheet_name=year, skiprows=4)
    except Exception as ex:
            raise InvalidInput("Excel file can not be read -- try fixing by using absolute path") from ex

    LSOA_codes = data["LSOA code"].values
    met_num = data["Number\nof meters\n"].values
    consump = data["Total \nconsumption\n(kWh)"].values

    elec_consump = []
    elec_meter = []
    
    # Replace nan values with zeros using a list comprehension
    met_num =  [0 if np.isnan(met_num) else met_num for met_num in met_num]  
    consump =  [0 if np.isnan(consump) else consump for consump in consump]  
    
    elec_consump.append([[LSOA_codes[i],consump[i]] for i in range(len(LSOA_codes))])
    elec_meter.append([[LSOA_codes[i],met_num[i]] for i in range(len(LSOA_codes))])

    print(f'Electricity consumption for year {year} successfully retrieved from Excel')
    return elec_consump, elec_meter

def read_from_excel_gas(year:str = '2020'):
    '''
        Return lists of readings from Excel
        
        Arguments:
        year: the number of year of which the data you may want to read
    '''

    try:
            data = pd.read_excel('./Data/LSOA_domestic_gas_2010-20.xlsx', sheet_name=year, skiprows=4)
    except Exception as ex:
            raise InvalidInput("Excel file can not be read -- try fixing by using absolute path") from ex

    LSOA_codes = data["LSOA code"].values
    met_num = data["Number\nof meters\n"].values
    non_met_num = data['Number of\nnon-consuming meters'].values
    consump = data["Total \nconsumption\n(kWh)"].values
    'Number of\nnon-consuming meters'

    gas_consump = []
    gas_meter = []
    gas_non_meter = []
    

    # Replace the 'null' data to zero
    met_num =  [0 if np.isnan(met_num) else met_num for met_num in met_num]  
    non_met_num =  [0 if np.isnan(non_met_num) else non_met_num for non_met_num in non_met_num]  
    consump =  [0 if np.isnan(consump) else consump for consump in consump]  
    
    gas_consump.append([[LSOA_codes[i],consump[i]] for i in range(len(LSOA_codes))])
    gas_meter.append([[LSOA_codes[i],met_num[i]] for i in range(len(LSOA_codes))])
    gas_non_meter.append([[LSOA_codes[i],non_met_num[i]] for i in range(len(LSOA_codes))])

    print(f'Gas consumption for year {year} successfully retrieved from Excel')
    return gas_consump, gas_meter, gas_non_meter

def read_from_excel_fuel_poor():
  data = pd.read_excel(
    "./Data/sub-regional-fuel-poverty-2022-tables.xlsx",
    sheet_name="Table 3",
    skiprows=2,
    skipfooter=9)

  LSOA_codes = data["LSOA Code"].values
  house_num = data["Number of households"].values
  poor_num = data["Number of households in fuel poverty"].values

    # Replace the 'null' data to zero
  house_num =  [0 if np.isnan(house_num) else house_num for house_num in house_num]  
  poor_num =  [0 if np.isnan(poor_num) else poor_num for poor_num in poor_num]  

  house_num_list = []
  fuel_poor = []

  house_num_list.append([[LSOA_codes[i],house_num[i]] for i in range(len(LSOA_codes))])
  fuel_poor.append([[LSOA_codes[i],poor_num[i] / house_num[i]] for i in range(len(LSOA_codes))])

  print(f'Fuel poverty for year 2020 successfully retrieved from Excel')
  return house_num_list, fuel_poor

def convert_df(df):
  '''
  This module is to parse the dataframe into a file called df.txt so you can visualise it
  could be useful when the terminal contain too much annoying logging message
  '''
  df.to_csv('C:/Users/jx309/Documents/TheWorldAvatar/Agents/ElectricityConsumptionAgent/df.txt', sep='\t', index=False)

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

    def process_list(data):
      # Remove the first bracket using indexing
      data = data[0]
      # Convert the list to a numpy array
      data = np.array(data)
      return data

    gas_results, meters_results, non_meters_results = read_from_excel_gas()
    elec_results, elec_meters_results = read_from_excel_elec()
    house_num_result, fuel_poor_result = read_from_excel_fuel_poor()
    shape_result = call_pickle('./Data/pickle_files/shapes_array')

    elec_results = process_list(elec_results)
    gas_results = process_list(gas_results)
    house_num_result = process_list(house_num_result)
    

    unique_LSOA_1 = np.unique(gas_results[:, 0])
    unique_LSOA_2 = np.unique(elec_results[:, 0])
    unique_LSOA_3 = np.unique(house_num_result[:, 0])
    unique_LSOA_4 = np.unique(shape_result[:, 0])

    unique_LSOA = set(unique_LSOA_1).union(unique_LSOA_2, unique_LSOA_3, unique_LSOA_4)
    unique_LSOA = list(unique_LSOA)

    print(len(unique_LSOA))
    print(len(unique_LSOA_1))
    print(len(unique_LSOA_2))
    print(len(unique_LSOA_3))
    print(len(unique_LSOA_4))
    save_pickle_variable(unique_LSOA = unique_LSOA)

def get_all_data(limit = False):
    '''
  This module provide a 'shortcut' method to retrieve all the data required 
  for the project. Which returns a DataFrame looks like this:
    LSOA_code  ons_shape   Electricity_consump   Electricity_meter   Gas_consump   Gas_meter   Gas_nonmeter   FuelPoor_%   Household_num   temp
  0
  1
  2
  ...
  and this DataFrame will be stored as a pickle file under the ./Data folder
  so that this data can be called much much much more quick than query from the knowledge graph
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

    gas_results = dict(gas_results)
    meters_results = dict(meters_results )
    non_meters_results = dict(non_meters_results ) 
    elec_results = dict(elec_results) 
    elec_meters_results = dict(elec_meters_results) 
    fuel_poor_propotion_result = dict(fuel_poor_propotion_result) 
    num_household_result = dict(num_household_result ) 
    LSOA_shapes = dict(LSOA_shapes) 

    # Remove some irrelavent data
    elec_results = {key: float(value.replace('http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/', '')) if isinstance(value, str) else value for key, value in elec_results.items()}
    elec_meters_results = {key: float(value.replace('http://www.theworldavatar.com/kb/ontogasgrid/offtakes_abox/', '')) if isinstance(value, str) else value for key, value in elec_meters_results.items()}

    # Making the DataFrame
    df = pd.DataFrame(unique_LSOA, columns=['LSOA_code'])
    df['ons_shape'] = df['LSOA_code'].apply(lambda x: LSOA_shapes[x])
    df['Electricity_consump'] = df['LSOA_code'].apply(lambda x: round(float(elec_results[x]),3))
    df['Electricity_meter'] = df['LSOA_code'].apply(lambda x: float(elec_meters_results[x]))
    df['Gas_consump'] = df['LSOA_code'].apply(lambda x: round(float(gas_results[x])))
    df['Gas_meter'] = df['LSOA_code'].apply(lambda x: float(meters_results[x]))
    df['Gas_nonmeter'] = df['LSOA_code'].apply(lambda x: float(non_meters_results[x]))
    df['FuelPoor_%'] = df['LSOA_code'].apply(lambda x: round(float(fuel_poor_propotion_result[x])))
    df['Household_num'] = df['LSOA_code'].apply(lambda x: float(num_household_result[x]))
    df['temp'] = df['LSOA_code'].apply(lambda x: temp_dict[x])

    convert_df(df)
    return df

#save_pickle(read_the_temperature,"./Data/pickle_files/temp_all_results")
#get_all_data(limit = False)
valid_LSOA_list()