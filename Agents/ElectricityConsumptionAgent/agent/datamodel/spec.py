import pickle
import numpy as np
import pandas as pd
import shapely.speedups
shapely.speedups.enable()
from agent.datamodel.iris import *

### --------------------------------- Spec Vars -------------------------------------------- ###
DEF_NAMESPACE = "ontogasgrid"
LOCAL_KG = "http://localhost:8080/blazegraph"
QUERY_ENDPOINT= UPDATE_ENDPOINT = LOCAL_KG + "/namespace/" + DEF_NAMESPACE + "/sparql"

DB_URL = "jdbc:postgresql:ts_example"
DB_USER = "postgres"
DB_PASSWORD = "postgres"
### ----------------------------------------------------------------------------------------------


### ---------------------- Some useful 'shortcut' functions ----------------------------------- ###
def parse_to_file(query):
    #ONLY for testing purpose
  f = open("demofile3.txt", "w")
  f.write(str(query))
  f.close()

  #open and read the file after the appending:
  f = open("demofile3.txt", "r")

def convert_df(df):
  df.to_csv('C:/Users/jx309/Documents/TheWorldAvatar/Agents/ElectricityConsumptionAgent/df.txt', sep='\t', index=False)

def call_pickle(pathname):
    infile = open(pathname,'rb')
    results = pickle.load(infile)
    infile.close()
    return results

def save_pickle(module,pathname):
    results = module(limit = False)
    outfile = open(pathname,'wb')
    pickle.dump(results,outfile)
    outfile.close()
    return results

def save_pickle_variable(*vars):
    outfile = open(f"./Data/pickle_files/remaining_temp", 'wb')
    pickle.dump(vars, outfile)
    outfile.close()

def save_state():
    data = globals()
    # Remove the variables that are not pickleable
    data = {k: v for k, v in data.items() if isinstance(v, (int, float, str, list, dict, np.ndarray, pd.DataFrame))}
    # Save the data dictionary to a pickle file
    with open('.\Data\pickle_files\progress.pkl', 'wb') as f:
        pickle.dump(data, f)

def resume_state():
    # Load the data dictionary from the pickle file
    with open('.\Data\pickle_files\progress.pkl', 'rb') as f:
        data = pickle.load(f)
    # Update the global namespace with the values in the data dictionary
    globals().update(data)

def resume_data(key, Test = False):
  with open('.\Data\pickle_files\progress.pkl', 'rb') as f:
    my_data = pickle.load(f)
    value = my_data
    #[key]
  
  if Test == True:
    print(f'Value of {key} is,', value)  
    
  return value

def get_all_data(limit):
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
