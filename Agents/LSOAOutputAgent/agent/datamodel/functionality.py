################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to provide some shortcut functions for other module

import agentlogging
from agent.errorhandling.exceptions import *
from agent.utils.env_configs import YEAR
from agent.utils.stack_configs import (QUERY_ENDPOINT, UPDATE_ENDPOINT)

import matplotlib.pyplot as plt
import scipy.stats as st
import matplotlib.colors as cl 
import matplotlib.cm as cm
import pandas as pd
import numpy as np
import copy
import pickle


# Initialise logger
logger = agentlogging.get_logger("prod")

# ------------------------ Some 'shortcut' functions ----------------------- #
# data calculation ----------------------- #
def monthly_disaggregation(df_in: pd.DataFrame, monthly_ref: list, annual: bool = False):
    '''
    To calculate the monthly distribution, based on the whole year data from df, and reference monthly distribution
    from monthly_ref
    Note: In many cases, monthly disaggregation can be done before or after a variable is calculated, 
    such as cost, emission, you can calculate a annual one and disaggregate into monthly data
    that won't affect the result
    Arguments:
    df: two-column data frame which MUST have the data to disaggregate placed at the second column
    (i.e. at position [1])
    monthly_ref: reference monthly distribution.
    annual: if True, the second column will include the annual value
            if False, only monthly value will be returned
    '''
    global months
    df = copy.deepcopy(df_in)
    months = ['January','February','March','April','May','June','July','August','September','October','November','December']
    total = sum(monthly_ref)
    for i in range(12):
        df[f'{months[i]}'] = df[df.columns[1]] * monthly_ref[i] / total
    if annual == False:
        df = drop_column(df,[1])
    return df

def T_from_COP(COP):
    '''
    Return temperature based on a given COP
    '''
    T = 45 - ((45+273.15)/(COP/0.35))

    return T

# df data treatment ----------------------- #
def convert_to_int(val):
    '''
    get rid of all Nan data, and convert the data format to int
    '''
    if val == 'NaN':
        return np.nan
    else:
        return int(val)

def convert_to_float(val):
    '''
    get rid of all Nan data, and convert the data format to Float
    '''
    if val == 'NaN':
        return np.nan
    else:
        val = float(val)
        val = round(val,3)
        return val

def convert_to_zero(val):
    '''
    get rid of all Nan data, and convert to 0
    '''
    if val == 'NaN':
        return 0
    else:
        return int(val)

def save_figures(arg_name):
    '''
    Save the figure under ./'figure_output folder, both png and pdf format
    Argument:
    arg_name: the name of this figure file
    '''
    plt.savefig('figure_output/'+f"{arg_name}".lower()+'.png',dpi=200) 
    plt.savefig('figure_output/'+f"{arg_name}".lower()+'.pdf')

def remove_NaN(df_in: pd.DataFrame):
    '''
    Remove all the row which contain the value 'NaN' 
    iteration start from second column and above, as the first column is always LSOA code for identification
    i.e., can't be NaN
    
    Note: This function is only used for data processing as this will delete the row
    '''
    df = copy.deepcopy(df_in)

    nan_rows = df[df.isnull().any(1)].index
    deleted_rows = []
    for i in nan_rows:
        row = df.iloc[i, :]
        first_col_value = row[df.columns[0]]
        nan_cols = row.isnull().index[row.isnull() == True].tolist()
        deleted_rows.append((first_col_value, nan_cols))

    df = df.drop(nan_rows)

    for row in deleted_rows:
        logger.info(f"LSOA area {row[0]} was deleted due to NaN values in columns {row[1]}")
        #print(f"LSOA area {row[0]} was deleted due to NaN values in columns {row[1]}")

    df.reset_index(drop=True, inplace=True)
    return df

def drop_column(df, indices):
    '''
    drop multiple columns using indices, can use index numbers or names
    '''
    if all(isinstance(index, int) for index in indices):
        df = df.drop(columns=[df.columns[index] for index in indices])
    else:
        df = df.drop(columns=indices)
    return df

def select_column(df, indices):
    '''
    select multiple columns using indices, can use index numbers or names
    '''
    if all(isinstance(index, int) for index in indices):
        df = df[[df.columns[index] for index in indices]]
    else:
        df = df[indices]
    return df

def convert_to_tensor(input, monthly_ref = None, LSOA_index_id = None, year:str =YEAR):
    '''
    This module is to create tensor, specific for the type of data like:
[LSOA_1:[Jan: [tasmin: ,  tas:   , tasmax:  ],
        Feb: [tasmin: ,  tas:   , tasmax:  ],
        Mar: [tasmin: ,  tas:   , tasmax:  ],
                .........
        Dec: [tasmin: ,  tas:   , tasmax:  ]],

LSOA_2:[Jan: [tasmin: ,  tas:   , tasmax:  ],
        Feb: [tasmin: ,  tas:   , tasmax:  ],
        Mar: [tasmin: ,  tas:   , tasmax:  ],
                .........
        Dec: [tasmin: ,  tas:   , tasmax:  ]],
                .........
        ]]] 
    i.e. for all data relating to temperature, such as COP, change of gas, electricity etc
    convert into tensor will make it faster to calculate
    Beside the tensor, this function will return an LSOA_index as well, which using row index as 
    key, LSOA code as value, for the later match up. This index is a dict like:
    {LSOA_1:0, LSOA_2:1, LSOA_3:2, ...}
    i.e, {LOSA_code: tenosr index}
    Arguments:
    input: can be dict or df. 
        If it's a dict, which should have structure like:
        dict = { LSOA_1: 
                    {'2020-01-01 12:00:00:
                    {'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin':0,\
                        'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas':1,\
                        'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax':2,\}
                    {'2020-02-01 12:00:00:
                    {'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin':0,\
                        'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas':1,\
                        'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax':2,\}}
                        .......
                LSOA_2: 
                    {'2020-01-01 12:00:00:
                    {'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin':0,\
                        'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas':1,\
                        'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax':2,\}
                    {'2020-02-01 12:00:00:
                    {'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin':0,\
                        'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas':1,\
                        'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax':2,\}}
                        .......
                }
    if it is a df, should have a structure like:
        LSOA_code   date                      var                                                                 temp
    0   LSOA_1      '2020-01-01 12:00:00      'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin'    3
        ...          ...                      'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas'       5
        ...          ...                      'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax'    7
                
                    '2020-02-01 12:00:00:     'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin'    3
        ...          ...                      'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas'       5
        ...          ...                      'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax'    7
                    .......
            
    1   LSOA_2      '2020-01-01 12:00:00      'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin'    3
        ...          ...                      'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas'       5
        ...          ...                      'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax'    7
                
                    '2020-02-01 12:00:00:     'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin'    3
        ...          ...                      'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas'       5
        ...          ...                      'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax'    7
                    .......     
    
    or df is not relating to temperature, which can be this shape:
                LSOA_code   elec_consump
        0       LSOA_1      100
        1       LSOA_2      100
        2       LSOA_3      100
        ....
    or:
                LSOA_code   Jan   Feb   March ...
        0       LSOA_1      100   ...
        1       LSOA_2      100   ...
        2       LSOA_3      100   ...
        ....
    if the input df is in former shape, it will firstly disaggrate the value into monthly distribution (like the second shape), 
    and then parse the result having the same shape as np.zeros((3, len(df), 12)). 
    Note that in this senario, the monthly data will be distributed into the Axis-2 as which have 12 positions, 
    data for each LSOA will be distributed into Axis-1 based on LSOA_index (and need to be provided in this case)
    however, the Axis-0 will have the same value, i.e. result_tensor[0][:][:] = result_tensor[1][:][:] = result_tensor[2][:][:]. 
    As this senario only apply to those data are not temperature related, making them into this shape is just for the sake of calculation.
    
    monthly_ref: ONLY NEED when input is a df and have only two column, and this monthly_ref is been used 
        to disaggregate into monthly data
    LSOA_index_id: ONLY NEED when input is a df, and are not-temperature related (such as consumption/usage)
                   This should be returned when initially call this function to convert temperature tensor, 
                   and can be used from their directly.
    ** The logic is, for those data are not temperature related to perform this operation, they ultimately 
    are used to serve data that DO is temperature related, for example, convert electricity consumption into tensor,
    to serve delta_electricity_consumption_tensor, to get remaining_electricity_consumption_tensor. so they need
    to know which index is located in which LSOA area, so, this LSOA_index_id is needed for match up the date **
    '''
    
    if type(input) == pd.DataFrame:
        df = input
        if df.shape[1] == 4:
            # Create dicts for assigning index
            t_dict = {'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin':0,\
                'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas':1,\
                'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax':2}
            date_dict = generate_time_dict(year)

            # Parse the df results
            all_results = df.values

            # Get unique LSOA keys, generate LSOA_index dict
            LSOA_index = {}
            lsoa_array = np.unique(all_results[:,0]) 
            for i in range(len(lsoa_array)):
                LSOA_index[lsoa_array[i]] = i

            # Allocate the data into tensor
            results_tensor = np.zeros((3,len(LSOA_index),12)) 
            for j in range(len(all_results[:, 0])):
                t_ind = t_dict[all_results[j, 2]]
                d_ind = date_dict[all_results[j, 1]]
                lsoa_ind = LSOA_index[all_results[j, 0]]
                # allocating a value (last index is value)
                results_tensor[t_ind, lsoa_ind, d_ind] = all_results[j, -1]
        else:
            # if annual data, disaggrate it
            if df.shape[1] == 2:
                df = monthly_disaggregation(df, monthly_ref)
            
            # initialize a results_tensor = np.zeros((3, length of row, 12))
            results_tensor = np.full(shape=(3, len(LSOA_index_id), 12), fill_value=np.nan, dtype=float) 

            # Iterate over the indices and LSOA codes in the dictionary
            for key, value in LSOA_index_id.items():
                row = df.loc[df[df.columns[0]] == key]
                if not row.empty:
                    results_tensor[:, value, :] = row.iloc[:, 1:].values

            # No LSOA_index in this case
            LSOA_index = None

    elif type(input) == dict:
        inputdict = input
        LSOA_index = {key:i for i, key in enumerate(inputdict)}
        # initialize a results_tensor= np.zeros((3, length of row,12))
        results_tensor = np.zeros((3, len(inputdict), 12))
        for h, data in inputdict.items():
            for j, value in data.items():
                for k, v in value.items():
                    lsoa_index = LSOA_index.get(h)
                    t_index = t_dict[k]
                    date_index = date_dict[j]
                    results_tensor[t_index][lsoa_index][date_index] = v

    return LSOA_index, results_tensor

def compare_tensor(array_temp, unique_LSOA, *array_to_compare):
    '''
    Compare two array see if they have nan value due to comprimisation from fitting to temperature array
    can accept multiple array_to_compare
    '''
    # Create mask if applicable
    mask = np.isnan(np.hstack(array_to_compare)).any(axis=(0,2))
    nan_indices = set(np.where(mask)[0])

    # modify lsoa index dict
    unique_LSOA = {k: v for k, v in unique_LSOA.items() if v not in nan_indices}
    unique_LSOA = {k: i for i, (k, v) in enumerate(unique_LSOA.items())}
    
    # modify array to compare
    array_to_compare = np.array([np.delete(arr, np.where(mask), axis=1) for arr in array_to_compare])
    
    # modify temperature array
    array_temp = np.delete(array_temp, np.where(mask), axis=1)
    
    return array_temp, unique_LSOA, *array_to_compare

def tensor_to_df(tensor, lsoa_index, temp_var = None, annual = False):
    '''
    Convert a given tensor (shape of (3,xxx,12)) into dataframe
    Arguments:
        tensor: to be converted
        lsoa_index: use to locate the lsoa code, which will be used for first column in df
        temp_var: 'tasmax'/'tas'/'tasmin' use to select the correct set of data
                   based on max / mean / min temperature
        annual: if True, annual data will be provided on the second column along with the rest for the monthly data
                if False, only monthly data will be provided
    '''
    #  shape reduce to (xxx,12)
    if temp_var == 'tasmin':
        result_arr = tensor[0]
    if temp_var == 'tas':
        result_arr = tensor[1]
    if temp_var == 'tasmax':
        result_arr = tensor[2]
    
    # create a df
    months = ['January', 'February', 'March', 'April', 'May', 'June', \
          'July', 'August', 'September', 'October', 'November', 'December']

    df = pd.DataFrame(result_arr, columns=months)
    df.insert(0, 'LSOA_code', list(lsoa_index.keys()))

    #  shape reduce to (xxx)
    if annual == True:
        result_arr_annual = np.sum(result_arr, axis=1)
        df.insert(1, 'annual', result_arr_annual)

    return df

def remove_unlocated_data(df):
    # Create a boolean mask indicating which rows contain 'Unallocated' in the first column
    mask = df[df.columns[0]].str.contains('Unallocated')
    # Use the mask to index the DataFrame and drop the rows
    df = df[~mask]
    # Reset the indices of the DataFrame
    df = df.reset_index(drop=True)

    return df

# Figure generation ----------------------- #
def data_treatment(df, arg_name, arg_value_in):
        '''
    This function is created to append a new column in the existing dataframe (at last position)
    and creat a np.darray contain all the new data, excluded nan data, to be used
    to set the scale of colorbar

    Arguments:
                *** Please use this function with plot_geodistribution***
    df: dataframe to be appended
    arg_name: the name you want to give to this new column
    arg_value: two column pd.DataFrame, which MUST have a column with name 's' which contain LSOA code for data identification
            and another column for data
    '''
        df_copy = copy.deepcopy(df)
        arg_value = copy.deepcopy(arg_value_in)
        for col in arg_value.columns[1:]: 
            # Create a dict, which will make the data matching soooooo fast 
            dictionary = {row[arg_value.columns[0]]: row[col] for _, row in arg_value.iterrows()}

            if arg_name == False:
                # Appended the variable data to the df_geo
                df_copy = df_copy.assign(**{f"{col}": np.nan})
                df_copy[f"{col}"] = df_copy[df_copy.columns[0]].apply(lambda x: dictionary.get(x, np.nan))
            else:
                df_copy = df_copy.assign(**{f"{arg_name}": np.nan})
                df_copy[f"{arg_name}"] = df_copy[df_copy.columns[0]].apply(lambda x: dictionary.get(x, np.nan))
                
        # ------------ This val_values is normally used for colorbar plot ------------------- #
        try:
            # Specify the value to plot
            val_values = df_copy[f"{arg_name}"].values
        except:
            # If can not found, default as the last column
            val_values = df_copy.iloc[:, -1].values
            
        # Check if this array is applicable for normalization
        if isinstance(val_values[0], (int, float)):
            # Create a boolean mask indicating which elements are NaN values
            mask = np.isnan(val_values)
            # Select only the non-NaN values from the array
            val_values = val_values[~mask]
        else:
            # if still can't find (or data type not applicable) then give a none
            val_values = np.empty(df_copy.shape[0])
            val_values[:] = 0
            print('val_values not applicable to generate an non-nan array, return zero array instead')


        return df_copy, val_values

def normalization(val_values, cb_scale):
    '''
    Provide normalization for color bar
    Arguments:
                *** Please use this function with plot_geodistribution***
    val_values: NDArray which have NO nan data
    cb_scale: to adjust the scale of colorbar, sometimes the figure is ugly because the max and min value are too close
                in a nutshell, the bigger the cb_scale, the larger the gap between max and min for colorbar
                I suggest start with 0, and then try 1, 1.5 maybe. Need a few try and error.
    '''
    iqr = st.iqr(val_values)
    q1,q3 = st.mstats.idealfourths(val_values)
    bottom = q1 - cb_scale * iqr
    top = q3 + cb_scale * iqr
    divnorm = cl.Normalize(vmin=bottom, vmax=top)

    return divnorm

def create_color_bar(color_theme: str, divnorm, label: str, axs, cax1, val_df: pd.DataFrame):
    '''
    Provide color bar
    Arguments:
                *** Please use this function with plot_geodistribution***
    color_theme: color theme of the map
    divnorm: normalization result returned from function normalization(val_values, cb_scale)
    label: the label of the color bar (or say legend)
    axs: define the axs, should be consistent with the previous plt
    cax1: define the cax1, should be consistent with the previous plt
    val_df: a pd.Dataframe (should have one column of data ONLY so which can be set as array)
    '''
    # Create a colorbar for the plot
    scalar_mappable = cm.ScalarMappable(cmap=color_theme, norm=divnorm)
    scalar_mappable.set_array(val_df)
    colorbar = plt.colorbar(scalar_mappable, ax=axs, cax=cax1)
    # Set the label for the colorbar
    colorbar.set_label(label)

# Others ----------------------- #
def add_prefix(x, prefix):
    return prefix + str(x)

def generate_time_dict(year: str):

      time_dict = {f'{year}-01-01 12:00:00':0,\
             f'{year}-02-01 12:00:00':1,\
             f'{year}-03-01 12:00:00':2,\
             f'{year}-04-01 12:00:00':3,\
             f'{year}-05-01 12:00:00':4,\
             f'{year}-06-01 12:00:00':5,\
             f'{year}-07-01 12:00:00':6,\
             f'{year}-08-01 12:00:00':7,\
             f'{year}-09-01 12:00:00':8,\
             f'{year}-10-01 12:00:00':9,\
             f'{year}-11-01 12:00:00':10,\
             f'{year}-12-01 12:00:00':11}
            
      return time_dict

def get_median(df_in:pd.DataFrame, row_name: str = '0'):
    '''
    for given dataframes, calculate the median value of each column, and return one dataframe
    which contain only one row data of the median value result. The row name can be customised 
    and the column name remain unchanged.
    Arguments:
    df: dataframe to be calculated
        Note that for a column in df that contain strings, i.e. this column is not median-able, therefore 
        this column will be automatically excluded in the return df
        (This is a bonus! No need to remove the 'LSOA_code' column in advance! :)
    row_name: row name you may want to customise, default as 'o' 
    '''
    df = copy.deepcopy(df_in)
    medians = df.median()
    median_df = medians.to_frame().transpose()
    median_df.index = [row_name]

    return median_df

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

def get_key(val, my_dict):
    for key, value in my_dict.items():
        if val == value:
            return key
    return None

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

def convert_df(df, filename: str = 'df'):
  '''
  This module is to parse the dataframe into a file called df.txt so you can visualise it
  could be useful when the terminal contain too much annoying logging message
  '''
  df.to_csv(f'./Data/{filename}.txt', sep='\t', index=False)
  print(f'Dataframe successfully printed at ./Data/{filename}.txt')
