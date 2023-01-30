################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to provide some shortcut functions for other module

import agentlogging
from agent.errorhandling.exceptions import *

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

def save_figures(arg_name):
    '''
    Save the figure under ./'figure_output folder, both png and pdf format
    Argument:
    arg_name: the name of this figure file
    '''
    plt.savefig('figure_output/'+f"{arg_name}".lower()+'.png',dpi=200) 
    plt.savefig('figure_output/'+f"{arg_name}".lower()+'.pdf')

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

def remove_NaN(df: pd.DataFrame):
    '''
    Remove all the row which contain the value 'NaN' 
    iteration start from second column and above, as the first column is always LSOA code for identification
    i.e., can't be NaN
    
    Note: This function is only used for data processing as this will delete the row
    '''
    df = df[df.iloc[:, 1:].notnull().all(axis=1)]
    df = df.reset_index(drop=True)
    return df

def drop_column(df,index):
    '''
    drop a column using index, can using index number or name
    '''
    if type(index) == int:
        df = df.drop(columns=df.columns[index])
    else:
        df = df.drop(columns=[index])
    return df

def convert_to_tensor(input, monthly_ref = None, LSOA_index_id = None):
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
    key, LSOA code as value, for the later match up.
    Arguments:
    input: can be dict or df. If it's a dict, which should have structure like:
    dict = { LSOA_1: 
                {'2020-01-01T12:00:00.000Z':
                {'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin':0,\
                    'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas':1,\
                    'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax':2,\}
                {'2020-02-01T12:00:00.000Z':
                {'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin':0,\
                    'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas':1,\
                    'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax':2,\}}
                    .......
            LSOA_2: 
                {'2020-01-01T12:00:00.000Z':
                {'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin':0,\
                    'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas':1,\
                    'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax':2,\}
                {'2020-02-01T12:00:00.000Z':
                {'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin':0,\
                    'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas':1,\
                    'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax':2,\}}
                    .......
            }
    if it is a df, should have a structure like:
        LSOA_code   temp
    0   LSOA_1    {'2020-01-01T12:00:00.000Z':
                {'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin':0,\
                    'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas':1,\
                    'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax':2,\}
                    '2020-02-01T12:00:00.000Z':
                {'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin':0,\
                    'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas':1,\
                    'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax':2,\}
                    .......}
            
    1   LSOA_2    {'2020-01-01T12:00:00.000Z':
                {'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin':0,\
                    'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas':1,\
                    'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax':2,\}
                    '2020-02-01T12:00:00.000Z':
                {'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin':0,\
                    'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas':1,\
                    'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax':2,\}
                    .......}       

    or df can be this shape:
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
    if the input df is in first shape, it will firstly disaggrate the value into monthly distribution (like the second shape), 
    and then parse the result having the same shape as np.zeros((3, len(df), 12)). 
    Note that in this senario, the monthly data will be distributed into the Axis-2 as which have 12 positions, 
    data for each LSOA will be distributed into Axis-1 based on LSOA_index (and need to be provided in this case)
    however, the Axis-0 will have the same value, i.e. result_tensor[0][:][:] = result_tensor[1][:][:] = result_tensor[2][:][:]. 
    As this senario only apply to those data are not temperature related, making them into this shape is just for the sake of calculation.
    
    monthly_ref: ONLY NEED when input is a df and have only two column, and this monthly_ref is been used 
    to disaggregate into monthly data
    LSOA_index_id: ONLY NEED when input is a df, and are not-temperature related (such as consumption/usage)
    ** The logic is, for those data are not temperature related to perform this operation, they ultimately 
    are used to serve data that DO is temperature related, for example, convert electricity consumption into tensor,
    to serve delta_electricity_consumption_tensor, to get remaining_electricity_consumption_tensor. so they need
    to know which index is located in which LSOA area, so, this LSOA_index_id is needed for match up the date **
    '''
    t_dict = {'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmin':0,\
          'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tas':1,\
          'http://www.theworldavatar.com/kb/ontogasgrid/climate_abox/tasmax':2}
    date_dict = generate_time_dict('2020')
    if type(input) == pd.DataFrame:
        df = input
        if type(df.iloc[1,1]) == dict:
            # initialize a results_tensor= np.zeros((3, length of row,12))
            results_tensor = np.zeros((3, len(df), 12))
            # loop all the rows, and
            LSOA_index = {}
            for i, row in df.iterrows():
                # Make a dict called LSOA_index, using row index as key and data in the first column as value
                LSOA_index[row[0]] = i
            
            # The data in the second column is a dict contain 12 pairs, for each pair, the value are dicts as well.
            # The second level dict have 3 pairs, each key have it's value
            for i, data in df[df.columns[1]].items():
                # Parse the values in second column into results_tensor to fit in the shape
                for j, value in data.items():
                    for k, v in value.items():
                        t_index = t_dict[k]
                        date_index = date_dict[j]
                        results_tensor[t_index][i][date_index] = v
        else:
            # initialize a results_tensor= np.zeros((3, length of row,12))
            results_tensor = np.zeros((3, len(LSOA_index_id), 12))
            if df.shape[1] == 2:
                df = monthly_disaggregation(df, monthly_ref)
            # Select all rows and columns starting from the second column of the DataFrame
            values = df.iloc[:, 1:].values
            # Flatten the 2D array into a 1D array
            values = values.flatten()
            # Iterate over the indices and LSOA codes in the dictionary
            for i, (lsoa, index) in enumerate(LSOA_index_id.items()):
                # Use the at method to assign the values to the array
                results_tensor[:, index, :] = values[i * 12: (i + 1) * 12]
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

def remove_unlocated_data(df):
    # Create a boolean mask indicating which rows contain 'Unallocated' in the first column
    mask = df[df.columns[0]].str.contains('Unallocated')
    # Use the mask to index the DataFrame and drop the rows
    df = df[~mask]
    # Reset the indices of the DataFrame
    df = df.reset_index(drop=True)

    return df

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
