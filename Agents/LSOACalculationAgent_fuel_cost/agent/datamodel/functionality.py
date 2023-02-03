from agent.errorhandling.exceptions import *

import numpy as np

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

def drop_column(df,index):
    '''
    drop a column using index, can using index number or name
    '''
    if type(index) == int:
        df = df.drop(columns=df.columns[index])
    else:
        df = df.drop(columns=[index])
    return df
