################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################


import numpy as np
import copy
import pandas as pd

def calculate_inequality_index(row, min_fp, max_fp, min_dc, max_dc):
    poverty_values = row[2]
    change_values = row[1]
    a = ((poverty_values-min_fp)/(max_fp-min_fp))
    b = ((2*(change_values-min_dc))/(max_dc-min_dc))-1
    inequality_index = a*b
    
    return np.around(inequality_index,decimals=3)

def calculate_df(df_change_of_cost_in: pd.DataFrame, df_fuel_poverty_in: pd.DataFrame, min_deltaC_nth_percentile:float = 1, \
                                 max_deltaC_nth_percentile:float = 99, min_fuel_poverty:float = 0, max_fuel_poverty:float = 0.2 ):
    '''
    '''
    # make copy of df_in
    df_change_of_cost = copy.deepcopy(df_change_of_cost_in)
    df_fuel_poverty = copy.deepcopy(df_fuel_poverty_in)
    
    # Merge the data into one df
    df_fuel_poverty.rename(columns={df_fuel_poverty.columns[0]: 'LSOA_code'}, inplace=True)
    df_change_of_cost.rename(columns={df_change_of_cost.columns[0]: 'LSOA_code'}, inplace=True)
    df_all = df_change_of_cost.merge(df_fuel_poverty, left_on=df_change_of_cost.columns[0], right_on=df_fuel_poverty.columns[0], how='inner')
    
    change_values = df_all[df_all.columns[1]].values
    min_fp = min_fuel_poverty * 100
    max_fp = max_fuel_poverty * 100
    min_dc = np.nanpercentile(change_values,min_deltaC_nth_percentile)
    max_dc = np.nanpercentile(change_values,max_deltaC_nth_percentile)
    df_all['inequality_index'] = df_all.apply(calculate_inequality_index, axis=1, args=(min_fp, max_fp, min_dc, max_dc))
    df_all.drop(columns=[df_all.columns[1], df_all.columns[2]], inplace=True)

    return df_all
