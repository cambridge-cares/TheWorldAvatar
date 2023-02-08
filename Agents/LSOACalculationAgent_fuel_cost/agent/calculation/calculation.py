################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to define functions to calculate COP based on Temperature
# And another way around.

import numpy as np
import copy
import pandas as pd

from agent.dataretrieval.dataretrival import *

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
    annual: if True, the second column will include the annual value (with monthly data)
            if False, only monthly value will be returned
    '''
    global months
    df = copy.deepcopy(df_in)
    months = ['January','February','March','April','May','June','July','August','September','October','November','December']
    total = sum(monthly_ref)
    for i in range(12):
        df[f'{months[i]}'] = df[df.columns[1]] * monthly_ref[i] / total
    if annual == False:
        df = drop_column(df,1)
    return df

def calculating_fuel_cost(df_elec_in:pd.DataFrame, df_gas_in:pd.DataFrame, year: str, annual: bool = False):
    '''
    To calculate the fuel cost per LSOA, normally on per household basis
    Returns three dataframe which have first column as LSOA code, and the following 12
    columns for each month of df_cost_total, df_cost_elec, df_cost_gas
    Arguments:
    df_elec: two-column data frame which MUST have the electricity data placed at the second column
    (i.e. at position [1])
    df_gas: two-column data frame which MUST have the gas data placed at the second column
    (i.e. at position [1])
    year: choose the year for index: prices of gas & elec, monthly distribution references of gas & elec
    annual: if True, the second column will include the annual value (with monthly data)
            if False, only monthly value will be returned
    '''
    
    df_cost_elec = calculating_single_cost_elec(df_elec_in, year, annual)
    df_cost_gas = calculating_single_cost_gas(df_gas_in, year, annual)
    
    # Merge to total cost
    df_cost_gas.rename(columns={df_cost_gas.columns[0]: 'LSOA_code'}, inplace=True)
    df_cost_elec.rename(columns={df_cost_elec.columns[0]: 'LSOA_code'}, inplace=True)
    df_cost_total = df_cost_elec.merge(df_cost_gas, left_on=df_cost_elec.columns[0], right_on=df_cost_gas.columns[0], how='inner')
    
    # Iterate through the columns of the merged dataframe and add the values
    for col in df_cost_gas.columns[1:]:
        df_cost_total[col] = df_cost_total[col + '_x'] + df_cost_total[col + '_y']

    # Drop the original columns from the merged dataframe
    df_cost_total.drop(columns=[col + '_x' for col in df_cost_elec.columns[1:]], inplace=True)
    df_cost_total.drop(columns=[col + '_y' for col in df_cost_gas.columns[1:]], inplace=True)
    
    return df_cost_total, df_cost_elec, df_cost_gas

def calculating_single_cost_gas(df_in:pd.DataFrame,year:str,annual: bool = False):
    '''
    Calculate gas cost based on gas consumption
    Returns adataframe which have first column as LSOA code, and the following 12
    columns for each month 
    Arguments:
    df_in: two-column data frame which MUST have the gas data placed at the second column
    (i.e. at position [1])
    year: choose the year for index: prices of gas & elec, monthly distribution references of gas & elec
    annual: if True, the second column will include the annual value (with monthly data)
            if False, only monthly value will be returned
    '''
    # Get index
    logger.info(f'Reading indexs for the year {year} to calculate fuel cost...')
    price_gas = read_from_web_price_gas(year)
    monthly_ref_gas = read_from_web_monthly_distribution_gas(year)
    df_gas = copy.deepcopy(df_in)

    df_gas[df_gas.columns[1]] *= price_gas
    df_gas = df_gas.rename(columns={df_gas.columns[1]: 'Annual cost'}, inplace=False)
    df_cost_gas = df_gas.copy()

    # Disaggrate into monthly dataframe
    df_cost_gas = monthly_disaggregation(df_cost_gas,monthly_ref_gas, annual)
    return df_cost_gas

def calculating_single_cost_elec(df_in:pd.DataFrame,year:str,annual: bool = False):
    '''
    Calculate electricity cost based on electricity consumption
    Returns adataframe which have first column as LSOA code, and the following 12
    columns for each month 
    Arguments:
    df_in: two-column data frame which MUST have the electricity data placed at the second column
    (i.e. at position [1])
    year: choose the year for index: prices of gas & elec, monthly distribution references of gas & elec
    annual: if True, the second column will include the annual value (with monthly data)
            if False, only monthly value will be returned
    '''
    # Get index
    logger.info(f'Reading indexs for the year {year} to calculate fuel cost...')
    price_elec = read_from_web_price_elec(year)
    monthly_ref_elec = read_from_web_monthly_distribution_elec(year)
    df_elec = copy.deepcopy(df_in)

    # Replace the first column from consumption into cost
    df_elec[df_elec.columns[1]] *= price_elec
    df_elec = df_elec.rename(columns={df_elec.columns[1]: 'Annual cost'}, inplace=False)
    df_cost_elec = df_elec.copy()

    # Disaggrate into monthly dataframe
    df_cost_elec = monthly_disaggregation(df_cost_elec, monthly_ref_elec, annual)
    
    return df_cost_elec

