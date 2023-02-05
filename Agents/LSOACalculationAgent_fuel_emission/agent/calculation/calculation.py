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
    such as emission, emission, you can calculate a annual one and disaggregate into monthly data
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

def calculating_fuel_emission(df_elec_in:pd.DataFrame, df_gas_in:pd.DataFrame, year: str, annual: bool = False):
    '''
    To calculate the fuel emission per LSOA, normally on per household basis
    Returns three dataframe which have first column as LSOA code, and the following 12
    columns for each month of df_emission_total, df_emission_elec, df_emission_gas
    Arguments:
    df_elec: two-column data frame which MUST have the electricity data placed at the second column
    (i.e. at position [1])
    df_gas: two-column data frame which MUST have the gas data placed at the second column
    (i.e. at position [1])
    year: choose the year for index: prices of gas & elec, monthly distribution references of gas & elec
    annual: if True, the second column will include the annual value (with monthly data)
            if False, only monthly value will be returned
    '''
    
    df_emission_elec = calculating_single_emission_elec(df_elec_in, year, annual)
    df_emission_gas = calculating_single_emission_gas(df_gas_in, year, annual)
    
    # Merge to total emission
    df_emission_total = df_emission_elec.merge(df_emission_gas, left_on=df_emission_elec.columns[0], right_on=df_emission_gas.columns[0], how='inner')
    
    # Iterate through the columns of the merged dataframe and add the values
    for col in df_emission_gas.columns[1:]:
        df_emission_total[col] = df_emission_total[col + '_x'] + df_emission_total[col + '_y']

    # Drop the original columns from the merged dataframe
    df_emission_total.drop(columns=[col + '_x' for col in df_emission_elec.columns[1:]], inplace=True)
    df_emission_total.drop(columns=[col + '_y' for col in df_emission_gas.columns[1:]], inplace=True)
    
    return df_emission_total, df_emission_elec, df_emission_gas

def calculating_single_emission_gas(df_in:pd.DataFrame,year:str,annual: bool = False):
    '''
    Calculate gas emission based on gas consumption
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
    logger.info(f'Reading indexs for the year {year} to calculate fuel emission...')
    carbon_index_gas = read_from_web_carbon_index('Gas', year)
    monthly_ref_gas = read_from_web_monthly_distribution_gas(year)
    df_gas = copy.deepcopy(df_in)

    df_gas[df_gas.columns[1]] *= carbon_index_gas
    df_gas = df_gas.rename(columns={df_gas.columns[1]: 'Annual emission'}, inplace=False)
    df_emission_gas = df_gas.copy()

    # Disaggrate into monthly dataframe
    df_emission_gas = monthly_disaggregation(df_emission_gas,monthly_ref_gas, annual)
    return df_emission_gas

def calculating_single_emission_elec(df_in:pd.DataFrame,year:str,annual: bool = False):
    '''
    Calculate electricity emission based on electricity consumption
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
    logger.info(f'Reading indexs for the year {year} to calculate fuel emission...')
    emission_elec = read_from_web_carbon_index('Electricity',year)
    monthly_ref_elec = read_from_web_monthly_distribution_elec(year)
    df_elec = copy.deepcopy(df_in)

    # Replace the first column from consumption into emission
    df_elec[df_elec.columns[1]] *= emission_elec
    df_elec = df_elec.rename(columns={df_elec.columns[1]: 'Annual emission'}, inplace=False)
    df_emission_elec = df_elec.copy()

    # Disaggrate into monthly dataframe
    df_emission_elec = monthly_disaggregation(df_emission_elec, monthly_ref_elec, annual)
    
    return df_emission_elec

