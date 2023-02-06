################################################
# Authors: Jieyang Xu (jx309@cam.ac.uk) #
# Date: 30/11 2022                            #
################################################

# The purpose of this module is to perform data calculation by calling various calculation agent
# 
# NOTE: pd.DataFrame is used frequently in this module, where for all the dataframe
# without special notice, the column[0] should all be LSOA code used as identifier

import agentlogging
from agent.errorhandling.exceptions import *
from agent.dataretrieval.dataretrival import *
from agent.utils.env_configs import YEAR

import numpy as np
import pandas as pd
import requests
import copy


# Initialise logger
logger = agentlogging.get_logger("prod")

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
        df = drop_column(df,1)
    return df
    
# ----------------------Call Calculation Agent---------------------------------- #
def call_cop_agent(url: str, temp: np.ndarray, unit:str, t_h: float = None, hp_efficiency: float = None):
    '''
    This module will call the calculation agent(cop) to perform calculation with the parameters and url specified
    
    Arguments:
    url: the endpoint to perform calculation
    temperature: should be an array (np.ndarray) I suppose you are not interested in calculating single value...
    unit: unit with respect to temperature, must be wrapped as a url as per OntoMeasurement. Units below are available:
          DegreeCelsius ℃:    http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius
          DegreeFahrenheit ℉:    http://www.ontology-of-units-of-measure.org/resource/om-2/degreeFahrenheit
          Kelvin K:    http://www.ontology-of-units-of-measure.org/resource/om-2/kelvin
          t_h: hot side temperature (see equation above), if not provided, 318.15 will be used as default value.
          hp_efficiency: heat pump efficiency (see equation above), if not provided, 0.35 will be used as default value.
    '''
    
    # Wrapping the query
    query = {
        'query':{'temperature':temp.tolist(),
                'unit':unit
                }
        }
    
    if t_h:
        query['query']['t_h'] = t_h
    else:
        logger.info('Using default hot side temperature (318.15K) when calculating COP')

    if hp_efficiency:
        query['query']['hp_efficiency'] = hp_efficiency
    else:
        logger.info('Using default heat pump efficiency (0.35) when calculating COP')

    logger.info('Sending data to calculationagent_cop...')
    
    try:
        headers = {'Content-type': 'application/json'}
        response = requests.get(url, headers=headers, json=query)

    except Exception as ex:
        logger.error(f'Fail to connect to calculation agent (cop) -- Response code {response.status_code}')
        raise ConnectionError(f'Fail to connect to calculation agent (cop) -- Response code {response.status_code}') from ex

    try:
        data = response.json()

    except Exception as ex:
        logger.error(f'No valid JSON context from response -- Response code {response.status_code}')
        raise InvalidInput(f'No valid JSON context from response -- Response code {response.status_code}') from ex

    cop = np.array(data['COP'])
    logger.info('COP successfully calculated')

    return cop

def call_change_of_fuel_agent(url: str, uptake: float, gas_consumption: np.ndarray, propotion_of_heating: float = None, cop: np.ndarray = None, boiler_efficiency: float = None):
    '''
    This module will call the calculation agent(change of fuel) to perform calculation with the parameters and url specified
    
    Arguments:
    uptake: a index which quantify the extent of the deployment of heat pump by measureing the gas being replaced by using heat pump comparing to conventional fossil fule heating system.
            Defined as (change of gas / gas used for heating), or (change of gas / (total gas consumption * propotion of heating))
    gas consumption: should be an array (np.ndarray), 1D or 2D or 3D are all fine. value should be in unit of kWh
            The calculated change of gas array will have the exactly same shape as the gas consumption you provided. The input COP (if applicable) should have the exact same shape as the gas consumption you provided.
    propotion of heating: the propotion of gas which is used for heating(see equation above), if not provided, 0.9 will be used as default value.
    cop: Coefficient of Performance. should be an array (np.ndarray), and having the exactly same shape as the gas consumption you provided.
            The value will only be used when calculating change of electricity. Therefore if not provided, only the change of gas array will be returned. If provided, both change of gas and electricity arrays will be returned.
            Details about the COP can be found on builded python function to call this agent can be found on: home page of cop calculation agent, you may also use python function to call the cop calculation agent using available function call_cop_agent here
    boiler efficiency: boiler efficiency (see equation above), if not provided, 0.8 will be used as default value.
    '''
    
    # Wrapping the query
    query = {
        'query':{'uptake':uptake,
                'gas consumption':gas_consumption.tolist(),
                }
        }
    
    if propotion_of_heating:
        query['query']['propotion of heating'] = propotion_of_heating
    else:
        logger.info('Using default propotion of heating (0.9) when calculating change of gas consumption')

    if cop:
        query['query']['cop'] = cop.tolist()
    else:
        logger.info('No COP data provided, only change of gas consumption will be returned')

    if boiler_efficiency:
        query['query']['boiler efficiency'] = boiler_efficiency
    else:
        logger.info('Using default boiler efficiency (0.8) when calculating change of electricity consumption')

    logger.info('Sending data to calculationagent_change_in_fuel...')
    
    try:
        headers = {'Content-type': 'application/json'}
        response = requests.get(url, headers=headers, json=query)

    except Exception as ex:
        logger.error(f'Fail to connect to calculation agent (change_in_fuel) -- Response code {response.status_code}')
        raise ConnectionError(f'Fail to connect to calculation agent (change_in_fuel) -- Response code {response.status_code}') from ex

    try:
        data = response.json()

    except Exception as ex:
        logger.error(f'No valid JSON context from response -- Response code {response.status_code}')
        raise InvalidInput(f'No valid JSON context from response -- Response code {response.status_code}') from ex


    change_of_gas = np.array(data['change of gas'])
    logger.info('change_in_fuel successfully calculated')
    # Empty :)
    change_of_electricity = np.array()
    
    if cop:
        change_of_electricity = np.array(data['change of electricity'])
        logger.info('change_in_fuel successfully calculated')
    
    return change_of_gas, change_of_electricity

def call_fuel_cost_agent(url:str, df_elec_in:pd.DataFrame = pd.DataFrame(), df_gas_in:pd.DataFrame = pd.DataFrame(), year: str = YEAR, annual: bool = None):
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

    # Wrapping the query
    query = {
        'query':{
                'year':year
                }
        }

    if not df_elec_in.empty:
        # Make a copy so it won't ruin the original df
        df_elec = copy.deepcopy(df_elec_in)
        query['query']['df_electricity'] = df_elec.to_dict(orient='list')
    
    if not df_gas_in.empty:
        # Make a copy so it won't ruin the original df
        df_gas = copy.deepcopy(df_gas_in)
        query['query']['df_gas'] = df_gas.to_dict(orient='list')

    # At lease should have one input consumption
    if df_elec_in.empty and df_gas_in.empty:
        raise InvalidInput('None of gas consumption or electricity consumption provided.')
    
    if annual:
        query['query']['annual'] = str(annual)

        logger.info('Sending data to calculationagent_fuel_cost...')
    
    try:
        headers = {'Content-type': 'application/json'}
        response = requests.get(url, headers=headers, json=query)

    except Exception as ex:
        logger.error(f'Fail to connect to calculation agent (fuel cost) -- Response code {response.status_code}')
        raise ConnectionError(f'Fail to connect to calculation agent (fuel cost) -- Response code {response.status_code}') from ex

    try:
        data = response.json()

    except Exception as ex:
        logger.error(f'No valid JSON context from response -- Response code {response.status_code}')
        raise InvalidInput(f'No valid JSON context from response -- Response code {response.status_code}') from ex
    
    if not df_elec_in.empty and not df_gas_in.empty:
        df_cost_total = pd.DataFrame.from_dict(data['df_cost_total'])
    else:
        # Empty :)
        df_cost_total= pd.DataFrame()
    
    if not df_elec_in.empty:
        df_cost_elec = pd.DataFrame.from_dict(data['df_cost_elec'])
    else:
        # Empty :)
        df_cost_elec= pd.DataFrame()

    if not df_gas_in.empty:
        df_cost_gas = pd.DataFrame.from_dict(data['df_cost_gas'])
    else:
        # Empty :)
        df_cost_gas= pd.DataFrame()
    
    return df_cost_total, df_cost_elec, df_cost_gas

def call_fuel_emission_agent(url:str, df_elec_in:pd.DataFrame = pd.DataFrame(), df_gas_in:pd.DataFrame = pd.DataFrame(), year: str = YEAR, annual: bool = None):
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

    # Wrapping the query
    query = {
        'query':{
                'year':year
                }
        }

    if not df_elec_in.empty:
        # Make a copy so it won't ruin the original df
        df_elec = copy.deepcopy(df_elec_in)
        query['query']['df_electricity'] = df_elec.to_dict(orient='list')
    
    if not df_gas_in.empty:
        # Make a copy so it won't ruin the original df
        df_gas = copy.deepcopy(df_gas_in)
        query['query']['df_gas'] = df_gas.to_dict(orient='list')

    # At lease should have one input consumption
    if df_elec_in.empty and df_gas_in.empty:
        raise InvalidInput('None of gas consumption or electricity consumption provided.')
    
    if annual:
        query['query']['annual'] = str(annual)

        logger.info('Sending data to calculationagent_fuel_emission...')
    
    try:
        headers = {'Content-type': 'application/json'}
        response = requests.get(url, headers=headers, json=query)

    except Exception as ex:
        logger.error(f'Fail to connect to calculation agent (fuel emission) -- Response code {response.status_code}')
        raise ConnectionError(f'Fail to connect to calculation agent (fuel emission) -- Response code {response.status_code}') from ex

    try:
        data = response.json()

    except Exception as ex:
        logger.error(f'No valid JSON context from response -- Response code {response.status_code}')
        raise InvalidInput(f'No valid JSON context from response -- Response code {response.status_code}') from ex
    
    if not df_elec_in.empty and not df_gas_in.empty:
        df_emission_total = pd.DataFrame.from_dict(data['df_emission_total'])
    else:
        # Empty :)
        df_emission_total= pd.DataFrame()
    
    if not df_elec_in.empty:
        df_emission_elec = pd.DataFrame.from_dict(data['df_emission_elec'])
    else:
        # Empty :)
        df_emission_elec= pd.DataFrame()

    if not df_gas_in.empty:
        df_emission_gas = pd.DataFrame.from_dict(data['df_emission_gas'])
    else:
        # Empty :)
        df_emission_gas= pd.DataFrame()
    
    return df_emission_total, df_emission_elec, df_emission_gas