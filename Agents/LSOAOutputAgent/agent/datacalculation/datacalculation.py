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
from agent.dataretrieval.dataretrival import drop_column
from agent.utils.env_configs import YEAR

import numpy as np
import pandas as pd
import requests
import copy


# Initialise logger
logger = agentlogging.get_logger("prod")

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

def call_change_of_fuel_agent(url: str, uptake: float, gas_consumption: np.ndarray, propotion_of_heating: float = None, cop: np.ndarray = np.array([]), boiler_efficiency: float = None):
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

    if cop.any():
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
    change_of_electricity = np.array([])

    if cop.any():
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

def call_inequality_index_agent(url:str, df_change_of_cost_in:pd.DataFrame, df_fuel_poverty_in:pd.DataFrame, min_dc: float = None, max_dc: float = None,min_fp: float = None, max_fp: float = None):
    '''
    To calculate the inequality index, generate dataframe in JSON format, based on given change of fuel cost and fuel poverty proportion (GET request):
    (request expects all individual query parameter to be provided in a single nested JSON object with key 'query', see details below):
    Arguments:
    df_change_of_cost: two-column data frame, the first column (i.e. at position [0]) should be occupied with LSOA code as identifiers, respectively, the annual change of fuel cost data should be placed at the second column (i.e. at position [1]) using the unit of sterling pound

    df_fuel_poverty: two-column data frame, the first column (i.e. at position [0]) should be occupied with LSOA code as identifiers, respectively, the fuel poverty data should be placed at the second column (i.e. at position [1]). No unit for this index
    NOTE: column names don't matters as long as the position is correct
    Example input df:
    LSOA_code	     	Change of fuel cost
    0  	E01011954	     	1000
    1	E01011969	     	1000
    2	E01011970	     	1000

    min change of cost nth-percentile: input a float number(let's say 'n'), which means 'nth'-percentile of the distribution of the argument across all households, will be used as the min change of cost for normalisation (see equation above)
    If not provided, default value 1 will be used

    max change of cost nth-percentile: input a float number(let's say 'n'), which means 'nth'-percentile of the distribution of the argument across all households, will be used as the max change of cost for normalisation (see equation above)
    If not provided, default value 99 will be used

    min fuel poverty: input a float number, which will be used as the min fuel poverty for normalisation (see equation above)
    If not provided, default value 0 will be used

    max fuel poverty: input a float number, which will be used as the max fuel poverty for normalisation (see equation above)
    If not provided, default value 0.2 will be used
    '''
    # Make a copy so it won't ruin the original df
    df_change_of_cost = copy.deepcopy(df_change_of_cost_in)
    df_fuel_poverty = copy.deepcopy(df_fuel_poverty_in)

    # Wrapping the query
    query = {
        'query':{
                'df_change_of_cost':df_change_of_cost.to_dict(orient='list'),
                'df_fuel_poverty':df_fuel_poverty.to_dict(orient='list'),
                }
        }

    if min_dc:
        query['query']['min change of cost nth-percentile'] = float(min_dc)

    if max_dc:
        query['query']['max change of cost nth-percentile'] = float(max_dc)

    if min_fp:
        query['query']['min fuel poverty'] = float(min_fp)

    if max_fp:
        query['query']['max fuel poverty'] = float(max_fp)

    logger.info('Sending data to calculationagent_inequality_index...')
    
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
    
    df_inequality_index = pd.DataFrame.from_dict(data['df_inequality_index'])
    
    return df_inequality_index