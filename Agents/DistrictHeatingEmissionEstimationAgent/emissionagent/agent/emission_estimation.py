################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 21 Aug 2023                            #
################################################

# The purpose of this module is to provide the actual estimation logic to 
# calculate emissions from consumed gas and provided heat

import pandas as pd
from datetime import datetime
import CoolProp.CoolProp as CP

from py4jps import agentlogging

from emissionagent.datamodel.iris import *


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


#
# ESTIMATION PARAMETERS
#

# Fluid properties
FLUID = 'Air'           # Assume exhaust gases as hot air
PRESSURE = 101325.0     # Assume atmospheric pressure of 1atm (in Pa)

# Emission factors (kg/MWh)
EFW_FACTORS = {
    # in kg/MWh heat sourced
    # NOTE: NO2 is currently not supported by Aermod; hence, NOx is used instead
    #OD_NO2: 4.01351, 
    OD_NOX: 4.01351, 
    OD_PM10: 0.13260,
    OD_PM2_5: 0.11883
}
DH_FACTORS = {
    # in kg/MWh (lower calorific value) gas burned
    # NOTE: NO2 is currently not supported by Aermod; hence, NOx is used instead
    #OD_NO2: 0.22429, 
    OD_NOX: 0.22429, 
    OD_PM10: 0.00753,
    OD_PM2_5: 0.00605 
}

# Default rounding behaviour
ROUNDING = 6
# Time format (for logging/console output)
TIME_FORMAT = "%Y-%m-%dT%H:%M:%SZ"

#
# ESTIMATION METHODS
#

def extract_relevant_gas_or_heat_amount(ts_client, kg_client, derivationIRI:str,
                                        dataIRIs:list, simulation_time_iri:str) -> float:
    """
    Extracts the consolidated consumed gas or provided heat amount from the
    time series data for the specified simulation time.

    Arguments:
        ts_client {TSClient} -- TimeSeriesClient instance
        kg_client {KGClient} -- SPARQL Client instance
        derivationIRI {str} -- IRI of derivation instance
        dataIRIs {list} -- list of dataIRIs for which to extract time series values
        simulation_time_iri {str} -- IRI of SimulationTime for which to extract 
                                     time series values
    Returns:
        amount {float} -- consolidated amount of consumed gas or provided heat
                          for specified time stamp
    """

    dfs = []
    for dataIRI in dataIRIs:
        times, values = ts_client.retrieve_timeseries(dataIRI=dataIRI)
        # Create DataFrame with DateTimeIndex per dataIRI and append to list
        df = pd.DataFrame({'time': times, dataIRI: values})
        df['time'] = pd.to_datetime(df['time'])
        df.set_index('time', inplace=True)
        dfs.append(df)
    # Concatenate all individual DataFrames based on index and sum values
    df = pd.concat(dfs, axis=1)
    df['sum'] = df.sum(axis=1)
    # Create column with unix time stamps (in seconds, not nanoseconds)
    df.reset_index(inplace=True)
    df['unix'] = df['time'].apply(lambda x: x.timestamp())
    df['unix'] = df['unix'].astype(int)

    # Extract time series value for SimulationTime
    unix_time = kg_client.get_unix_timestamps(simulation_time_iri)
    try:        
        amount = df[df['unix'] == unix_time]['sum'].values[0]
        # Provide some console output
        t = datetime.utcfromtimestamp(unix_time).strftime(TIME_FORMAT)
        print(f'Emission estimation input values for current simulation time: {t}')
        df_time_step = df[df['unix'] == unix_time]
        for col in df_time_step:
            print(f"{col}: {df_time_step[col].values}")
    except IndexError:
        msg = f'Derivation {derivationIRI}: SimulationTime unix value {unix_time} not found in time series data. '
        msg += f'Time series data unix range: [ {df["unix"].min()} , {df["unix"].max()} ]'
        logger.error(msg)
        raise ValueError(msg)
    
    return amount


def calculate_emissions_for_consumed_gas(pollutant_iri:str, consumed_gas:float) -> dict:
    """
    Calculates the emissions associated with a certain amount of burned gas
    NOTE: Assumes that the amount of consumed gas refers to MWh/h (in line
          with the overall district heating optimisation based on hourly data)

    Arguments:
        pollutant_iri {str} -- IRI of pollutant ID for which to calculate emissions
        consumed_gas {float} -- amount of consumed gas (expressed with regards
                                to lower calorific value), MWh
    Returns:
        emission {dict} -- emission data to be instantiated as derivation output
        NOTE: All returned values are in SI units i.e.,
                temperature - K, 
                density - kg/m3, 
                mass flow rate - kg/s
    """
    
    # Temperature of flue gas: 200 degC (assumption due to use of heat recovery,
    #  in line with typical flue gas value by IPCC)
    temp = 200.0 + 273.15    # Kelvin
    # Assume: density of exhaust stream = density of hot air
    rho = CP.PropsSI('D', 'P', PRESSURE, 'T', temp, FLUID)

    # Calculate emission mass flow rate 
    # NOTE: this mass flow represents the pure pollutant mass flow, not the
    #       total mass flow of the exhaust stream
    mass_flow = DH_FACTORS[pollutant_iri] * consumed_gas    # kg/MWh * MWh = kg
    # Assume: equal mass flow throughout entire hour
    mass_flow /= 3600.0                                     # kg/s

    # Populate return dictionary
    emission = {
        'pollutantID': pollutant_iri,
        'temperature': temp,
        'density': round(rho, 6),
        'massflow': round(mass_flow, 6)
    }
    logger.info(f'Estimated gas burning emissions: {emission}')

    return emission


def calculate_emissions_for_provided_heat(pollutant_iri:str, provided_heat:float) -> dict:
    """
    Calculates the emissions associated with a certain amount of sourced heat
    NOTE: Assumes that the amount of provided heat refers to MWh/h (in line
          with the overall district heating optimisation based on hourly data)

    Arguments:
        pollutant_iri {str} -- IRI of pollutant ID for which to calculate emissions
        provided_heat {float} -- amount of sourced heat, MWh
    Returns:
        emission {dict} -- emission data to be instantiated as derivation output
        NOTE: All returned values are in SI units i.e.,
                temperature - K, 
                density - kg/m3, 
                mass flow rate - kg/s
    """
    
    # Temperature of flue gas: 220 degC based on EfW plant operator
    temp = 220.0 + 273.15    # Kelvin
    # Assume: density of exhaust stream = density of hot air
    rho = CP.PropsSI('D', 'P', PRESSURE, 'T', temp, FLUID)

    # Calculate emission mass flow rate 
    # NOTE: this mass flow represents the pure pollutant mass flow, not the
    #       total mass flow of the exhaust stream
    mass_flow = EFW_FACTORS[pollutant_iri] * provided_heat  # kg/MWh * MWh = kg
    # Assume: equal mass flow throughout entire hour
    mass_flow /= 3600.0                                     # kg/s

    # Populate return dictionary
    emission = {
        'pollutantID': pollutant_iri,
        'temperature': temp,
        'density': round(rho, 6),
        'massflow': round(mass_flow, 6)
    }
    logger.info(f'Estimated EfW emissions: {emission}')

    return emission
