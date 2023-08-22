################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #    
# Date: 21 Aug 2023                            #
################################################

# The purpose of this module is to provide the actual estimation logic to 
# calculate emissions from consumed gas and provided heat

import pandas as pd

from py4jps import agentlogging

from emissionagent.datamodel.iris import *


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


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
    except IndexError:
        msg = f'Derivation {derivationIRI}: SimulationTime unix value {unix_time} not found in time series data. '
        msg += f'Time series data unix range: [ {df["unix"].min()} , {df["unix"].max()} ]'
        logger.error(msg)
        raise ValueError(msg)
    
    return amount


def calculate_emissions_for_consumed_gas(pollutant_iri:str, consumed_gas:float) -> dict:
    """
    Calculates the emissions for consumed gas

    Arguments:
        pollutant_iri {str} -- IRI of pollutant ID for which to calculate emissions
        consumed_gas {float} -- amount of consumed gas, MWh
    Returns:
        emission {dict} -- emission data to be instantiated as derivation output
    """
    
    # Initialise return dictionary
    emission = {
        'pollutantID': pollutant_iri,
        'temperature': 10.0,
        'density': 10.0,
        'massflow': 10.0
    }
    
    # TODO: Implement emission calculation

    return emission


def calculate_emissions_for_provided_heat(pollutant_iri:str, provided_heat:float) -> dict:
    """
    Calculates the emissions for consumed gas

    Arguments:
        pollutant_iri {str} -- IRI of pollutant ID for which to calculate emissions
        provided_heat {float} -- amount of sourced heat, MWh
    Returns:
        emission {dict} -- emission data to be instantiated as derivation output
    """
    
    # Initialise return dictionary
    emission = {
        'pollutantID': pollutant_iri,
        'temperature': 10.0,
        'density': 10.0,
        'massflow': 10.0
    }
    
    # TODO: Implement emission calculation

    return emission
