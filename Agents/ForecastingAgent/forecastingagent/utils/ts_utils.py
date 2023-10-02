################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk) #
#          Magnus Mueller (mm2692@cam.ac.uk)   #
# Date: 17 Aug 2023                            #
################################################

# This module provides multiple helper functions to handle time series data and
# condition it as required by Darts

import numpy as np
import pandas as pd
from darts import TimeSeries
from darts import concatenate
from darts.dataprocessing.transformers import Scaler
from darts.utils.timeseries_generation import datetime_attribute_timeseries as dt_attr

from py4jps import agentlogging

from forecastingagent.datamodel.iris import *
from forecastingagent.agent.forcasting_config import *


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


def get_df_of_ts(dataIRI, tsClient, lowerbound, upperbound, column_name="cov", index="time"):
    """
    Retrieve time series data for provided dataIRI between given bounds and 
    returns as pandas dataframe with default column names
    """
    times, values = tsClient.retrieve_timeseries(dataIRI, lowerbound, upperbound)
    if len(values) == 0:
        msg = f'No time series data available for dataIRI "{dataIRI}" between {lowerbound} and {upperbound}.'
        logger.error(msg)
        raise ValueError(msg)
    logger.info(f'Loaded {len(values)} values for dataIRI "{dataIRI}" from "{lowerbound}" to "{upperbound}"')

    df = pd.DataFrame(zip(values, times), columns=[column_name, index])
    
    # Remove time zone and convert to datetime
    df[index] = pd.to_datetime(df[index]).dt.tz_convert('UTC').dt.tz_localize(None)
    
    return df


def scale_covariate(df, column_name):
    """
    Method scales time series data for the provided column based on the entire
    data history provided, i.e., scales data based on min/max values of data
    in provided column

    Arguments:
        df {pd.DataFrame} -- the dataframe containing the data
        col {str} -- the column name of the dataframe to extract
    Returns
        A darts.TimeSeries object of the scaled time series
    """

    cov = TimeSeries.from_dataframe(df, time_col='time', value_cols=column_name)
    scaler_cov = Scaler()
    cov_scaled = scaler_cov.fit_transform(cov)

    return cov_scaled


def get_time_covariates(df, cov: dict):
    """
    Method creates a date/time-related covariates based on the date/time index 
    of a provided DataFrame, i.e., 
        - day of year (cyclic)
        - day of week (cyclic)
        - hour of day (cyclic)

    The `cyclic` keyword argument is used to indicate that the covariate is cyclic.
    This is important for the model to learn the correct periodicity.

    Arguments:
        df {DataFrame} -- DataFrame with relevant time index (e.g., any other 
                          covariate or actual time series)
        cov {dict} -- dictionary of time covariates to include
    Returns:
        A darts.TiomeSeries object with the following time series
        - day of year (cyclic)
        - day of week (cyclic)
        - hour of day (cyclic)
    """

    series = TimeSeries.from_dataframe(df, time_col='time')

    covs = concatenate(
        [
            dt_attr(series.time_index, k, dtype=np.float32, cyclic=v) for k, v in cov.items()

        ],
        axis="component",
    )

    return covs


def check_if_enough_covs_exist(cfg, covariates):
    if covariates is not None and \
       (cfg['fc_start_timestamp'] + cfg['frequency'] * (cfg['horizon'] - 1) > covariates.end_time()):
        msg = f'Not enough covariates for complete future horizon. '
        msg += f'Covariates end at {covariates.end_time()} but forecast horizon ends at {cfg["fc_start_timestamp"] + cfg["frequency"] * (cfg["horizon"] - 1)}'
        logger.error(msg)
        raise ValueError(msg)
    return True


def get_ts_lower_upper_bound(cfg, time_format=TIME_FORMAT):
    """
    It takes the forecast start date, the frequency, the horizon, and the data length, 
    and returns the lower and upper bounds of the time series

    Arguments:
        cfg: a dictionary with the following keys:
            'fc_start_timestamp': the start of the forecast (pd.Timestamp)
            'frequency': the frequency of the time series data (dt.timedelta)
            'horizon': the length of the forecast, i.e. number of time steps (int)
            'data_length': the length of the time series data to retrieve before the
                           fc_start_timestamp, i.e. number of time steps (int)

    Returns:
        Lower and upper bounds of the time series as strings
    """

    # upper bound is fc_start_timestamp + forecast horizon
    upperbound = cfg['fc_start_timestamp'] + \
                 cfg['frequency'] * (cfg['horizon'] - 1)

    # lower bound is fc_start_timestamp - (historical) data_length
    lowerbound = cfg['fc_start_timestamp'] - \
        cfg['frequency'] * (cfg['data_length'])
    
    return lowerbound.strftime(time_format), upperbound.strftime(time_format)


def max_error(s1, s2):
    """
    The method takes two time series, slices them to the intersection of their
    time ranges, and then returns the maximum absolute difference between them

    Arguments:
        s1 {TimeSeries} -- the first time series
        s2 {TimeSeries} -- the original data
    Returns:
        The maximum error between the two time series
    """
    s2 = s2.slice_intersect(s1)
    s1 = s1.slice_intersect(s2)
    return np.max(np.abs(s1.values() - s2.values()))
