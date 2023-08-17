################################################
# Authors: Magnus Mueller (mm2692@cam.ac.uk)   #
# Date: 30 Nov 2022                            #
################################################

# The purpose of this file is to provide helper functions to query and update the KG

import numpy as np
import pandas as pd
from darts import TimeSeries
from darts import concatenate
from darts.dataprocessing.transformers import Scaler
from darts.utils.timeseries_generation import datetime_attribute_timeseries as dt_attr

from py4jps import agentlogging

from forecastingagent.datamodel.iris import *


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


# def check_cov_matches_rdf_type(row, rdf_type):
#     if 'type_with_measure' in row and row['type_with_measure'] == rdf_type or 'type_without_measure' in row and row['type_without_measure'] == rdf_type:
#         return True
#     else:
#         return False


def get_covs_heat_supply(kgClient, tsClient, covariates, lowerbound, upperbound, df=None):
    """
    NOTE: provide general load covariate function here with mapping in forecasting_config
    add note about complexity and need for revisiting, as order of covariates is important +
    day of week, etc. are not marked up but were used during training


    'load_covariates_func': The function which is used to load the covariates. If not provided, no covariates are loaded.
        Be aware, that the returned covariates must be available for the whole 'horizon'. 
        If the covariates are not long enough, Prophet is used, which does not require covariates.
        The function must return parameters:
        'covariates_iris': A list if iris, which are used.
        'covariates': A darts series object, which can be passed into model.predict()
        
        The function will receive the following parameters:
        'kgClient': The kgClient
        'tsClient': The tsClient
        'lowerbound': The lower bound of the time series data (can be None)
        'upperbound': The upper bound of the time series data (can be None)
    

    """
    cov_iris = []

    # NOTE The following approach just works for the data iris that have an unique rdf type.
    ts_by_type = kgClient.performQuery(get_ts_by_type())
    #get_time_series_details

    
    df_air_temp, df_public_holiday = None, None
    # NOTE: If multiple ts have the same air_temp and public_holiday rdf type, then the first one is used.
    for row in ts_by_type:
        if check_cov_matches_rdf_type(row, ONTOEMS_AIRTEMPERATURE) and df_air_temp is None:
            logger.info(f'Loading air temperature covariate')
            df_air_temp = get_df_of_ts(
                row['dataIRI'], tsClient, lowerbound=lowerbound, upperbound=upperbound)
            cov_iris.append(row['dataIRI'])

        if check_cov_matches_rdf_type(row, OHN_ISPUBLICHOLIDAY) and df_public_holiday is None:
            logger.info(f'Loading public holiday covariate')
            df_public_holiday = get_df_of_ts(
                row['dataIRI'], tsClient, lowerbound=lowerbound, upperbound=upperbound)
            cov_iris.append(row['dataIRI'])

    # create covariates list with time covariates for the forecast
    # Attention: be aware to have same order of covariates as during training
    covariates = concatenate(
        [
            get_data_cov(df_air_temp, "cov"),
            # use dates of other covariate to extract time covariates
            # if no other covariate is available, use df of orginal series
            # TODO:
            # in that case you need to extend the time range of the original series
            # in order to have enough data for the future time covariates (length of forecast horizon)
            get_time_cov(
                df_air_temp, {"dayofyear": True, "dayofweek": True, "hour": True}),
            get_data_cov(df_public_holiday, "cov"),
        ],
        axis="component",
    )
    logger.info(f'Created time covariates: "dayofyear", "dayofweek", "hour"')
    # add darts covariates as string
    #cov_iris += ['dayofyear', 'dayofweek', 'hour']
    return cov_iris, covariates


def get_df_of_ts(dataIRI, tsClient, lowerbound, upperbound, column_name="cov", index="time"):
    # Retrieve time series data and return as pandas dataframe with default column names
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


def get_data_cov(df, col):
    """
    It takes a dataframe and a column name as input, and returns a scaled time series

    :param df: the dataframe containing the data
    :param col: the column name of the dataframe that you want to use
    :return: A time series object
    """

    cov = TimeSeries.from_dataframe(df, time_col='Date', value_cols=col)
    scaler_cov = Scaler()
    cov_scaled = scaler_cov.fit_transform(cov)
    return cov_scaled


def get_time_cov(df, cov: dict):
    """
    It takes a df and returns a covariate matrix with the following columns:

    - day of year (cyclic)
    - day of week (cyclic)
    - hour of day (cyclic)

    The `cyclic` keyword argument is used to indicate that the covariate is cyclic. This is important
    for the model to learn the correct periodicity

    :param series: the time series to be modeled
    :return: A covariate matrix with the following columns:
        - day of year (cyclic)
        - day of week (cyclic)
        - hour of day (cyclic)
    """

    series = TimeSeries.from_dataframe(
        df, time_col='Date')  # , fill_missing_dates =True

    covs = concatenate(
        [
            dt_attr(series.time_index, k, dtype=np.float32, cyclic=v) for k, v in cov.items()

        ],
        axis="component",
    )
    return covs


def max_error(s1, s2):
    """
    It takes two time series, slices them to the intersection of their time ranges, 
    and then returns the maximum absolute difference between the two time series

    :param s1: the first time series
    :param s2: the original data
    :return: The maximum error between the two time series.
    """
    s2 = s2.slice_intersect(s1)
    s1 = s1.slice_intersect(s2)
    return np.max(np.abs(s1.values() - s2.values()))
